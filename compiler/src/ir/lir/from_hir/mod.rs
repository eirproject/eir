use ::ir::{ Module, FunctionDefinition };
use ::ir::hir;
use ::ir::lir;
use ::ir::lir::Source;
use ::ir::hir::scope_tracker::ScopeTracker;
use ::ir::SSAVariable;
use ::ir::lir::cfg::LabelN;
use ::intern::RAISE as ATOM_RAISE;

mod exception_handler_stack;
use self::exception_handler_stack::ExceptionHandlerStack;

pub fn do_lower(module: &mut Module, env: &mut ScopeTracker) {
    module.lower(env)
}

impl Module {
    fn lower(&mut self, env: &mut ScopeTracker) {
        for fun in &mut self.functions {
            println!("{}", fun.ident);
            fun.lower(env);
        }
    }
}

impl FunctionDefinition {
    fn lower(&mut self, env: &mut ScopeTracker) {
        let mut cfg = lir::FunctionCfg::new();

        {
            let mut builder = lir::cfg::FunctionCfgBuilder::new(&mut cfg);
            let entry = builder.get_entry();

            builder.op_arguments(
                entry,
                self.hir_fun.args.iter().map(|a| a.ssa).collect()
            );

            if self.ident.lambda.is_some() {
                let lambda_env = env.get_lambda_env(self.ident.lambda.unwrap());
                for (_scope_def, _outer, inner) in lambda_env.captures.iter() {
                    // TODO: Should extract from env
                    builder.op_move(entry, ::parser::AtomicLiteral::Nil, *inner);
                }
                for (fun, ssa) in lambda_env.meta_binds.iter() {
                    builder.basic_op(
                        entry,
                        lir::OpKind::BindClosure {
                            ident: fun.clone(),
                        },
                        // TODO: Should be env
                        vec![Source::Constant(::parser::AtomicLiteral::Nil)],
                        vec![*ssa]
                    );
                }
            }

            let exc_block = builder.add_block();
            let exc_ssa = env.new_ssa();
            builder.ensure_phi(exc_block, exc_ssa);

            let mut exc_stack = ExceptionHandlerStack::new(exc_block, exc_ssa);
            let (block, ssa) = self.hir_fun.body.lower(&mut builder, env, &mut exc_stack,
                                              entry);
            exc_stack.finish();

            builder.op_return_ok(block, lir::Source::Variable(ssa));
            builder.op_return_throw(exc_block, lir::Source::Variable(exc_ssa));
        }

        self.lir_function = Some(cfg);
    }
}

macro_rules! lower_chain {
    ($inner:expr, $builder:expr, $env:expr, $exc_stack:expr, $chain:expr) => {
        {
            let (label, ssa) = $inner.lower($builder, $env, $exc_stack, $chain);
            $chain = label;
            ssa
        }
    }
}

struct CaseStructureDef<'a> {
    entry_block: LabelN,
    match_val: SSAVariable,
    clauses: Vec<CaseStructureClauseDef<'a>>,
    values: Vec<SSAVariable>,
    return_ssa: SSAVariable,
    match_fail: Option<LabelN>,
}
struct CaseStructureClauseDef<'a> {
    patterns: Vec<::ir::hir::Pattern>,
    guard: Box<Fn(&mut lir::cfg::FunctionCfgBuilder, &mut ScopeTracker,
                      &mut ExceptionHandlerStack, LabelN, &[SSAVariable])
                     -> (LabelN, SSAVariable) + 'a>,
    body: Box<Fn(&mut lir::cfg::FunctionCfgBuilder, &mut ScopeTracker,
                     &mut ExceptionHandlerStack, LabelN, &[SSAVariable])
                    -> (LabelN, SSAVariable) + 'a>,
}

fn case_structure(
    b: &mut lir::cfg::FunctionCfgBuilder,
    env: &mut ScopeTracker,
    exc_stack: &mut ExceptionHandlerStack,
    def: &CaseStructureDef,
) -> (LabelN, SSAVariable) {
    let main_cont = def.entry_block;

    // The central block of the Case structure. Contains only one
    // operation, the OpKind::Case.
    let match_body_label = b.add_block();

    // The block that control flow converges to after the match
    // structure is complete. No ops are inserted in this block
    // here, except a PHI node.
    let done_label = b.add_block();


    // The SSA variable representing the case structure.
    let case_structure_ssa = env.new_ssa();

    let guard_exception_label = b.add_block();
    let guard_exception_ssa = env.new_ssa();
    b.ensure_phi(guard_exception_label, guard_exception_ssa);

    // === Fail leaf ===
    let mut leaves = if let Some(match_fail) = def.match_fail {
        vec![match_fail]
    } else {
        let case_fail_label = b.add_block();
        let fail_ssa = env.new_ssa();
        b.basic_op(case_fail_label,
                   lir::OpKind::Move,
                   vec![lir::Source::Constant(::parser::AtomicLiteral::Nil)],
                   vec![fail_ssa]);
        b.basic_op(case_fail_label, lir::OpKind::Jump, vec![], vec![]);
        exc_stack.add_error_jump(
            b, case_fail_label, fail_ssa);
        vec![case_fail_label]
    };

    // === Match leaves
    for (idx, clause) in def.clauses.iter().enumerate() {
        let orig_clause_label = b.add_block();
        let mut clause_label = orig_clause_label;

        let case_values_ssa: Vec<_> = clause.patterns.iter()
            .flat_map(|pattern| {
                pattern.binds.iter()
                    .map(|binding| binding.1)
            }).collect();

        b.basic_op(clause_label,
                   lir::OpKind::CaseValues,
                   vec![lir::Source::Variable(case_structure_ssa)],
                   case_values_ssa.clone(),
        );
        exc_stack.push_catch(guard_exception_label, guard_exception_ssa);
        let (clause_label, guard_ret) = (clause.guard)(
            b, env, exc_stack, clause_label, &case_values_ssa);
        exc_stack.pop_catch();
        b.basic_op(clause_label,
                   lir::OpKind::IfTruthy,
                   vec![lir::Source::Variable(guard_ret)],
                   vec![]
        );
        let guard_fail_label = b.add_block();
        let mut leaf_body_label = b.add_block();
        b.add_jump(clause_label, leaf_body_label);
        b.add_jump(clause_label, guard_fail_label);
        b.finish(clause_label);

        // Guard fail
        b.basic_op(guard_fail_label,
                   lir::OpKind::CaseGuardFail { clause_num: idx },
                   vec![lir::Source::Variable(case_structure_ssa)],
                   vec![]);
        b.add_jump(guard_fail_label, match_body_label);
        b.finish(guard_fail_label);

        // Guard ok
        b.basic_op(leaf_body_label,
                   lir::OpKind::CaseGuardOk,
                   vec![lir::Source::Variable(case_structure_ssa)],
                   vec![]);
        b.op_tombstone(leaf_body_label, case_structure_ssa);

        let (ret_label, clause_ret_ssa) = (clause.body)(
            b, env, exc_stack, leaf_body_label, &case_values_ssa);
        b.op_jump(ret_label, done_label);
        b.add_phi(ret_label, clause_ret_ssa,
                  done_label, def.return_ssa);
        b.finish(ret_label);

        leaves.push(orig_clause_label);
    }

    // === Match body ===
    let clauses: Vec<_> = def.clauses.iter().map(|c| {
        lir::Clause {
            patterns: c.patterns.clone(),
        }
    }).collect();
    b.basic_op(
        match_body_label,
        lir::OpKind::Case(clauses.len()),
        vec![Source::Variable(case_structure_ssa)], vec![]);
    for leaf in leaves.iter() {
        b.add_jump(match_body_label, *leaf);
        b.finish(*leaf);
    }
    b.finish(match_body_label);

    // === Entry block ===
    b.basic_op(
        main_cont,
        lir::OpKind::CaseStart {
            vars: def.match_val,
            clauses: clauses,
            value_vars: def.values.clone(),
        },
        vec![Source::Variable(def.match_val)],
        vec![case_structure_ssa]
    );
    // Jump to match body
    b.op_jump(main_cont, match_body_label);
    b.finish(main_cont);

    // === Guard exception case ===
    b.basic_op(
        guard_exception_label,
        lir::OpKind::CaseGuardThrow,
        vec![case_structure_ssa.into()],
        vec![]
    );
    b.op_tombstone(guard_exception_label, case_structure_ssa);
    b.basic_op(
        guard_exception_label,
        lir::OpKind::Jump,
        vec![], vec![]
    );
    exc_stack.add_error_jump(b, guard_exception_label,
                             guard_exception_ssa);

    (done_label, def.return_ssa)
}

use self::hir::SingleExpressionKind as HSEK;
impl hir::SingleExpression {
    fn lower(&self, b: &mut lir::cfg::FunctionCfgBuilder,
             env: &mut ScopeTracker,
             exc_stack: &mut ExceptionHandlerStack, in_block: LabelN)
             -> (LabelN, SSAVariable) {

        match self.kind {
            HSEK::InterModuleCall { ref module, ref name, ref args } => {
                let mut main_cont = in_block;

                let mut reads_r = vec![
                    lower_chain!(module, b, env, exc_stack, main_cont),
                    lower_chain!(name, b, env, exc_stack, main_cont),
                ];
                for arg in args.iter() {
                    reads_r.push(lower_chain!(arg, b, env, exc_stack, main_cont));
                }

                //reads_r.extend(args.iter().map(|a| a.lower(b, env, exc_stack)));
                let reads: Vec<_> = reads_r.iter()
                    .map(|r| lir::Source::Variable(*r))
                    .collect();

                let exc_ssa = env.new_ssa();
                b.basic_op(main_cont, lir::OpKind::Call, reads, vec![self.ssa, exc_ssa]);
                //let prev_block = b.get_block();

                //let throw_block = b.add_block();
                //b.set_block(throw_block);
                //b.basic_op(lir::OpKind::ReturnThrow, vec![], vec![]);

                let resume_block = b.add_block();
                //b.set_block(resume_block);

                b.add_jump(main_cont, resume_block);
                exc_stack.add_error_jump(b, main_cont, exc_ssa);
                //b.add_jump(prev_block, throw_block);

                // Because the exception value can never be used in the
                // continuation case, tombstone it.
                b.op_tombstone(resume_block, exc_ssa);

                // TODO: Should add tombstones in exception case as well

                (resume_block, self.ssa)
            },
            HSEK::ApplyCall { ref fun, ref args } => {
                let mut main_cont = in_block;

                let read_1_ssa = lower_chain!(fun, b, env, exc_stack, main_cont);

                let mut reads_r = vec![
                    read_1_ssa,
                ];
                for arg in args.iter() {
                    reads_r.push(lower_chain!(arg, b, env, exc_stack, main_cont));
                }
                //reads_r.extend(args.iter().map(|a| a.lower(b, env, exc_stack)));
                let reads: Vec<_> = reads_r.iter()
                    .map(|r| lir::Source::Variable(*r))
                    .collect();

                let exc_ssa = env.new_ssa();
                b.basic_op(main_cont, lir::OpKind::Apply, reads, vec![self.ssa, exc_ssa]);
                let resume_block = b.add_block();
                b.add_jump(main_cont, resume_block);

                exc_stack.add_error_jump(b, main_cont, exc_ssa);

                // Because the exception value can never be used in the
                // continuation case, tombstone it.
                b.op_tombstone(resume_block, exc_ssa);

                // TODO: Should add tombstones in exception case as well

                (resume_block, self.ssa)
            },
            HSEK::Atomic(ref atomic) => {
                b.basic_op(
                    in_block,
                    lir::OpKind::Move,
                    vec![lir::Source::Constant(atomic.clone())], vec![self.ssa]);
                (in_block, self.ssa)
            },
            HSEK::NamedFunction { ref name, is_lambda } => {
                if is_lambda {
                    (in_block, self.ssa) // TODO: What?
                } else {
                    let n = ::ir::FunctionIdent {
                        name: name.var.name.clone(),
                        arity: name.var.arity,
                        lambda: None,
                    };
                    b.basic_op(
                        in_block,
                        lir::OpKind::CaptureNamedFunction(n),
                        vec![], vec![self.ssa]);
                    (in_block, self.ssa)
                }
            },
            HSEK::ExternalNamedFunction { ref module, ref name } => {
                let n = ::ir::FunctionIdent {
                    name: name.var.name.clone(),
                    arity: name.var.arity,
                    lambda: None,
                };
                b.basic_op(
                    in_block,
                    lir::OpKind::CaptureExternalNamedFunction(module.clone(), n),
                    vec![], vec![self.ssa]);
                (in_block, self.ssa)
            },
            HSEK::Variable(_) => {
                (in_block, self.ssa)
            },
            HSEK::Let { ref val, ref body, ref vars } => {
                let mut main_cont = in_block;

                let val_ssa = lower_chain!(val, b, env, exc_stack, main_cont);
                assert!(val.ssa == val_ssa);

                let ssa_values: Vec<_> = vars.iter().map(|v| v.ssa).collect();
                b.op_unpack_value_list(main_cont, val_ssa, ssa_values);

                let body_ssa = lower_chain!(body, b, env, exc_stack, main_cont);
                assert!(body_ssa == self.ssa);

                (main_cont, self.ssa)
            },
            HSEK::Try { ref body, ref then_vars, ref then, ref catch_vars, ref catch } => {

                let mut main_cont = in_block;
                let mut catch_block = b.add_block();
                let catch_ssa = env.new_ssa();
                let exit_block = b.add_block();

                // == Body ==
                exc_stack.push_catch(catch_block, catch_ssa);
                let body_ssa = lower_chain!(body, b, env, exc_stack, main_cont);
                assert!(body_ssa == body.ssa);
                exc_stack.pop_catch();

                // == Then clause ==
                // Unpack the result value list of the body
                let then_unpack_vars: Vec<_> =
                    then_vars.iter().map(|v| v.ssa).collect();
                b.op_unpack_value_list(main_cont, body_ssa, then_unpack_vars);

                // Then block
                let then_ssa = lower_chain!(then, b, env, exc_stack, main_cont);
                assert!(then_ssa == then.ssa);
                b.op_jump(main_cont, exit_block);

                // == Catch clause ==
                // Unpack the result value list from the error
                let catch_unpack_vars: Vec<_> =
                    catch_vars.iter().map(|v| v.ssa).collect();
                b.op_unpack_value_list(catch_block, catch_ssa, catch_unpack_vars);

                let catch_ret_ssa = lower_chain!(
                    catch, b, env, exc_stack, catch_block);
                assert!(catch_ret_ssa == catch.ssa);
                b.op_jump(catch_block, exit_block);

                // == Phi node ==
                b.add_phi(main_cont, then_ssa, exit_block, self.ssa);
                b.add_phi(catch_block, catch_ret_ssa, exit_block, self.ssa);

                (exit_block, self.ssa)
            },
            HSEK::Catch { ref body } => {
                let mut main_cont = in_block;

                let mut catch_block = b.add_block();
                let catch_ssa = env.new_ssa();

                let ret_block = b.add_block();

                // Main path
                exc_stack.push_catch(catch_block, catch_ssa);
                let body_ssa = lower_chain!(body, b, env, exc_stack, main_cont);
                exc_stack.pop_catch();

                b.op_jump(main_cont, ret_block);
                b.add_phi(main_cont, body_ssa, ret_block, self.ssa);

                // Exception path

                // Unpack exception
                let e1_ssa = env.new_ssa();
                let e2_ssa = env.new_ssa();
                let e3_ssa = env.new_ssa();
                b.op_unpack_value_list(
                    catch_block,
                    catch_ssa,
                    vec![e1_ssa, e2_ssa, e3_ssa]
                );

                let case_main_block = b.add_block();
                let case_structure_ssa = env.new_ssa();
                b.basic_op(
                    catch_block,
                    lir::OpKind::CaseStart {
                        vars: catch_ssa,
                        clauses: vec![
                            lir::Clause {
                                patterns: vec![
                                    hir::Pattern {
                                        binds: vec![],
                                        node: hir::PatternNode::Atomic(
                                            ::parser::AtomicLiteral::Atom(
                                                ::Atom::from("throw"))),
                                    },
                                ]
                            },
                            lir::Clause {
                                patterns: vec![
                                    hir::Pattern {
                                        binds: vec![],
                                        node: hir::PatternNode::Atomic(
                                            ::parser::AtomicLiteral::Atom(
                                                ::Atom::from("exit"))),
                                    },
                                ]
                            },
                            lir::Clause {
                                patterns: vec![
                                    hir::Pattern {
                                        binds: vec![],
                                        node: hir::PatternNode::Atomic(
                                            ::parser::AtomicLiteral::Atom(
                                                ::Atom::from("error"))),
                                    },
                                ]
                            },
                        ],
                        value_vars: vec![],
                    },
                    vec![lir::Source::Variable(catch_ssa)],
                    vec![case_structure_ssa]
                );
                b.op_jump(catch_block, case_main_block);

                b.basic_op(
                    case_main_block,
                    lir::OpKind::Case(3),
                    vec![Source::Variable(case_structure_ssa)],
                    vec![]
                );

                // Failure block. Since this is the value returned
                // from the exception, this realistically never
                // happen.
                let fail_block = b.add_block();
                b.add_jump(case_main_block, fail_block);
                b.op_unreachable(fail_block);

                // Throw case
                let throw_block = b.add_block();
                b.add_jump(case_main_block, throw_block);
                b.op_case_guard_ok(throw_block, case_structure_ssa);
                b.op_jump(throw_block, ret_block);
                b.add_phi(throw_block, e2_ssa, ret_block, self.ssa);

                // Exit case
                let exit_block = b.add_block();
                b.add_jump(case_main_block, exit_block);
                b.op_case_guard_ok(exit_block, case_structure_ssa);
                let exit_res_ssa = env.new_ssa();
                b.op_make_tuple(
                    exit_block,
                    vec![
                        Source::Constant(::parser::AtomicLiteral::Atom(
                            ::Atom::from("EXIT"))),
                        Source::Variable(e2_ssa),
                    ],
                    exit_res_ssa
                );
                b.op_jump(exit_block, ret_block);
                b.add_phi(exit_block, exit_res_ssa, ret_block, self.ssa);

                // Error case
                let error_block = b.add_block();
                b.add_jump(case_main_block, error_block);
                b.op_case_guard_ok(error_block, case_structure_ssa);
                let error_stacktrace_ssa = env.new_ssa();
                b.op_primop(error_block, ::Atom::from("exc_trace"),
                            vec![Source::Variable(e3_ssa)],
                            vec![error_stacktrace_ssa]);
                let error_int_res_ssa = env.new_ssa();
                b.op_make_tuple(
                    error_block,
                    vec![
                        Source::Variable(e2_ssa),
                        Source::Variable(error_stacktrace_ssa),
                    ],
                    error_int_res_ssa,
                );
                let error_res_ssa = env.new_ssa();
                b.op_make_tuple(
                    error_block,
                    vec![
                        Source::Constant(::parser::AtomicLiteral::Atom(
                            ::Atom::from("EXIT"))),
                        Source::Variable(error_int_res_ssa),
                    ],
                    error_res_ssa
                );
                b.op_jump(error_block, ret_block);
                b.add_phi(error_block, error_res_ssa, ret_block, self.ssa);

                (ret_block, self.ssa)
            },
            HSEK::Case { ref val, ref clauses, ref values } => {
                let mut main_cont = in_block;

                // Lower values in the expression we match on
                let val_ssa = lower_chain!(val, b, env, exc_stack, main_cont);
                assert!(val.ssa == val_ssa);

                // Lower match values
                for value in values {
                    let value_ssa = lower_chain!(value, b, env, exc_stack, main_cont);
                    assert!(value_ssa == value.ssa);
                }
                let value_vars: Vec<_> = values.iter().map(|v| v.ssa).collect();

                // The central block of the Case structure. Contains only one
                // operation, the OpKind::Case.
                let match_body_label = b.add_block();

                // The block that control flow converges to after the match
                // structure is complete. No ops are inserted in this block
                // here, except a PHI node.
                let done_label = b.add_block();

                let case_fail_label = b.add_block();

                // The SSA variable representing the case structure.
                let case_structure_ssa = env.new_ssa();

                let guard_exception_label = b.add_block();
                let guard_exception_ssa = env.new_ssa();
                b.ensure_phi(guard_exception_label, guard_exception_ssa);

                // Fail leaf
                let fail_ssa = env.new_ssa();
                b.basic_op(case_fail_label,
                    lir::OpKind::Move,
                    vec![lir::Source::Constant(::parser::AtomicLiteral::Nil)],
                    vec![fail_ssa]);
                b.basic_op(case_fail_label, lir::OpKind::Jump, vec![], vec![]);
                exc_stack.add_error_jump(
                    b, case_fail_label, fail_ssa);

                let mut leaves = vec![case_fail_label];
                for (idx, clause) in clauses.iter().enumerate() {
                    let orig_clause_label = b.add_block();
                    let mut clause_label = orig_clause_label;
                    b.basic_op(clause_label,
                        lir::OpKind::CaseValues,
                        vec![lir::Source::Variable(case_structure_ssa)],
                        clause.patterns.iter()
                            .flat_map(|pattern| {
                                pattern.binds.iter()
                                    .map(|binding| binding.1)
                            }).collect(),
                    );
                    exc_stack.push_catch(guard_exception_label, guard_exception_ssa);
                    let (clause_label, guard_ret) = clause.guard.lower(
                        b, env, exc_stack, clause_label);
                    exc_stack.pop_catch();
                    b.basic_op(clause_label,
                        lir::OpKind::IfTruthy,
                        vec![lir::Source::Variable(guard_ret)],
                        vec![]
                    );
                    let guard_fail_label = b.add_block();
                    let mut leaf_body_label = b.add_block();
                    b.add_jump(clause_label, leaf_body_label);
                    b.add_jump(clause_label, guard_fail_label);
                    b.finish(clause_label);

                    // Guard fail
                    b.basic_op(guard_fail_label,
                               lir::OpKind::CaseGuardFail { clause_num: idx },
                               vec![lir::Source::Variable(case_structure_ssa)],
                               vec![]);
                    b.add_jump(guard_fail_label, match_body_label);
                    b.finish(guard_fail_label);

                    // Guard ok
                    b.basic_op(leaf_body_label,
                               lir::OpKind::CaseGuardOk,
                               vec![lir::Source::Variable(case_structure_ssa)],
                               vec![]);
                    b.op_tombstone(leaf_body_label, case_structure_ssa);

                    let (ret_label, clause_ret_ssa) = clause.body.lower(
                        b, env, exc_stack, leaf_body_label);
                    b.op_jump(ret_label, done_label);
                    b.add_phi(ret_label, clause_ret_ssa,
                              done_label, self.ssa);
                    b.finish(ret_label);

                    leaves.push(orig_clause_label);
                }

                // Match body
                let mut clauses: Vec<_> = clauses.iter().map(|c| {
                    lir::Clause {
                        patterns: c.patterns.clone(),
                    }
                }).collect();
                b.basic_op(
                    match_body_label,
                    lir::OpKind::Case(clauses.len()),
                    vec![Source::Variable(case_structure_ssa)], vec![]);
                for leaf in leaves.iter() {
                    b.add_jump(match_body_label, *leaf);
                    b.finish(*leaf);
                }
                b.finish(match_body_label);

                b.basic_op(
                    main_cont,
                    lir::OpKind::CaseStart {
                        vars: val.ssa,
                        clauses: clauses,
                        value_vars: value_vars,
                    },
                    vec![Source::Variable(val.ssa)],
                    vec![case_structure_ssa]
                );

                // Jump to match body
                b.op_jump(main_cont, match_body_label);
                b.finish(main_cont);

                // Guard exception case
                b.basic_op(
                    guard_exception_label,
                    lir::OpKind::CaseGuardThrow,
                    vec![case_structure_ssa.into()],
                    vec![]
                );
                b.op_tombstone(guard_exception_label, case_structure_ssa);
                b.basic_op(
                    guard_exception_label,
                    lir::OpKind::Jump,
                    vec![], vec![]
                );
                exc_stack.add_error_jump(b, guard_exception_label,
                                         guard_exception_ssa);

                (done_label, self.ssa)
            },
            HSEK::Tuple(ref elems) => {
                let mut main_cont = in_block;

                for elem in elems.iter() {
                    let elem_ssa = lower_chain!(elem, b, env, exc_stack, main_cont);
                    assert!(elem.ssa == elem_ssa);
                }

                b.basic_op(main_cont,
                           lir::OpKind::MakeTuple,
                           elems.iter()
                           .map(|e| lir::Source::Variable(e.ssa))
                           .collect(),
                           vec![self.ssa]);

                (main_cont, self.ssa)
            },
            HSEK::List{ ref head, ref tail } => {
                let mut main_cont = in_block;

                let tail_ssa = lower_chain!(tail, b, env, exc_stack, main_cont);
                assert!(tail.ssa == tail_ssa);

                for elem in head.iter() {
                    let elem_ssa = lower_chain!(elem, b, env, exc_stack, main_cont);
                    assert!(elem.ssa == elem_ssa);
                }

                let mut reads = vec![lir::Source::Variable(tail.ssa)];
                reads.extend(head.iter()
                             .map(|v| lir::Source::Variable(v.ssa)));

                b.basic_op(main_cont,
                           lir::OpKind::MakeList,
                           reads, vec![self.ssa]);

                (main_cont, self.ssa)
            },
            HSEK::Map { ref values, ref merge } => {
                let mut main_cont = in_block;

                let mut reads = Vec::new();
                for &(ref key, ref value) in values.iter() {
                    let key_ssa = lower_chain!(key, b, env, exc_stack, main_cont);
                    let value_ssa = lower_chain!(value, b, env, exc_stack, main_cont);

                    assert!(key_ssa == key.ssa);
                    assert!(value_ssa == value.ssa);

                    reads.push(lir::Source::Variable(key.ssa));
                    reads.push(lir::Source::Variable(value.ssa));
                }

                // TODO INCORRECT CONTROL FLOW: Merge!
                main_cont = merge.as_ref()
                    .map_or(
                        main_cont,
                        |m| m.lower(b, env, exc_stack, main_cont).0);

                b.basic_op(main_cont,
                           lir::OpKind::MakeMap,
                           reads, vec![self.ssa]);

                (main_cont, self.ssa)
            },
            HSEK::Binary(ref elems) => {

                let mut main_cont = in_block;

                let mut reads = Vec::new();
                for (val, opts) in elems.iter() {
                    assert!(opts.len() == 4);
                    let val_ssa = lower_chain!(val, b, env, exc_stack, main_cont);
                    //let val_ssa = fuse_ssa!(val.lower(b, env, exc_stack));

                    reads.push(lir::Source::Variable(val_ssa));
                    for opt in opts {
                        let n_ssa = lower_chain!(opt, b, env, exc_stack, main_cont);
                        reads.push(lir::Source::Variable(n_ssa));
                    }
                }

                b.basic_op(
                    main_cont,
                    lir::OpKind::MakeBinary,
                    reads,
                    vec![self.ssa]
                );
                (main_cont, self.ssa)
            },
            HSEK::PrimOp { ref name, ref args } if name == &*ATOM_RAISE => {

                let mut main_cont = in_block;

                // TODO:
                let mut reads = Vec::new();
                for arg in args.iter() {
                    let n_ssa = lower_chain!(arg, b, env, exc_stack, main_cont);
                    reads.push(lir::Source::Variable(n_ssa));
                }

                let exc_path_ssa = env.new_ssa();
                b.basic_op(main_cont, lir::OpKind::MakeTuple, reads, vec![exc_path_ssa]);

                b.basic_op(main_cont, lir::OpKind::Jump, vec![], vec![]);
                exc_stack.add_error_jump(b, main_cont, exc_path_ssa);

                let ret_dummy = b.add_block();
                b.basic_op(ret_dummy, lir::OpKind::MakeNoValue, vec![], vec![self.ssa]);

                (ret_dummy, self.ssa)
            },
            HSEK::PrimOp { ref name, ref args } => {
                let mut main_cont = in_block;

                //println!("PrimOp: {}", name);
                for arg in args.iter() {
                    let n_ssa = lower_chain!(arg, b, env, exc_stack, main_cont);
                    assert!(arg.ssa == n_ssa);
                }
                b.basic_op(
                    main_cont,
                    lir::OpKind::PrimOp(name.clone()),
                    args.iter().map(|a| lir::Source::Variable(a.ssa)).collect(),
                    vec![self.ssa]);

                (main_cont, self.ssa)
            },
            HSEK::Do(ref d1, ref d2) => {
                let mut main_cont = in_block;

                let d1_ssa = lower_chain!(d1, b, env, exc_stack, main_cont);
                assert!(d1.ssa == d1_ssa);

                let ret_ssa = lower_chain!(d2, b, env, exc_stack, main_cont);
                assert!(self.ssa == ret_ssa);

                (main_cont, self.ssa)
            },
            HSEK::Receive { ref clauses, ref timeout_time, ref timeout_body,
                            ref pattern_values } => {
                let mut main_cont = in_block;

                // Lower match values
                for value in pattern_values {
                    let m_n_ssa = lower_chain!(value, b, env, exc_stack, main_cont);
                    assert!(m_n_ssa == value.ssa);
                }
                let value_vars: Vec<_> = pattern_values.iter().map(|v| v.ssa)
                    .collect();

                let timeout_time_var = lower_chain!(
                    timeout_time, b, env, exc_stack, main_cont);

                let start_label = main_cont;
                let receive_loop_label = b.add_block();
                let timeout_body_label = b.add_block();
                let match_body_label = b.add_block();
                let expression_exit_label = b.add_block();

                let receive_structure_ssa = env.new_ssa();

                // Entry to receive structure (#start)
                b.basic_op(start_label,
                           lir::OpKind::ReceiveStart,
                           vec![lir::Source::Variable(timeout_time_var)],
                           vec![receive_structure_ssa]);
                b.add_jump(start_label, receive_loop_label);
                b.finish(start_label);

                // Receive loop block (#receive_loop)
                b.basic_op(receive_loop_label,
                           lir::OpKind::ReceiveWait,
                           vec![lir::Source::Variable(receive_structure_ssa)],
                           vec![]);
                b.add_jump(receive_loop_label, match_body_label);
                b.add_jump(receive_loop_label, timeout_body_label);
                b.finish(receive_loop_label);

                // Timeout branch (#timeout_body)
                let mut timeout_body_cont = timeout_body_label;
                b.op_tombstone(timeout_body_cont, receive_structure_ssa);
                let (clause_ret_block, clause_ret_ssa) = timeout_body.lower(
                    b, env, exc_stack, timeout_body_cont);
                b.basic_op(clause_ret_block, lir::OpKind::Jump, vec![], vec![]);
                b.add_jump(clause_ret_block, expression_exit_label);
                b.add_phi(clause_ret_block, clause_ret_ssa,
                          expression_exit_label, self.ssa);

                // Match logic (#match_body)
                let message_label = env.new_ssa();
                let case_ret_label = env.new_ssa();
                b.basic_op(
                    match_body_label,
                    lir::OpKind::ReceiveGetMessage,
                    vec![lir::Source::Variable(receive_structure_ssa)],
                    vec![message_label]
                );

                let clauses_slice = clauses.as_slice();
                let (case_ret_label, case_ret_ssa) = {
                    let r_clauses: Vec<_> = clauses_slice.iter()
                        .map(|c| {
                            // TODO: Fix this hacky clone
                            let body = c.body.clone();
                            CaseStructureClauseDef {
                                patterns: c.patterns.clone(),
                                guard: Box::new(|b, env, _exc_stack, block, _matches| {
                                    let ssa = env.new_ssa();
                                    b.op_move(
                                        block,
                                        Source::Constant(
                                            ::parser::AtomicLiteral::Atom(
                                                ::Atom::from("true"))),
                                        ssa
                                    );
                                    (block, ssa)
                                }),
                                body: Box::new(move |b, env, exc_stack, block, _matches|
                                               body.lower(b, env, exc_stack, block)),
                            }
                        }).collect();
                    let mut def = CaseStructureDef {
                        clauses: r_clauses,
                        entry_block: match_body_label,
                        match_fail: Some(receive_loop_label),
                        match_val: message_label,
                        values: value_vars.clone(),
                        return_ssa: case_ret_label,
                    };

                    case_structure(b, env, exc_stack, &def)
                };

                b.basic_op(case_ret_label, lir::OpKind::Jump, vec![], vec![]);
                b.add_jump(case_ret_label, expression_exit_label);
                b.add_phi(case_ret_label, case_ret_ssa,
                          expression_exit_label, self.ssa);

                (expression_exit_label, self.ssa)
            },
            HSEK::BindClosure { ref closure, lambda_env, env_ssa } => {
                // TODO
                let env_read_vars: Vec<_> = {
                    let lenv = env.get_lambda_env(lambda_env.unwrap());
                    lenv.captures.iter()
                        .map(|(_, r, _)| lir::Source::Variable(*r))
                        .collect()
                };

                b.basic_op(
                    in_block,
                    lir::OpKind::MakeClosureEnv {
                        env_idx: lambda_env.unwrap()
                    },
                    env_read_vars, vec![env_ssa]);
                b.basic_op(
                    in_block,
                    lir::OpKind::BindClosure {
                        ident: closure.ident.clone().unwrap(),
                    },
                    vec![Source::Variable(env_ssa)],
                    vec![self.ssa]);
                (in_block, self.ssa)
            },
            HSEK::BindClosures { closures: ref closures, lambda_env, ref body, env_ssa } => {
                let mut main_cont = in_block;

                // TODO
                let env_read_vars: Vec<_> = {
                    let lenv = env.get_lambda_env(lambda_env.unwrap());
                    lenv.captures.iter()
                        .map(|(_, r, _)| lir::Source::Variable(*r))
                        .collect()
                };

                b.basic_op(
                    main_cont,
                    lir::OpKind::MakeClosureEnv {
                        env_idx: lambda_env.unwrap()
                    },
                    env_read_vars, vec![env_ssa]
                );

                for closure in closures {
                    b.basic_op(
                        main_cont,
                        lir::OpKind::BindClosure {
                            ident: closure.ident.clone().unwrap(),
                        },
                        vec![Source::Variable(env_ssa)],
                        vec![closure.alias.as_ref().unwrap().ssa]
                    )
                }

                let ret_ssa = lower_chain!(body, b, env, exc_stack, main_cont);
                assert!(self.ssa == ret_ssa);
                (main_cont, self.ssa)
            },
            HSEK::ValueList(ref values) => {
                let mut main_cont = in_block;

                if values.len() == 0 {
                    println!("WARNING: Empty ValueList");
                    b.op_pack_value_list(main_cont, vec![], self.ssa);
                } else if values.len() == 1 {
                    let ssa = lower_chain!(values[0], b, env, exc_stack, main_cont);
                    assert!(ssa == values[0].ssa);
                    b.op_move(main_cont, Source::Variable(ssa), self.ssa)
                } else {
                    let values_src: Vec<_> =
                        values.iter().map(|v| {
                            let ssa = lower_chain!(v, b, env, exc_stack, main_cont);
                            assert!(ssa == v.ssa);
                            Source::Variable(ssa)
                        }).collect();
                    b.op_pack_value_list(main_cont, values_src, self.ssa);
                }

                (main_cont, self.ssa)
            }
            ref s => panic!("Unhandled: {:?}", s),
        }
    }
}
