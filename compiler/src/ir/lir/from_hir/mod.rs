use std::collections::{ HashMap, HashSet };

use ::ir::{ Module, FunctionDefinition };
use ::ir::hir;
use ::ir::lir;
//use ::ir::lir::Source;
use ::ir::hir::scope_tracker::ScopeTracker;
//use ::ir::SSAVariable;

use ::ssa::{ SSAVariable, SSAVariableGenerator };
use ::eir::{ Function, FunctionBuilder, Ebb, Value, AtomicTerm, Clause };
use ::eir::intern::Atom;
//use ::eir::cfg::{ FunctionCfgBuilder, LabelN };
use ::eir::op::{ OpKind };

mod exception_handler_stack;
use self::exception_handler_stack::ExceptionHandlerStack;

pub fn do_lower(module: &mut Module, env: &ScopeTracker) {
    module.lower(env)
}

struct LirLowerState<'a> {
    env: &'a ScopeTracker,
    exc_stack: ExceptionHandlerStack,
    bindings: HashMap<SSAVariable, Value>,
    val_buf: Vec<Value>,
    ssa_gen: SSAVariableGenerator,
}

impl Module {
    fn lower(&mut self, env: &ScopeTracker) {
        for fun in &mut self.functions {
            println!("{}", fun.ident);
            fun.lower(env);
        }
    }
}

impl FunctionDefinition {
    fn lower(&mut self, env: &ScopeTracker) {
        let mut ssa_gen = env.clone_ssa_generator();
        let mut function = Function::new(self.ident.clone());
        let mut bindings: HashMap<SSAVariable, Value> = HashMap::new();

        {
            let mut b = FunctionBuilder::new(&mut function);
            let entry = b.insert_ebb_entry();

            // Add args
            let mut env_ssa = None;
            let mut args = if self.ident.lambda.is_some() {
                let env_ssa_u = ssa_gen.next();
                bindings.insert(env_ssa_u, b.add_ebb_argument(entry));
                env_ssa = Some(env_ssa_u);
                vec![env_ssa_u]
            } else {
                vec![]
            };
            args.extend(self.hir_fun.args.iter().map(|a| {
                bindings.insert(a.ssa, b.add_ebb_argument(entry));
                a.ssa
            }));

            b.position_at_end(entry);

            if self.ident.lambda.is_some() {
                let lambda_env = env.get_lambda_env(self.ident.lambda.unwrap().0);
                let lambda_env_ssa = env_ssa.unwrap();

                let mut env_values = Vec::with_capacity(lambda_env.captures.len());
                b.op_unpack_env(bindings[&lambda_env_ssa],
                                lambda_env.captures.len(),
                                &mut env_values);
                for (val, cap) in env_values.iter().zip(lambda_env.captures.iter()) {
                    bindings.insert(cap.2, *val);
                }
                //println!("BIND: {:?} {:?}", lambda_env.captures, env_values);

                for (fun, ssa, alt_ssa) in lambda_env.meta_binds.iter() {
                    //println!("META BIND: {:?} {:?}", ssa, alt_ssa);
                    let val = b.op_bind_closure(
                        fun.clone(), bindings[&lambda_env_ssa]);
                    bindings.insert(*ssa, val);
                    if let Some(alt_ssa) = alt_ssa {
                        bindings.insert(*alt_ssa, val);
                    }
                }
            }

            let exception_block = b.insert_ebb();
            let exception_value = b.add_ebb_argument(exception_block);
            b.position_at_end(exception_block);
            b.op_return_throw(exception_value);

            let ok_block = b.insert_ebb();
            let ok_value = b.add_ebb_argument(ok_block);
            b.position_at_end(ok_block);
            b.op_return_ok(ok_value);

            b.position_at_end(entry);

            let mut st = LirLowerState {
                env: env,
                exc_stack: ExceptionHandlerStack::new(exception_block),
                bindings: bindings,
                val_buf: Vec::new(),
                ssa_gen: ssa_gen,
            };

            let ssa = self.hir_fun.body.lower(
                &mut b, &mut st);

            let call = b.create_ebb_call(ok_block, &[st.bindings[&ssa]]);;
            b.op_jump(call);

            st.exc_stack.finish();
        }

        self.eir_fun = Some(function);
    }
}

//macro_rules! lower_chain {
//    ($inner:expr, $builder:expr, $env:expr, $exc_stack:expr, $chain:expr) => {
//        {
//            let (label, ssa) = $inner.lower($builder, $env, $exc_stack, $chain);
//            $chain = label;
//            ssa
//        }
//    }
//}

struct CaseStructureDef<'a> {
    entry_ebb: Ebb,
    match_val: SSAVariable,
    clauses: Vec<CaseStructureClauseDef<'a>>,
    values: Vec<SSAVariable>,
    return_ssa: SSAVariable,
    match_fail: Option<Ebb>,
}
struct CaseStructureClauseDef<'a> {
    patterns: Vec<::ir::hir::Pattern>,
    guard: Box<Fn(&mut FunctionBuilder, &mut LirLowerState, &[SSAVariable])
                  -> SSAVariable + 'a>,
    body: Box<Fn(&mut FunctionBuilder, &mut LirLowerState, &[SSAVariable])
                 -> SSAVariable + 'a>,
}

fn case_structure(
    b: &mut FunctionBuilder,
    st: &mut LirLowerState,
    def: &CaseStructureDef,
) -> SSAVariable {
    let entry = def.entry_ebb;

    // The central block of the Case structure. Contains only one
    // operation, the OpKind::Case.
    let match_body_ebb = b.insert_ebb();

    // The block that control flow converges to after the match
    // structure is complete. No ops are inserted in this block
    // here, except a PHI node.
    let done_ebb = b.insert_ebb();
    let done_value = b.add_ebb_argument(done_ebb);
    st.bindings.insert(def.return_ssa, done_value);

    // Case start
    let (clauses, assign_map) = {
        let mut pattern_lower_util = ::ir::hir::PatternLowerUtil::new(
            b, &st.bindings);
        let clauses: Vec<_> = def.clauses.iter().map(|c| {
            let patterns = c.patterns.iter()
                .map(|p| p.to_eir(&mut pattern_lower_util))
                .collect();
            let assigns = pattern_lower_util.finish_clause();
            Clause {
                patterns: patterns,
                assigns: assigns,
            }
        }).collect();
        let assign_map = pattern_lower_util.finish();
        (clauses, assign_map)
    };

    st.val_buf.clear();
    {
        let bindings = &st.bindings;
        st.val_buf.extend(def.values.iter().map(|ssa| bindings[ssa]));
    }

    let case_structure_value = b.op_case_start(
        clauses.clone(), st.bindings[&def.match_val], &st.val_buf, match_body_ebb);
    //let exc_jump = b.create_ebb_call(match_body_ebb, &[]);
    //b.op_jump(exc_jump);

    // Create fail leaf
    let fail_ebb;
    if let Some(match_fail) = def.match_fail {
        fail_ebb = match_fail;
    } else {
        fail_ebb = b.insert_ebb();

        b.position_at_end(fail_ebb);
        let exc_value = b.create_atomic(AtomicTerm::Nil);
        let exc_jump = st.exc_stack.make_error_jump(b, exc_value);
        b.op_jump(exc_jump);
    }

    // Create leaves
    let mut leaves = Vec::new();
    for clause in def.clauses.iter() {
        let leaf_ebb = b.insert_ebb();
        leaves.push(leaf_ebb);
    }

    // Case body and jumps
    b.position_at_end(match_body_ebb);
    b.op_case_body(case_structure_value, def.clauses.len());

    let fail_call = b.create_ebb_call(fail_ebb, &[]);
    b.add_op_ebb_call(fail_call);
    for leaf in leaves.iter() {
        let call = b.create_ebb_call(*leaf, &[]);
        b.add_op_ebb_call(call);
    }

    for (idx, (clause, eir_clause)) in def.clauses.iter().zip(clauses.iter()).enumerate() {
        let ebb = leaves[idx];
        b.position_at_end(ebb);

        let case_values_ssa: Vec<_> = clause.patterns.iter()
            .flat_map(|pattern| pattern.binds.iter().map(|binding| binding.1))
            .collect();
        assert!(case_values_ssa.len() == eir_clause.assigns.len());
        b.op_case_values(case_structure_value, case_values_ssa.len(), &mut st.val_buf);
        for (idx, val) in eir_clause.assigns.iter().enumerate() {
            st.bindings.insert(assign_map[val], st.val_buf[idx]);
        }

        let guard_fail_ebb = b.insert_ebb();
        let _guard_fail_value = b.add_ebb_argument(guard_fail_ebb);

        // ==== Main path ====
        // Guard
        st.exc_stack.push_handler(guard_fail_ebb);
        let guard_ret_ssa = (clause.guard)(b, st, &case_values_ssa);
        st.exc_stack.pop_handler();

        // Guard value chech
        let fail_call = b.create_ebb_call(guard_fail_ebb, &[]);
        b.op_branch_not_truthy(st.bindings[&guard_ret_ssa], fail_call);

        // CaseGuardOk
        b.op_case_guard_ok(case_structure_value);

        // Body
        let body_ssa = (clause.body)(b, st, &case_values_ssa);

        // Jump to merging Ebb
        let finish_call = b.create_ebb_call(done_ebb, &[st.bindings[&body_ssa]]);
        b.op_jump(finish_call);

        // ==== Fail path ====
        b.position_at_end(guard_fail_ebb);
        b.op_case_guard_fail(case_structure_value, idx);

        let case_body_call = b.create_ebb_call(match_body_ebb, &[]);
        b.op_jump(case_body_call);
    }

    b.position_at_end(done_ebb);

    def.return_ssa
}

use self::hir::SingleExpressionKind as HSEK;
impl hir::SingleExpression {
    fn lower(&self, b: &mut FunctionBuilder, st: &mut LirLowerState)
             -> SSAVariable {

        //println!("-> lower");
        let a = match self.kind {
            HSEK::InterModuleCall { ref module, ref name, ref args } => {

                let module_ssa = module.lower(b, st);
                let name_ssa = name.lower(b, st);
                let reads_val: Vec<_> = args.iter()
                    .map(|arg| {
                        let ssa = arg.lower(b, st);
                        st.bindings[&ssa]
                    }).collect();

                let (ok_val, exc_val) = b.op_call(
                    st.bindings[&module_ssa],
                    st.bindings[&name_ssa],
                    &reads_val,
                );
                let exc_jump = st.exc_stack.make_error_jump(b, exc_val);
                b.add_op_ebb_call(exc_jump);

                st.bindings.insert(self.ssa, ok_val);

                // TODO: Should add tombstones

                self.ssa
            },
            HSEK::ApplyCall { ref fun, ref args } => {
                let fun_ssa = fun.lower(b, st);

                let args_val: Vec<_> = args.iter().map(|arg| {
                    let ssa = arg.lower(b, st);
                    st.bindings[&ssa]
                }).collect();

                let (ok_val, exc_val) = b.op_apply(
                    st.bindings[&fun_ssa],
                    &args_val,
                );
                let exc_jump = st.exc_stack.make_error_jump(b, exc_val);
                b.add_op_ebb_call(exc_jump);

                st.bindings.insert(self.ssa, ok_val);

                // TODO: Should add tombstones

                self.ssa
            },
            HSEK::Atomic(ref atomic) => {

                let value = b.create_atomic(atomic.clone().into());
                st.bindings.insert(self.ssa, value);

                self.ssa
            },
            HSEK::NamedFunction { ref name, is_lambda } => {
                if is_lambda {
                    //(in_block, self.ssa) // TODO: What?
                    self.ssa
                } else {
                    let value = b.op_capture_named_function(name.var.clone());
                    st.bindings.insert(self.ssa, value);
                    self.ssa
                }
            },
            HSEK::ExternalNamedFunction { ref name } => {

                let value = b.op_capture_named_function(name.var.clone());
                st.bindings.insert(self.ssa, value);
                self.ssa
            },
            HSEK::Variable(_) => {
                self.ssa
            },
            HSEK::Let { ref val, ref body, ref vars } => {

                let val_ssa = val.lower(b, st);
                assert!(val.ssa == val_ssa);

                b.op_unpack_value_list(st.bindings[&val_ssa],
                                       vars.len(), &mut st.val_buf);
                for (var, value) in vars.iter().zip(st.val_buf.iter()) {
                    st.bindings.insert(var.ssa, *value);
                }

                let body_ssa = body.lower(b, st);
                assert!(body_ssa == self.ssa);

                self.ssa
            },
            HSEK::Try { ref body, ref then_vars, ref then, ref catch_vars, ref catch } => {

                let catch_block = b.insert_ebb();
                let catch_value = b.add_ebb_argument(catch_block);

                let exit_block = b.insert_ebb();
                let exit_value = b.add_ebb_argument(exit_block);

                // == Body ==
                st.exc_stack.push_handler(catch_block);
                let body_ssa = body.lower(b, st);
                assert!(body_ssa == body.ssa);
                st.exc_stack.pop_handler();

                // == Then clause ==
                // Unpack the result value list of the body
                b.op_unpack_value_list(st.bindings[&body_ssa],
                                       then_vars.len(), &mut st.val_buf);
                for (ssa, val) in then_vars.iter().zip(st.val_buf.iter()) {
                    st.bindings.insert(ssa.ssa, *val);
                }

                // Then block
                let then_ssa = then.lower(b, st);
                assert!(then_ssa == then.ssa);
                let then_jump = b.create_ebb_call(
                    exit_block, &[st.bindings[&then_ssa]]);
                b.op_jump(then_jump);

                // == Catch clause ==
                // Unpack the result value list from the error
                b.position_at_end(catch_block);
                b.op_unpack_value_list(catch_value, catch_vars.len(), &mut st.val_buf);
                for (ssa, val) in catch_vars.iter().zip(st.val_buf.iter()) {
                    st.bindings.insert(ssa.ssa, *val);
                }

                let catch_ret_ssa = catch.lower(b, st);
                assert!(catch_ret_ssa == catch.ssa);

                let catch_jump = b.create_ebb_call(
                    exit_block, &[st.bindings[&catch_ret_ssa]]);
                b.op_jump(catch_jump);

                b.position_at_end(exit_block);
                st.bindings.insert(self.ssa, exit_value);

                self.ssa
            },
            //HSEK::Catch { ref body } => {
            //    let mut catch_block = b.add_block();
            //    let catch_ssa = b.new_ssa();

            //    let ret_block = b.add_block();

            //    // Main path
            //    exc_stack.push_catch(catch_block, catch_ssa);
            //    let body_ssa = lower_chain!(body, b, env, exc_stack, main_cont);
            //    exc_stack.pop_catch();

            //    let ret_jump = b.op_jump(main_cont, ret_block);
            //    b.add_phi(ret_jump, body_ssa, self.ssa);

            //    // Exception path

            //    // Unpack exception
            //    let e1_ssa = b.new_ssa();
            //    let e2_ssa = b.new_ssa();
            //    let e3_ssa = b.new_ssa();
            //    b.op_unpack_value_list(
            //        catch_block,
            //        catch_ssa,
            //        vec![e1_ssa, e2_ssa, e3_ssa]
            //    );

            //    let case_main_block = b.add_block();
            //    let case_structure_ssa = b.new_ssa();
            //    b.basic_op(
            //        catch_block,
            //        OpKind::CaseStart {
            //            vars: catch_ssa,
            //            clauses: vec![
            //                Clause {
            //                    patterns: vec![
            //                        ::eir::pattern::Pattern {
            //                            binds: HashSet::new(),
            //                            node: ::eir::pattern::PatternNode::Atomic(
            //                                Atom::from("throw").into()),
            //                        },
            //                    ]
            //                },
            //                Clause {
            //                    patterns: vec![
            //                        ::eir::pattern::Pattern {
            //                            binds: HashSet::new(),
            //                            node: ::eir::pattern::PatternNode::Atomic(
            //                                Atom::from("exit").into()),
            //                        },
            //                    ]
            //                },
            //                Clause {
            //                    patterns: vec![
            //                        ::eir::pattern::Pattern {
            //                            binds: HashSet::new(),
            //                            node: ::eir::pattern::PatternNode::Atomic(
            //                                Atom::from("error").into()),
            //                        },
            //                    ]
            //                },
            //            ],
            //            value_vars: vec![],
            //        },
            //        vec![catch_ssa.into()],
            //        vec![case_structure_ssa]
            //    );
            //    b.add_jump(catch_block, case_main_block);

            //    b.basic_op(
            //        case_main_block,
            //        OpKind::Case(3),
            //        vec![case_structure_ssa.into()],
            //        vec![]
            //    );

            //    // Failure block. Since this is the value returned
            //    // from the exception, this realistically never
            //    // happen.
            //    let fail_block = b.add_block();
            //    b.add_jump(case_main_block, fail_block);
            //    b.op_unreachable(fail_block);

            //    // Throw case
            //    let throw_block = b.add_block();
            //    b.add_jump(case_main_block, throw_block);
            //    b.op_case_values(throw_block, case_structure_ssa, vec![]);
            //    b.op_case_guard_ok(throw_block, case_structure_ssa);
            //    let ret_jump = b.op_jump(throw_block, ret_block);
            //    b.add_phi(ret_jump, e2_ssa, self.ssa);

            //    // Exit case
            //    let exit_block = b.add_block();
            //    b.add_jump(case_main_block, exit_block);
            //    b.op_case_values(exit_block, case_structure_ssa, vec![]);
            //    b.op_case_guard_ok(exit_block, case_structure_ssa);
            //    let exit_res_ssa = b.new_ssa();
            //    b.op_make_tuple(
            //        exit_block,
            //        vec![
            //            Atom::from("EXIT").into(),
            //            Source::Variable(e2_ssa),
            //        ],
            //        exit_res_ssa
            //    );
            //    let ret_jump = b.op_jump(exit_block, ret_block);
            //    b.add_phi(ret_jump, exit_res_ssa, self.ssa);

            //    // Error case
            //    let error_block = b.add_block();
            //    b.add_jump(case_main_block, error_block);
            //    b.op_case_values(error_block, case_structure_ssa, vec![]);
            //    b.op_case_guard_ok(error_block, case_structure_ssa);
            //    let error_stacktrace_ssa = b.new_ssa();
            //    b.op_primop(error_block, Atom::from("exc_trace"),
            //                vec![Source::Variable(e3_ssa)],
            //                vec![error_stacktrace_ssa]);
            //    let error_int_res_ssa = b.new_ssa();
            //    b.op_make_tuple(
            //        error_block,
            //        vec![
            //            Source::Variable(e2_ssa),
            //            Source::Variable(error_stacktrace_ssa),
            //        ],
            //        error_int_res_ssa,
            //    );
            //    let error_res_ssa = b.new_ssa();
            //    b.op_make_tuple(
            //        error_block,
            //        vec![
            //            Atom::from("EXIT").into(),
            //            Source::Variable(error_int_res_ssa),
            //        ],
            //        error_res_ssa
            //    );
            //    let ret_jump = b.op_jump(error_block, ret_block);
            //    b.add_phi(ret_jump, error_res_ssa, self.ssa);

            //    (ret_block, self.ssa)
            //},
            HSEK::Case { ref val, ref clauses, ref values } => {
                // Lower values in the expression we match on
                let val_ssa = val.lower(b, st);
                assert!(val.ssa == val_ssa);

                // Lower match values
                for value in values {
                    let value_ssa = value.lower(b, st);
                    assert!(value_ssa == value.ssa);
                }
                let value_vars: Vec<_> = values.iter().map(|v| v.ssa).collect();

                let entry = b.current_ebb();
                b.assert_at_end();

                // Construct structure
                let case_ret_ssa = {
                    let case_def = CaseStructureDef {
                        entry_ebb: entry,
                        match_val: val_ssa,
                        clauses: clauses.iter().map(|cl| {
                            let body = cl.body.clone();
                            let guard = cl.guard.clone();
                            CaseStructureClauseDef {
                                body: Box::new(
                                    move |b, st, _matches|
                                    body.lower(b, st)
                                ),
                                guard: Box::new(
                                    move |b, st, _matches|
                                    guard.lower(b, st)
                                ),
                                patterns: cl.patterns.clone(),
                            }
                        }).collect(),
                        match_fail: None,
                        values: value_vars.clone(),
                        return_ssa: self.ssa,
                    };

                    case_structure(b, st, &case_def)
                };

                assert!(case_ret_ssa == self.ssa);

                self.ssa
            },
            HSEK::Tuple(ref elems) => {
                for elem in elems.iter() {
                    let elem_ssa = elem.lower(b, st);
                    assert!(elem.ssa == elem_ssa);
                }

                st.val_buf.clear();
                for elem in elems.iter() {
                    st.val_buf.push(st.bindings[&elem.ssa]);
                }

                let value = b.op_make_tuple(&st.val_buf);

                st.bindings.insert(self.ssa, value);

                self.ssa
            },
            HSEK::List{ ref head, ref tail } => {
                for elem in head.iter() {
                    let elem_ssa = elem.lower(b, st);
                    assert!(elem.ssa == elem_ssa);
                }

                let tail_ssa = tail.lower(b, st);
                assert!(tail.ssa == tail_ssa);

                st.val_buf.clear();
                for elem in head.iter() {
                    st.val_buf.push(st.bindings[&elem.ssa]);
                }

                let val = b.op_make_list(&st.val_buf, st.bindings[&tail_ssa]);
                st.bindings.insert(self.ssa, val);

                self.ssa
            },
            HSEK::Map { ref values, ref merge } => {
                if let Some(merge) = merge {
                    let merge_ssa = merge.lower(b, st);
                    assert!(merge_ssa == merge.ssa);
                }

                for &(ref key, ref value, _assoc) in values.iter() {
                    // TODO: Handle Assoc

                    let key_ssa = key.lower(b, st);
                    assert!(key_ssa == key.ssa);

                    let value_ssa = value.lower(b, st);
                    assert!(value_ssa == value.ssa);
                }

                st.val_buf.clear();
                for &(ref key, ref value, _assoc) in values.iter() {
                    st.val_buf.push(st.bindings[&key.ssa]);
                    st.val_buf.push(st.bindings[&value.ssa]);
                }

                // TODO INCORRECT CONTROL FLOW: Merge!
                //main_cont = merge.as_ref()
                //    .map_or(
                //        main_cont,
                //        |m| m.lower(b, env, exc_stack, main_cont).0);

                //b.basic_op(main_cont,
                //           OpKind::MakeMap,
                //           reads, vec![self.ssa]);

                let value = b.op_make_map(
                    merge.as_ref().map(|m| st.bindings[&m.ssa]),
                    &st.val_buf,
                );
                st.bindings.insert(self.ssa, value);

                self.ssa
            },
            HSEK::Binary(ref elems) => {

                //let mut reads = Vec::new();
                for (val, opts) in elems.iter() {
                    let val_ssa = val.lower(b, st);
                    assert!(val.ssa == val_ssa);

                    assert!(opts.len() == 4);
                    for opt in opts {
                        //let n_ssa = lower_chain!(opt, b, env, exc_stack, main_cont);
                        let n_ssa = opt.lower(b, st);
                        assert!(opt.ssa == n_ssa);
                    }
                }

                st.val_buf.clear();
                for (val, opts) in elems.iter() {
                    st.val_buf.push(st.bindings[&val.ssa]);
                    for opt in opts {
                        st.val_buf.push(st.bindings[&opt.ssa]);
                    }
                }

                //b.basic_op(
                //    main_cont,
                //    OpKind::MakeBinary,
                //    reads,
                //    vec![self.ssa]
                //);

                let value = b.op_make_binary(&st.val_buf);
                st.bindings.insert(self.ssa, value);

                self.ssa
            },
            HSEK::PrimOp { ref name, ref args } if name == &Atom::from("raise") => {
                //let mut main_cont = in_block;

                for arg in args.iter() {
                    let n_ssa = arg.lower(b, st);
                    assert!(arg.ssa == n_ssa);
                }

                st.val_buf.clear();
                for arg in args.iter() {
                    st.val_buf.push(st.bindings[&arg.ssa]);
                }

                // TODO:
                //let mut reads = Vec::new();
                //for arg in args.iter() {
                //    let n_ssa = lower_chain!(arg, b, env, exc_stack, main_cont);
                //    reads.push(Source::Variable(n_ssa));
                //}

                let exc_value = b.op_make_tuple(&st.val_buf);
                let exc_jump = st.exc_stack.make_error_jump(b, exc_value);
                b.op_jump(exc_jump);

                let ret_dummy = b.insert_ebb();
                b.position_at_end(ret_dummy);
                let value = b.op_make_no_value();
                st.bindings.insert(self.ssa, value);

                self.ssa
            },
            HSEK::PrimOp { ref name, ref args } if name == &Atom::from("match_fail") => {
                // Lower args
                for arg in args.iter() {
                    let n_ssa = arg.lower(b, st);
                    assert!(arg.ssa == n_ssa);
                }

                // Construct value list
                st.val_buf.clear();
                for arg in args.iter() {
                    st.val_buf.push(st.bindings[&arg.ssa]);
                }

                // Make exception tuple and jump to handler
                let exc_value = b.op_make_tuple(&st.val_buf);
                let exc_jump = st.exc_stack.make_error_jump(b, exc_value);
                b.op_jump(exc_jump);

                // Dummy continuation
                let ret_dummy = b.insert_ebb();
                b.position_at_end(ret_dummy);
                let value = b.op_make_no_value();
                st.bindings.insert(self.ssa, value);

                self.ssa
            },
            HSEK::PrimOp { ref name, ref args } => {
                //println!("PrimOp: {}", name);

                //for arg in args.iter() {
                //    let n_ssa = arg.lower(b, st);
                //    assert!(arg.ssa == n_ssa);
                //}

                //st.val_buf.clear();
                //for arg in args.iter() {
                //    st.val_buf.push(st.bindings[arg.ssa]);
                //}

                //b.

                //b.basic_op(
                //    main_cont,
                //    OpKind::PrimOp(name.clone()),
                //    args.iter().map(|a| a.ssa.into()).collect(),
                //    vec![self.ssa]);

                //(main_cont, self.ssa)

                unimplemented!("PrimOp: {}", name);
            },
            HSEK::Do(ref d1, ref d2) => {
                let d1_ssa = d1.lower(b, st);
                assert!(d1.ssa == d1_ssa);

                let ret_ssa = d2.lower(b, st);
                assert!(self.ssa == ret_ssa);

                self.ssa
            },
            HSEK::Receive { ref clauses, ref timeout_time, ref timeout_body,
                            ref pattern_values } => {

                // Lower match values
                for value in pattern_values {
                    //let m_n_ssa = lower_chain!(value, b, env, exc_stack, main_cont);
                    let m_n_ssa = value.lower(b, st);
                    assert!(m_n_ssa == value.ssa);
                }
                let value_vars: Vec<_> = pattern_values.iter().map(|v| v.ssa)
                    .collect();

                let timeout_time_ssa = timeout_time.lower(b, st);

                let receive_loop_ebb = b.insert_ebb();
                let timeout_body_ebb = b.insert_ebb();
                let match_body_ebb = b.insert_ebb();

                let expression_exit_ebb = b.insert_ebb();
                let expression_exit_ret_val =
                    b.add_ebb_argument(expression_exit_ebb);
                st.bindings.insert(self.ssa, expression_exit_ret_val);

                //let receive_structure_ssa = b.new_ssa();

                // Entry to receive structure (#start)
                let receive_loop_var = b.op_receive_start(
                    st.bindings[&timeout_time_ssa]);
                let loop_ebb_call = b.create_ebb_call(receive_loop_ebb, &[]);
                b.op_jump(loop_ebb_call);

                // Receive loop block (#receive_loop)
                b.position_at_end(receive_loop_ebb);
                let match_body_call = b.create_ebb_call(match_body_ebb, &[]);
                let timeout_body_call = b.create_ebb_call(timeout_body_ebb, &[]);
                b.op_receive_wait(receive_loop_var, match_body_call,
                                  timeout_body_call);

                // Timeout branch (#timeout_body)
                b.position_at_end(timeout_body_ebb);
                //b.op_tombstone(receive_structure_var);
                let case_ret_ssa = timeout_body.lower(b, st);
                let case_exit_call = b.create_ebb_call(
                    expression_exit_ebb, &[st.bindings[&case_ret_ssa]]);
                b.op_jump(case_exit_call);
                //b.basic_op(clause_ret_block, OpKind::Jump, vec![], vec![]);
                //let exit_jump = b.add_jump(clause_ret_block, expression_exit_label);
                //b.add_phi(exit_jump, clause_ret_ssa, self.ssa);

                // Match logic (#match_body)
                let message_val = b.op_receive_get_message(receive_loop_var);
                let message_ssa = st.ssa_gen.next();
                st.bindings.insert(message_ssa, message_val);
                //let message_label = b.new_ssa();
                //let case_ret_label = b.new_ssa();
                //b.basic_op(
                //    match_body_label,
                //    OpKind::ReceiveGetMessage,
                //    vec![Source::Variable(receive_structure_ssa)],
                //    vec![message_label]
                //);

                let case_ret_ssa = st.ssa_gen.next();

                let clauses_slice = clauses.as_slice();
                let case_ret_ssa = {
                    let r_clauses: Vec<_> = clauses_slice.iter()
                        .map(|c| {
                            // TODO: Fix this hacky clone
                            let body = c.body.clone();
                            CaseStructureClauseDef {
                                patterns: c.patterns.clone(),
                                guard: Box::new(|b, st, _matches| {
                                    let val = b.create_atomic(
                                        AtomicTerm::Atom(Atom::from("true")));
                                    let ssa = st.ssa_gen.next();
                                    st.bindings.insert(ssa, val);
                                    ssa
                                }),
                                body: Box::new(
                                    move |b, st, _matches| body.lower(b, st)),
                            }
                        }).collect();
                    let def = CaseStructureDef {
                        clauses: r_clauses,
                        entry_ebb: match_body_ebb,
                        match_fail: Some(receive_loop_ebb),
                        match_val: message_ssa,
                        values: value_vars.clone(),
                        return_ssa: case_ret_ssa,
                    };

                    case_structure(b, st, &def)
                };

                let ret_call = b.create_ebb_call(expression_exit_ebb,
                                                 &[st.bindings[&case_ret_ssa]]);
                b.op_jump(ret_call);

                b.position_at_end(expression_exit_ebb);

                self.ssa
            },
            HSEK::BindClosure { ref closure, lambda_env, env_ssa } => {
                // TODO

                st.val_buf.clear();
                let lenv = st.env.get_lambda_env(lambda_env.unwrap());
                for (_, r, _) in lenv.captures.iter() {
                    st.val_buf.push(st.bindings[r]);
                }

                let closure_env = b.op_make_closure_env(
                    lambda_env.unwrap(), &st.val_buf);
                st.bindings.insert(env_ssa, closure_env);

                let closure = b.op_bind_closure(
                    closure.ident.clone().unwrap(), closure_env);
                st.bindings.insert(self.ssa, closure);

                self.ssa
            },
            HSEK::BindClosures { ref closures, lambda_env, ref body, env_ssa } => {
                // TODO

                st.val_buf.clear();
                let lenv = st.env.get_lambda_env(lambda_env.unwrap());
                for (_, r, _) in lenv.captures.iter() {
                    st.val_buf.push(st.bindings[r]);
                }

                let closure_env = b.op_make_closure_env(
                    lambda_env.unwrap(), &st.val_buf);
                st.bindings.insert(env_ssa, closure_env);

                for closure in closures {
                    let closure_val = b.op_bind_closure(
                        closure.ident.clone().unwrap(), closure_env);
                    st.bindings.insert(closure.alias.as_ref().unwrap().ssa, closure_val);
                }

                let ret_ssa = body.lower(b, st);
                assert!(self.ssa == ret_ssa);

                self.ssa
            },
            HSEK::ValueList(ref values) => {

                if values.len() == 0 {
                    println!("WARNING: Empty ValueList");
                    let value = b.op_pack_value_list(&[]);
                    st.bindings.insert(self.ssa, value);
                } else if values.len() == 1 {
                    let ssa = values[0].lower(b, st);
                    assert!(ssa == values[0].ssa);
                    let val = st.bindings[&ssa];
                    st.bindings.insert(self.ssa, val);
                } else {
                    for value in values.iter() {
                        let ssa = value.lower(b, st);
                        assert!(ssa == value.ssa);
                    }

                    st.val_buf.clear();
                    for value in values.iter() {
                        let val = st.bindings[&value.ssa];
                        st.val_buf.push(val);
                    }

                    let value = b.op_pack_value_list(&st.val_buf);
                    st.bindings.insert(self.ssa, value);
                }

                self.ssa
            }
            ref s => panic!("Unhandled: {:?}", s),
        };
        //println!("<- lower");
        a
    }
}
