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

            builder.basic_op(
                entry,
                lir::OpKind::Arguments, vec![],
                self.hir_fun.args.iter().map(|a| a.ssa).collect());

            let exc_block = builder.add_block();
            let exc_ssa = env.new_ssa();

            let mut exc_stack = ExceptionHandlerStack::new(exc_block, exc_ssa);
            let (block, ssa) = self.hir_fun.body.lower(&mut builder, env, &mut exc_stack,
                                              entry);
            exc_stack.finish();

            builder.basic_op(
                block,
                lir::OpKind::ReturnOk,
                vec![lir::Source::Variable(ssa)], vec![]);

            builder.basic_op(exc_block, lir::OpKind::ReturnThrow, vec![], vec![]);
        }

        self.lir_function = Some(cfg);
    }
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

                let (main_cont, read_1_ssa) = module.lower(b, env, exc_stack, main_cont);
                let (main_cont, read_2_ssa) = name.lower(b, env, exc_stack, main_cont);

                let mut reads_r = vec![
                    read_1_ssa,
                    read_2_ssa,
                ];
                for arg in args.iter() {
                    let (main_cont, read_n_ssa) = arg.lower(b, env, exc_stack, main_cont);
                    reads_r.push(read_n_ssa);
                }

                //reads_r.extend(args.iter().map(|a| a.lower(b, env, exc_stack)));
                let reads: Vec<_> = reads_r.iter()
                    .map(|r| lir::Source::Variable(*r))
                    .collect();

                b.basic_op(main_cont, lir::OpKind::Call, reads, vec![self.ssa]);
                //let prev_block = b.get_block();

                //let throw_block = b.add_block();
                //b.set_block(throw_block);
                //b.basic_op(lir::OpKind::ReturnThrow, vec![], vec![]);

                let resume_block = b.add_block();
                //b.set_block(resume_block);

                b.add_jump(main_cont, resume_block);
                exc_stack.add_error_jump(
                    b, main_cont, ::util::ssa_variable::INVALID_SSA);
                //b.add_jump(prev_block, throw_block);

                (resume_block, self.ssa)
            },
            HSEK::ApplyCall { ref fun, ref args } => {
                let mut main_cont = in_block;

                let (main_cont, read_1_ssa) = fun.lower(b, env, exc_stack, main_cont);

                let mut reads_r = vec![
                    read_1_ssa,
                ];
                for arg in args.iter() {
                    let (main_cont, read_n_ssa) = arg.lower(b, env, exc_stack, main_cont);
                    reads_r.push(read_n_ssa);
                }
                //reads_r.extend(args.iter().map(|a| a.lower(b, env, exc_stack)));
                let reads: Vec<_> = reads_r.iter()
                    .map(|r| lir::Source::Variable(*r))
                    .collect();

                b.basic_op(main_cont, lir::OpKind::Apply, reads, vec![self.ssa]);
                //let prev_block = b.get_block();

                //let throw_block = b.add_block();
                //b.set_block(throw_block);
                //b.basic_op(lir::OpKind::ReturnThrow, vec![], vec![]);

                let resume_block = b.add_block();
                //b.set_block(resume_block);

                b.add_jump(main_cont, resume_block);
                exc_stack.add_error_jump(b, main_cont,
                                         ::util::ssa_variable::INVALID_SSA);

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
            HSEK::Let { ref val, ref body, .. } => {
                let mut main_cont = in_block;

                for v in val.values.iter() {
                    let (main_cont, v_ssa) = v.lower(
                        b, env, exc_stack, main_cont);
                    assert!(v.ssa == v_ssa);
                }
                let (main_cont, body_ssa) = body.lower(
                    b, env, exc_stack, main_cont);
                assert!(body_ssa == self.ssa);

                (main_cont, self.ssa)
            },
            HSEK::Try { ref body, ref then, ref catch, .. } => {

                let mut main_cont = in_block;
                let mut catch_block = b.add_block();
                let catch_ssa = env.new_ssa();
                let exit_block = b.add_block();

                exc_stack.push_catch(catch_block, catch_ssa);
                for expr in body.values.iter() {
                    let (main_cont, expr_ssa) = expr.lower(
                        b, env, exc_stack, main_cont);
                    assert!(expr.ssa == expr_ssa);
                }
                exc_stack.pop_catch();

                let (main_cont, then_ssa) = then.lower(
                    b, env, exc_stack, main_cont);
                assert!(then_ssa == then.ssa);
                b.op_jump(main_cont, exit_block);

                // TODO TODO FIXME Not 100% sure
                //chain_ssa_block_fuse!(main_cont, catch.lower(
                //    b, env, exc_stack, main_cont));
                //b.op_jump(main_cont, exit_block);
                let (catch_block, catch_ssa) = catch.lower(
                    b, env, exc_stack, main_cont);
                assert!(catch_ssa == catch.ssa);
                b.op_jump(catch_block, exit_block);

                // TODO: Phi node

                (exit_block, then.ssa)
            },
            HSEK::Case { ref val, ref clauses, ref values } => {

                let mut main_cont = in_block;

                // Lower values in the expression we match on
                for v in &val.values {
                    let (main_cont, v_ssa) = v.lower(
                        b, env, exc_stack, main_cont);
                    assert!(v.ssa == v_ssa);
                }

                // Lower match values
                for value in values {
                    let (main_cont, value_ssa) = value.lower(
                        b, env, exc_stack, main_cont);
                    assert!(value_ssa == value.ssa);
                }
                let value_vars: Vec<_> = values.iter().map(|v| v.ssa).collect();

                let match_body_label = b.add_block();
                let done_label = b.add_block();
                let case_fail_label = b.add_block();

                let case_structure_ssa = env.new_ssa();

                // Fail leaf
                // TODO: Proper error
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
                    let mut clause_label = b.add_block();
                    b.basic_op(clause_label,
                        lir::OpKind::CaseValues,
                        vec![lir::Source::Variable(case_structure_ssa)],
                        clause.patterns.iter()
                            .flat_map(|pattern| {
                                pattern.binds.iter()
                                    .map(|binding| binding.1)
                            }).collect(),
                    );
                    let (clause_label, guard_ret) = clause.guard.lower(
                        b, env, exc_stack, clause_label);
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
                    b.basic_op(leaf_body_label,
                               lir::OpKind::TombstoneSSA(case_structure_ssa),
                               vec![], vec![]);

                    let (ret_label, clause_ret_ssa) = clause.body.lower(
                        b, env, exc_stack, leaf_body_label);
                    b.op_jump(ret_label, done_label);
                    b.add_phi(ret_label, clause_ret_ssa,
                              done_label, self.ssa);
                    b.finish(ret_label);

                    leaves.push(clause_label);
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

                b.basic_op(main_cont,
                           lir::OpKind::CaseStart {
                               vars: val.values.iter().map(|v| v.ssa).collect(),
                               clauses: clauses,
                               value_vars: value_vars,
                           },
                           val.ssa_vars().map(|v| Source::Variable(v)).collect(),
                           vec![case_structure_ssa]);

                // Jump to match body
                b.op_jump(main_cont, match_body_label);
                b.finish(main_cont);

                (done_label, self.ssa)
            },
            HSEK::Tuple(ref elems) => {
                let mut main_cont = in_block;

                for elem in elems.iter() {
                    let (main_cont, elem_ssa) = elem.lower(
                        b, env, exc_stack, main_cont);
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

                let (main_cont, tail_ssa) = tail.lower(
                    b, env, exc_stack, main_cont);
                for elem in head.iter() {
                    let (main_cont, elem_ssa) = elem.lower(
                        b, env, exc_stack, main_cont);
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
                    let (main_cont, key_ssa) = key.lower(
                        b, env, exc_stack, main_cont);
                    let (main_cont, value_ssa) = value.lower(
                        b, env, exc_stack, main_cont);

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
                    let (main_cont, val_ssa) = val.lower(
                        b, env, exc_stack, main_cont);
                    //let val_ssa = fuse_ssa!(val.lower(b, env, exc_stack));

                    reads.push(lir::Source::Variable(val_ssa));
                    for opt in opts {
                        let (main_cont, n_ssa) = opt.lower(
                                b, env, exc_stack, main_cont);
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
                    let (main_cont, n_ssa) = arg.lower(
                            b, env, exc_stack, main_cont);
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

                println!("PrimOp: {}", name);
                for arg in args.iter() {
                    let (main_cont, n_ssa) = arg.lower(
                        b, env, exc_stack, main_cont);
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

                for v in d1.values.iter() {
                    let (main_cont, n_ssa) = v.lower(
                        b, env, exc_stack, main_cont);
                    assert!(v.ssa == n_ssa);
                }

                let (main_cont, ret_ssa) = d2.lower(
                    b, env, exc_stack, main_cont);
                assert!(self.ssa == ret_ssa);

                (main_cont, self.ssa)
            },
            HSEK::Receive { ref clauses, ref timeout_time, ref timeout_body,
                            ref pattern_values } => {
                let mut main_cont = in_block;

                // See lir::OpKind for more detailed documentation
                // (Many comments refer to the documentation over there)

                // Lower match values
                for value in pattern_values {
                    let (main_cont, m_n_ssa) = value.lower(
                        b, env, exc_stack, main_cont);
                    assert!(m_n_ssa == value.ssa);
                }
                let value_vars: Vec<_> = pattern_values.iter().map(|v| v.ssa)
                    .collect();

                let (main_cont, timeout_time_var) = timeout_time.lower(
                    b, env, exc_stack, main_cont);

                let start_label = main_cont; // #start
                let receive_loop_label = b.add_block(); // #receive_loop
                let match_body_label = b.add_block(); // #match_body
                let timeout_body_label = b.add_block(); // #timeout_body
                let expression_exit_label = b.add_block(); // Exit block of match

                let receive_structure_ssa = env.new_ssa();
                let case_structure_ssa = env.new_ssa();

                // Entry to receive structure (#start)
                b.basic_op(start_label,
                           lir::OpKind::ReceiveStart,
                           vec![lir::Source::Variable(timeout_time_var)],
                           vec![receive_structure_ssa]);
                b.add_jump(start_label, receive_loop_label);

                // Receive loop block (#receive_loop)
                b.basic_op(receive_loop_label,
                           lir::OpKind::ReceiveWait,
                           vec![lir::Source::Variable(receive_structure_ssa)],
                           vec![]);
                b.add_jump(receive_loop_label, match_body_label);
                b.add_jump(receive_loop_label, timeout_body_label);

                // Timeout branch
                let mut timeout_body_cont = timeout_body_label;
                b.basic_op(timeout_body_cont,
                           lir::OpKind::CaseValues,
                           vec![lir::Source::Variable(case_structure_ssa)], vec![]);
                b.basic_op(timeout_body_cont,
                           lir::OpKind::TombstoneSSA(case_structure_ssa),
                           vec![], vec![]);
                b.basic_op(timeout_body_cont,
                           lir::OpKind::TombstoneSSA(receive_structure_ssa),
                           vec![], vec![]);
                let (clause_ret_block, clause_ret_ssa) = timeout_body.lower(
                    b, env, exc_stack, timeout_body_cont);
                b.basic_op(clause_ret_block, lir::OpKind::Jump, vec![], vec![]);
                b.add_jump(clause_ret_block, expression_exit_label);
                b.add_phi(clause_ret_block, clause_ret_ssa,
                          expression_exit_label, self.ssa);

                // Match leaves (#message_?_match)
                let mut leaves: Vec<LabelN> = clauses.iter().enumerate()
                    .map(|(idx, clause)| {
                        let clause_label = b.add_block();
                        let mut clause_cont = clause_label;
                        b.basic_op(
                            clause_cont,
                            lir::OpKind::CaseValues,
                            vec![lir::Source::Variable(case_structure_ssa)],
                            clause.patterns.iter()
                                .flat_map(|pattern| {
                                    pattern.binds.iter()
                                        .map(|binding| binding.1)
                                }).collect(),
                        );

                        // Guard ret
                        let (clause_cont, guard_ret) = clause.guard
                            .lower(b, env, exc_stack, clause_cont);

                        // Guard ret check
                        b.basic_op(
                            clause_cont,
                            lir::OpKind::IfTruthy,
                            vec![lir::Source::Variable(guard_ret)],
                            vec![]
                        );
                        let guard_fail_label = b.add_block();
                        let leaf_body_label = b.add_block();
                        b.add_jump(clause_cont, leaf_body_label);
                        b.add_jump(clause_cont, guard_fail_label);

                        // Guard fail
                        b.basic_op(guard_fail_label,
                                   lir::OpKind::CaseGuardFail { clause_num: idx },
                                   vec![lir::Source::Variable(case_structure_ssa)],
                                   vec![]);
                        b.add_jump(guard_fail_label, match_body_label);

                        // Leaf body
                        b.basic_op(leaf_body_label,
                                   lir::OpKind::CaseGuardOk,
                                   vec![lir::Source::Variable(case_structure_ssa)],
                                   vec![]);
                        b.basic_op(leaf_body_label,
                                   lir::OpKind::TombstoneSSA(case_structure_ssa),
                                   vec![], vec![]);

                        b.basic_op(leaf_body_label,
                                   lir::OpKind::ReceiveFinish,
                                   vec![lir::Source::Variable(receive_structure_ssa)],
                                   vec![]);
                        b.basic_op(leaf_body_label,
                                   lir::OpKind::TombstoneSSA(receive_structure_ssa),
                                   vec![], vec![]);

                        let (clause_done_label, clause_ret_ssa) = clause.body.lower(
                            b, env, exc_stack, leaf_body_label);
                        b.basic_op(clause_done_label,
                                   lir::OpKind::Jump, vec![], vec![]);
                        b.add_jump(clause_done_label, expression_exit_label);
                        b.add_phi(clause_done_label, clause_ret_ssa,
                                  expression_exit_label, self.ssa);

                        clause_label
                    }).collect();
                leaves.insert(0, receive_loop_label);

                // Match body (#match_body)
                let message_ssa = env.new_ssa();
                b.basic_op(match_body_label,
                           lir::OpKind::ReceiveGetMessage,
                           vec![lir::Source::Variable(receive_structure_ssa)],
                           vec![message_ssa]);
                let mut clauses: Vec<_> = clauses.iter().map(|c| {
                    assert!(c.patterns.len() == 1);
                    lir::Clause {
                        patterns: c.patterns.clone(),
                    }
                }).collect();
                b.basic_op(
                    match_body_label,
                    lir::OpKind::Case(clauses.len())/* {
                        vars: vec![message_ssa],
                        clauses: clauses,
                        value_vars: value_vars,
                    }*/,
                    vec![lir::Source::Variable(message_ssa)],
                    vec![case_structure_ssa]
                );
                for leaf in leaves.iter() {
                    b.add_jump(match_body_label, *leaf);
                }

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
            HSEK::BindClosures { closures: ref _closures, lambda_env, ref body, env_ssa } => {
                let mut main_cont = in_block;

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
                    env_read_vars, vec![env_ssa]
                );
                let (main_cont, ret_ssa) = body.lower(b, env, exc_stack, main_cont);
                assert!(self.ssa == ret_ssa);
                (main_cont, self.ssa)
            },
            ref s => panic!("Unhandled: {:?}", s),
        }
    }
}
