use std::cell::RefCell;
use std::rc::Rc;

use ::{ SSAVariable, LabelN, Atom, LambdaEnvIdx, FunctionIdent, Source,
        AtomicLiteral, OpKind, BoundLambdaEnv, BasicBlock, Module };
use ::term::{ Term, TermType, Pid };
use ::vm::VMState;
use ::module::ModuleType;
use ::pattern::CaseContext;
use ::receive::ReceiveContext;

use super:: { StackFrame, BlockResult, CallOutcomes, CallReturn };

fn unpack_value_list(val_list: Term) -> Vec<Term> {
    if val_list.get_type() == TermType::ValueList {
        if let Term::ValueList(list) = val_list {
            return list.clone();
        }
        unreachable!()
    } else {
        return vec![val_list];
    }
}

impl StackFrame {

    pub fn exec_block(&mut self, module: &Module, block: &BasicBlock) -> BlockResult {

        // Apply phi nodes
        for phi in &block.phi_nodes {
            let source = &phi.entries.iter()
                .find(|(label, _)| label == self.prev_basic_block.as_ref().unwrap())
                .unwrap().1;
            let term = self.read(source);
            self.write(phi.ssa, term);
        }

        let mut block_ret: Option<BlockResult> = None;
        for op in &block.ops {
            assert!(block_ret.is_none());
            match op.kind {
                OpKind::Arguments => {
                    assert!(op.reads.len() == 0);
                    assert!(op.writes.iter().all(|w| self.variables.contains_key(w)))
                }
                OpKind::UnpackValueList => {
                    assert!(op.reads.len() == 1);
                    let res = self.read(&op.reads[0]);
                    let unpacked = unpack_value_list(res);
                    assert!(unpacked.len() == op.writes.len());
                    for (var, val) in op.writes.iter().zip(unpacked) {
                        self.write(*var, val);
                    }
                }
                OpKind::UnpackEnv => {
                    assert!(op.reads.len() == 1);
                    let env = self.read(&op.reads[0]);
                    assert!(env.get_type() == TermType::LambdaEnv);
                    if let Term::LambdaEnv(data) = env {
                        assert!(data.vars.len() == op.writes.len());
                        for (var, term) in op.writes.iter().zip(data.vars) {
                            self.write(*var, term);
                        }
                    }
                }
                OpKind::PackValueList => {
                    assert!(op.writes.len() == 1);
                    let reads_val: Vec<_> = op.reads.iter()
                        .map(|r| self.read(r))
                        .collect();
                    self.write(op.writes[0], Term::ValueList(reads_val));
                }
                OpKind::Move => {
                    assert!(op.reads.len() == 1);
                    assert!(op.writes.len() == 1);
                    let res = self.read(&op.reads[0]);
                    self.write(op.writes[0], res);
                }
                OpKind::Call { tail_call } => {
                    assert!(op.reads.len() >= 2);
                    if tail_call {
                        assert!(op.writes.len() == 0);
                    } else {
                        assert!(op.writes.len() == 2);
                    }

                    let module_term = self.read(&op.reads[0]);
                    let fun_term = self.read(&op.reads[1]);
                    let args: Vec<Term> = op.reads[2..].iter()
                        .map(|arg| self.read(arg)).collect();

                    let module_atom = module_term.as_atom().unwrap();
                    let fun_atom = fun_term.as_atom().unwrap();

                    if tail_call {
                        block_ret = Some(BlockResult::TailCall {
                            module: module_atom,
                            fun: fun_atom,
                            lambda: None,
                            args: args,
                        });
                    } else {
                        let outcomes = CallOutcomes {
                            ret_ok_slot: 0,
                            ret_ok_ssa: op.writes[0],
                            ret_ok_label: None,
                            ret_throw_slot: 1,
                            ret_throw_ssa: op.writes[1],
                            ret_throw_label: None,
                        };

                        block_ret = Some(BlockResult::Call {
                            module: module_atom,
                            fun: fun_atom,
                            lambda: None,
                            args: args,
                            outcomes: outcomes,
                        });
                    }
                }
                OpKind::ReturnOk => {
                    assert!(op.reads.len() == 1);
                    assert!(op.writes.len() == 0);
                    block_ret = Some(BlockResult::Return {
                        ret: CallReturn::Return { term: self.read(&op.reads[0]) },
                    });
                }
                OpKind::CaptureNamedFunction(ref ident) => {
                    assert!(op.reads.len() == 0);
                    assert!(op.writes.len() == 1);
                    assert!(ident.lambda.is_none());
                    let res = Term::CapturedFunction {
                        module: module.name.clone(),
                        fun_name: ident.name.clone(),
                        arity: ident.arity,
                    };
                    self.write(op.writes[0], res);
                }
                OpKind::Apply { tail_call } => {
                    assert!(op.reads.len() >= 1);
                    if tail_call {
                        assert!(op.writes.len() == 0);
                    } else {
                        assert!(op.writes.len() == 2);
                    }

                    let fun_var = self.read(&op.reads[0]);
                    let args: Vec<Term> = op.reads[1..].iter()
                        .map(|arg| self.read(arg)).collect();

                    let outcomes = if tail_call {
                        None
                    } else {
                        Some(CallOutcomes {
                            ret_ok_slot: 0,
                            ret_ok_ssa: op.writes[0],
                            ret_ok_label: None,
                            ret_throw_slot: 1,
                            ret_throw_ssa: op.writes[1],
                            ret_throw_label: None,
                        })
                    };

                    match fun_var {
                        Term::CapturedFunction { module: ref module_a,
                                                 ref fun_name, ref arity } => {
                            assert!(args.len() == *arity as usize);

                            if tail_call {
                                block_ret = Some(BlockResult::TailCall {
                                    module: module_a.clone(),
                                    fun: fun_name.clone(),
                                    lambda: None,
                                    args: args,
                                });
                            } else {
                                block_ret = Some(BlockResult::Call {
                                    module: module_a.clone(),
                                    fun: fun_name.clone(),
                                    lambda: None,
                                    args: args,
                                    outcomes: outcomes.unwrap(),
                                });
                            }
                        }
                        Term::BoundLambda {
                            module: ref module_a, ref fun_name, arity,
                            lambda, ref bound_env } => {

                            let mut rargs = vec![Term::LambdaEnv(bound_env.clone())];
                            rargs.extend(args.iter().cloned());

                            assert!(rargs.len() == (arity as usize));

                            if tail_call {
                                block_ret = Some(BlockResult::TailCall {
                                    module: module_a.clone(),
                                    fun: fun_name.clone(),
                                    lambda: Some(lambda),
                                    args: rargs,
                                });
                            } else {
                                block_ret = Some(BlockResult::Call {
                                    module: module_a.clone(),
                                    fun: fun_name.clone(),
                                    lambda: Some(lambda),
                                    args: rargs,
                                    outcomes: outcomes.unwrap(),
                                });
                            }
                        }
                        _ => panic!("Var is not callable"),
                    }
                }
                OpKind::MakeClosureEnv { ref env_idx } => {
                    assert!(op.reads.len() > 0);
                    assert!(op.writes.len() == 1);

                    let env_vars: Vec<Term> = op.reads.iter()
                        .map(|r| self.read(r))
                        .collect();

                    self.write(op.writes[0], Term::LambdaEnv(BoundLambdaEnv {
                        env: *env_idx,
                        vars: env_vars,
                    }));
                }
                OpKind::BindClosure { ref ident } => {
                    assert!(op.reads.len() == 1);
                    assert!(op.writes.len() == 1);

                    let lenv = if let Term::LambdaEnv(bound_env)
                        = self.read(&op.reads[0]) {
                        bound_env
                    } else {
                        panic!();
                    };

                    self.write(op.writes[0], Term::BoundLambda {
                        module: module.name.clone(),
                        fun_name: ident.name.clone(),
                        arity: ident.arity,
                        lambda: ident.lambda.unwrap(),
                        bound_env: lenv.clone(),
                    });
                }
                OpKind::CaseStart { ref clauses, .. } => {
                    let term = self.read(&op.reads[0]);
                    let vals = unpack_value_list(term);
                    let case_ctx = CaseContext::new(vals, clauses.clone());

                    self.write(op.writes[0], Term::CaseContext(
                        Rc::new(RefCell::new(case_ctx))));
                }
                OpKind::Case(_) => {
                    let to_leaf = {
                        let curr = self.read(&op.reads[0]);
                        let mut ctx = if let Term::CaseContext(ref ctx) = curr {
                            ctx.borrow_mut()
                        } else {
                            panic!("Case read not case context");
                        };

                        ctx.do_body()
                    };
                    block_ret = Some(BlockResult::Branch { slot: to_leaf });
                }
                OpKind::CaseGuardFail { clause_num } => {
                    let curr = self.read(&op.reads[0]);
                    let mut ctx = if let Term::CaseContext(ref ctx) = curr {
                        ctx.borrow_mut()
                    } else {
                        panic!("Case read not case context");
                    };

                    ctx.guard_fail(clause_num);
                }
                OpKind::CaseValues => {
                    let mut vals = {
                        let curr = self.read(&op.reads[0]);
                        let mut ctx = if let Term::CaseContext(ref ctx) = curr {
                            ctx.borrow_mut()
                        } else {
                            panic!("case read not case context");
                        };

                        ctx.case_values()
                    };
                    for write in &op.writes {
                        self.write(*write, vals.remove(write).unwrap());
                    }
                }
                OpKind::CaseGuardOk => {
                    let curr = self.read(&op.reads[0]);
                    let mut ctx = if let Term::CaseContext(ref ctx) = curr {
                        ctx.borrow_mut()
                    } else {
                        panic!("case read not case context");
                    };
                    ctx.guard_ok();
                }
                OpKind::ReceiveStart => {
                    assert!(op.reads.len() == 1);
                    let timeout_term = self.read(&op.reads[0]);
                    let receive_ctx = ReceiveContext::new(timeout_term);

                    self.write(op.writes[0], Term::ReceiveContext(
                        Rc::new(RefCell::new(receive_ctx))));
                }
                OpKind::ReceiveWait => {
                    let curr = self.read(&op.reads[0]);
                    let mut ctx = if let Term::ReceiveContext(ref ctx) = curr {
                        ctx.borrow_mut()
                    } else {
                        panic!("Receive read not receive context");
                    };

                    block_ret = Some(BlockResult::Suspend);
                }
                OpKind::Jump => {
                    block_ret = Some(BlockResult::Branch { slot: 0 });
                }
                OpKind::IfTruthy => {
                    let false_atom = &*core_erlang_compiler::intern::FALSE;
                    let matched = match self.read(&op.reads[0]) {
                        Term::Atom(ref atom) if atom == false_atom => false,
                        _ => true,
                    };

                    if matched {
                        block_ret = Some(BlockResult::Branch { slot: 0 });
                    } else {
                        block_ret = Some(BlockResult::Branch { slot: 1 });
                    }
                }
                OpKind::TombstoneSSA(ssa) => {
                    self.tombstone(ssa);
                }
                OpKind::MakeTuple => {
                    let term = Term::Tuple(
                        op.reads.iter().map(|r| self.read(r)).collect()
                    );
                    self.write(op.writes[0], term);
                }
                OpKind::MakeList => {
                    assert!(op.reads.len() >= 1);
                    assert!(op.writes.len() == 1);
                    let tail = self.read(&op.reads[0]);
                    let mut front: Vec<_> = op.reads.iter()
                        .skip(1)
                        .map(|r| self.read(r))
                        .collect();

                    // Just merge lists for prettyness
                    if let Term::List(i_head, i_tail) = tail {
                        front.extend(i_head);
                        self.write(op.writes[0], Term::List(front, i_tail));
                    } else {
                        self.write(op.writes[0], Term::List(front, Box::new(tail)));
                    }
                }
                _ => {
                    println!("Unimpl: {:?}", op);
                    println!("Variables: {:?}", self.variables);
                    unimplemented!()
                }
            }
        }

        return block_ret.unwrap();
    }

}
