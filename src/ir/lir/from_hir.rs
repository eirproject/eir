use ::ir::{ Module, FunctionDefinition };
use ::ir::SSAVariable;
use ::ir::hir;
use ::ir::lir;
use ::ir::lir::Source;

pub fn do_lower(module: &mut Module) {
    module.lower()
}

impl Module {
    fn lower(&mut self) {
        for fun in &mut self.functions {
            fun.lower();
        }
    }
}

impl FunctionDefinition {
    fn lower(&mut self) {
        let mut builder = lir::LirBuilder::new();
        let ret = self.hir_fun.body.lower(&mut builder);
        builder.basic_op(
            lir::OpKind::ReturnOk,
            vec![lir::Source::Variable(ret)], vec![]);

        self.lir_function = Some(builder.build());
    }
}

use self::hir::SingleExpressionKind as HSEK;
impl hir::SingleExpression {
    fn lower(&self, b: &mut lir::LirBuilder) -> ::ir::SSAVariable {
        match self.kind {
            HSEK::InterModuleCall { ref module, ref name, ref args } => {
                let mut reads_r = vec![module.lower(b), name.lower(b)];
                reads_r.extend(args.iter().map(|a| a.lower(b)));
                let reads: Vec<_> = reads_r.iter()
                    .map(|r| lir::Source::Variable(*r))
                    .collect();

                b.basic_op(lir::OpKind::Call, reads, vec![self.ssa]);
                let prev_block = b.get_block();

                let throw_block = b.add_block();
                b.set_block(throw_block);
                b.basic_op(lir::OpKind::ReturnThrow, vec![], vec![]);

                let resume_block = b.add_block();
                b.set_block(resume_block);

                b.add_jump(prev_block, resume_block);
                b.add_jump(prev_block, throw_block);

                self.ssa
            },
            HSEK::ApplyCall { ref fun, ref args } => {
                let mut reads_r = vec![fun.lower(b)];
                reads_r.extend(args.iter().map(|a| a.lower(b)));
                let reads: Vec<_> = reads_r.iter()
                    .map(|r| lir::Source::Variable(*r))
                    .collect();

                b.basic_op(lir::OpKind::Apply, reads, vec![self.ssa]);
                let prev_block = b.get_block();

                let throw_block = b.add_block();
                b.set_block(throw_block);
                b.basic_op(lir::OpKind::ReturnThrow, vec![], vec![]);

                let resume_block = b.add_block();
                b.set_block(resume_block);

                b.add_jump(prev_block, resume_block);
                b.add_jump(prev_block, throw_block);

                self.ssa
            },
            HSEK::Atomic(ref atomic) => {
                b.basic_op(
                    lir::OpKind::Move,
                    vec![lir::Source::Constant(atomic.clone())], vec![self.ssa]);
                self.ssa
            },
            HSEK::NamedFunction { ref name, is_lambda } => {
                if is_lambda {
                    self.ssa
                } else {
                    let n = ::ir::FunctionIdent {
                        name: name.var.name.clone(),
                        arity: name.var.arity,
                        lambda: None,
                    };
                    b.basic_op(
                        lir::OpKind::CaptureNamedFunction(n),
                        vec![], vec![self.ssa]);
                    self.ssa
                }
            }
            HSEK::Variable(_) => {
                self.ssa
            },
            HSEK::Let { ref val, ref body, .. } => {
                for v in val.values.iter() {
                    v.lower(b);
                }
                body.lower(b);
                self.ssa
            },
            HSEK::Try { ref body, ref then,
                        ref catch_vars, ref catch, .. } => {
                for expr in body.values.iter() {
                    expr.lower(b);
                }
                then.lower(b);
                then.ssa
            },
            HSEK::Case { ref val, ref clauses } => {
                for v in &val.values {
                    v.lower(b);
                }

                b.basic_op(
                    lir::OpKind::Case {
                        vars: val.values.iter().map(|v| v.ssa).collect(),
                        clauses: clauses.iter().map(|c| {
                            lir::Clause {
                                patterns: c.patterns.clone(),
                            }
                        }).collect(),
                    },
                    vec![], vec![]);
                let from_label = b.get_block();

                let done_label = b.add_block();

                for clause in clauses.iter() {
                    let clause_label = b.add_block();
                    b.add_jump(from_label, clause_label);
                    b.set_block(clause_label);
                    clause.body.lower(b);

                    b.basic_op(lir::OpKind::Jump, vec![], vec![]);
                    let clause_done_label = b.get_block();
                    b.add_jump(clause_done_label, done_label);
                }

                b.set_block(done_label);

                // TODO
                SSAVariable(0)
            },
            HSEK::Tuple(ref elems) => {
                for elem in elems.iter() {
                    elem.lower(b);
                }

                b.basic_op(lir::OpKind::MakeTuple,
                           elems.iter()
                           .map(|e| lir::Source::Variable(e.ssa))
                           .collect(),
                           vec![self.ssa]);

                self.ssa
            },
            HSEK::List{ ref head, ref tail } => {
                tail.lower(b);
                for elem in head.iter() {
                    elem.lower(b);
                }

                let mut reads = vec![lir::Source::Variable(tail.ssa)];
                reads.extend(head.iter()
                             .map(|v| lir::Source::Variable(v.ssa)));

                b.basic_op(lir::OpKind::MakeList,
                           reads, vec![self.ssa]);

                self.ssa
            },
            HSEK::PrimOp { ref name, ref args } => {
                for arg in args.iter() {
                    arg.lower(b);
                }
                b.basic_op(
                    lir::OpKind::PrimOp(name.clone()),
                    args.iter().map(|a| lir::Source::Variable(a.ssa)).collect(),
                    vec![]);
                self.ssa
            },
            HSEK::Do(ref d1, ref d2) => {
                for v in d1.values.iter() {
                    v.lower(b);
                }
                d2.lower(b);
                self.ssa
            },
            HSEK::Receive { .. } => {
                // TODO
                self.ssa
            },
            HSEK::BindClosure { ref closure, lambda_env, env_ssa } => {
                // TODO
                b.basic_op(
                    lir::OpKind::MakeClosureEnv {
                        env_idx: lambda_env.unwrap()
                    },
                    vec![], vec![env_ssa]);
                b.basic_op(
                    lir::OpKind::BindClosure {
                        ident: closure.ident.clone().unwrap(),
                    },
                    vec![Source::Variable(env_ssa)],
                    vec![self.ssa]);
                self.ssa
            },
            HSEK::BindClosures { ref closures, lambda_env, ref body, env_ssa } => {
                // TODO
                b.basic_op(
                    lir::OpKind::MakeClosureEnv {
                        env_idx: lambda_env.unwrap()
                    },
                    vec![], vec![env_ssa]);
                body.lower(b);
                self.ssa
            },
            ref s => panic!("Unhandled: {:?}", s),
        }
    }
}
