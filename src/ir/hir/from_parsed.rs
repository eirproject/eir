use ::std::collections::HashSet;

use ::{ Atom, Variable };
use ::ir::{ AVariable, AFunctionName, SSAVariable, Module, FunctionDefinition,
            FunctionVisibility, FunctionIdent };
use ::ir::hir::{ Expression, SingleExpression, SingleExpressionKind,
                 Function, Pattern, PatternNode, Closure };

impl Module {
    fn from_parsed(module: &::parser::Module) -> Self {
        let exported: HashSet<(Atom, u32)> = module.declarations.iter()
            .map(|f| (f.name.clone(), f.arity)).collect();
        Module {
            name: module.name.clone(),
            attributes: module.attributes.clone(),
            functions: module.definitions.iter()
                .map(|f| {
                    let name = f.name.0.name.clone();
                    let arity = f.name.0.arity;
                    FunctionDefinition {
                        visibility: if exported.contains(&(name.clone(), arity)) {
                            FunctionVisibility::Public
                        } else {
                            FunctionVisibility::Private
                        },
                        ident: FunctionIdent {
                            name: name,
                            arity: arity,
                            lambda: None,
                        },
                        hir_fun: ::ir::hir::Function::from_parsed(&f.fun.0),
                        lir_function: None,
                    }
                }).collect(),
        }
    }
}

impl Function {
    fn from_parsed(fun: &::parser::Function) -> Self {
        Function {
            args: fun.vars.iter().map(|a| AVariable::new(a.0.clone())).collect(),
            body: SingleExpression::from_parsed(&fun.body),
        }
    }
}

impl Expression {
    fn from_parsed(fun: &::parser::Expression) -> Expression {
        Expression {
            values: fun.0.iter()
                .map(|v| SingleExpression::from_parsed_single(&v.0))
                .collect(),
        }
    }
}

fn pat_node_from_parsed(node: &::parser::Pattern) -> PatternNode {
    use ::parser::Pattern as PP;
    match *node {
        PP::Variable(ref v) => PatternNode::Variable(v.clone()),
        PP::Atomic(ref a) => PatternNode::Atomic(a.clone()),
        PP::Bind(ref var, ref pat) =>
            PatternNode::Bind(var.clone(), Box::new(pat_node_from_parsed(&pat.0))),
        PP::Tuple(ref pats) =>
            PatternNode::Tuple(
                pats.iter().map(|p| pat_node_from_parsed(&p.0)).collect()
            ),
        PP::List(ref pats, ref tail) =>
            PatternNode::List(
                pats.iter().map(|p| pat_node_from_parsed(&p.0)).collect(),
                Box::new(pat_node_from_parsed(&tail.0))
            ),
    }
}

impl Pattern {
    fn from_parsed(pat: &::parser::Pattern) -> Self {
        let node = pat_node_from_parsed(pat);
        let mut bindings = Vec::new();
        node.collect_bindings(&mut bindings);
        Pattern {
            bindings: bindings.iter().map(|v| (v.clone(), SSAVariable(0))).collect(),
            node: node,
        }
    }
}

use ::parser::SingleExpression as PSE;
impl SingleExpression {
    fn from_parsed_single(expr: &PSE) -> SingleExpression {
        let kind = match *expr {
            PSE::Variable(ref v) =>
                SingleExpressionKind::Variable(AVariable::new(v.clone())),
            PSE::FunctionName(ref f) => {
                SingleExpressionKind::NamedFunction{
                    name: AFunctionName::new(f.clone()),
                    is_lambda: false,
                }
            },

            PSE::AtomicLiteral(ref a) => SingleExpressionKind::Atomic(a.clone()),
            PSE::InterModuleCall { ref module, ref name, ref args } =>
                SingleExpressionKind::InterModuleCall {
                    module: Box::new(SingleExpression::from_parsed(&module)),
                    name: Box::new(SingleExpression::from_parsed(&name)),
                    args: args.iter()
                        .map(|a| SingleExpression::from_parsed(a))
                        .collect(),
                },
            PSE::Let { ref vars, ref val, ref body } =>
                SingleExpressionKind::Let {
                    vars: vars.iter().map(|v| AVariable::new(v.0.clone())).collect(),
                    val: Expression::from_parsed(val),
                    body: Box::new(SingleExpression::from_parsed(body)),
                },
            PSE::ApplyCall { ref fun, ref args } =>
                SingleExpressionKind::ApplyCall {
                    fun: Box::new(SingleExpression::from_parsed(fun)),
                    args: args.iter()
                        .map(|v| SingleExpression::from_parsed(v))
                        .collect(),
                },
            PSE::Catch(ref body) => {
                let r = AVariable { var: Variable::from("_r"),
                                    ssa: SSAVariable(0) };

                let typ = AVariable { var: Variable::from("_t"),
                                      ssa: SSAVariable(0) };
                let kind = AVariable { var: Variable::from("_k"),
                                       ssa: SSAVariable(0) };
                let extra = AVariable { var: Variable::from("_e"),
                                        ssa: SSAVariable(0) };

                SingleExpressionKind::Try {
                    body: Expression::from_parsed(body),
                    then_vars: vec![r.clone()],
                    then: Box::new(SingleExpression {
                        ssa: SSAVariable(0),
                        kind: SingleExpressionKind::Variable(r),
                    }),
                    catch_vars: vec![typ.clone(), kind.clone(), extra.clone()],
                    catch: Box::new(SingleExpression {
                        ssa: SSAVariable(0),
                        kind: SingleExpressionKind::Case {
                            val: Expression {
                                values: vec![
                                    SingleExpression {
                                        ssa: SSAVariable(0),
                                        kind: SingleExpressionKind::Variable(typ.clone()),
                                    }
                                ],
                            },
                            clauses: vec![
                                // TODO
                            ],
                        },
                    }),
                }
            },
            PSE::Case { ref val, ref clauses } => {
                SingleExpressionKind::Case {
                    val: Expression::from_parsed(val),
                    clauses: clauses.iter()
                        .map(|c| {
                            ::ir::hir::Clause {
                                patterns: c.0.patterns.iter()
                                    .map(|p| Pattern::from_parsed(&p.0)).collect(),
                                guard: SingleExpression::from_parsed(&c.0.guard),
                                body: SingleExpression::from_parsed(&c.0.body),
                            }
                        }).collect(),
                }
            },
            PSE::Tuple(ref items) => {
                SingleExpressionKind::Tuple(
                    items.iter()
                        .map(|i| SingleExpression::from_parsed(i))
                        .collect())
            },
            PSE::List { ref head, ref tail } => {
                SingleExpressionKind::List {
                    head: head.iter()
                        .map(|i| SingleExpression::from_parsed(i))
                        .collect(),
                    tail: Box::new(SingleExpression::from_parsed(tail)),
                }
            },
            PSE::PrimOpCall(ref op) => {
                SingleExpressionKind::PrimOp {
                    name: op.name.clone(),
                    args: op.args.iter()
                        .map(|a| SingleExpression::from_parsed(a))
                        .collect(),
                }
            },
            PSE::Do(ref d1, ref d2) => {
                SingleExpressionKind::Do(
                    Expression::from_parsed(d1),
                    Box::new(SingleExpression::from_parsed(d2))
                )
            },
            PSE::Try { ref body, ref catch_vars, ref catch,
                       ref then_vars, ref then } => {
                SingleExpressionKind::Try {
                    body: Expression::from_parsed(body),
                    then_vars: then_vars.iter().map(|v| {
                        AVariable {
                            ssa: SSAVariable(0),
                            var: v.0.clone(),
                        }
                    }).collect(),
                    then: Box::new(SingleExpression::from_parsed(then)),
                    catch_vars: catch_vars.iter().map(|v| {
                        AVariable {
                            ssa: SSAVariable(0),
                            var: v.0.clone(),
                        }
                    }).collect(),
                    catch: Box::new(SingleExpression::from_parsed(catch)),
                }
            },
            PSE::Receive { ref clauses, ref timeout_time, ref timeout_body } => {
                SingleExpressionKind::Receive {
                    clauses: clauses.iter().map(|c| {
                        ::ir::hir::Clause {
                            patterns: c.0.patterns.iter()
                                .map(|p| Pattern::from_parsed(&p.0)).collect(),
                            guard: SingleExpression::from_parsed(&c.0.guard),
                            body: SingleExpression::from_parsed(&c.0.body),
                        }
                    }).collect(),
                    timeout_time: Box::new(SingleExpression::from_parsed(
                        timeout_time)),
                    timeout_body: Box::new(SingleExpression::from_parsed(
                        timeout_body)),
                }
            },
            PSE::Fun(ref fun) => {
                SingleExpressionKind::BindClosure {
                    closure: Closure {
                        alias: None,
                        ident: None,
                        fun: Some(Box::new(Function::from_parsed(fun))),
                        env: None,
                    },
                    lambda_env: None,
                    env_ssa: SSAVariable(0),
                }
            },
            PSE::LetRec { ref funs, ref body } => {
                SingleExpressionKind::BindClosures {
                    closures: funs.iter().map(|f| {
                        Closure {
                            alias: Some(AFunctionName::new(f.0.clone())),
                            ident: None,
                            fun: Some(Box::new(Function::from_parsed(&f.1))),
                            env: None,
                        }
                    }).collect(),
                    body: Box::new(SingleExpression::from_parsed(body)),
                    lambda_env: None,
                    env_ssa: SSAVariable(0),
                }
            },
            ref e => panic!("Unhandled: {:?}", e),
        };
        SingleExpression {
            ssa: SSAVariable(0),
            kind,
        }
    }
    fn from_parsed(fun: &::parser::Expression) -> SingleExpression {
        assert!(fun.0.len() == 1);
        SingleExpression::from_parsed_single(&fun.0[0].0)
    }
}

pub fn from_parsed(parsed: &::parser::Module) -> Module {
    Module::from_parsed(parsed)
}
