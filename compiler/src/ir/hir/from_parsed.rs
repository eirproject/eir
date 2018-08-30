use ::std::collections::HashSet;

use ::{ Atom, Variable };
use ::ir::{ AVariable, AFunctionName, Module, FunctionDefinition,
            FunctionVisibility, FunctionIdent };
use ::ir::hir::{ Expression, SingleExpression, SingleExpressionKind,
                 Function, Pattern, PatternNode, Closure };
use ::util::ssa_variable::INVALID_SSA;

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
                        lambda_env_idx: None,
                    }
                }).collect(),
            lambda_envs: None,
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

fn pat_node_from_parsed(node: &::parser::Pattern,
                        values: &mut Vec<SingleExpression>) -> PatternNode {

    let wildcard: ::parser::Variable = Atom::from("_");

    use ::parser::Pattern as PP;
    match *node {
        PP::Atomic(ref a) => PatternNode::Atomic(a.clone()),
        PP::Wildcard => PatternNode::Wildcard,
        PP::BindVar(ref var, ref pat) if *var == wildcard =>
            pat_node_from_parsed(&pat.0, values),
        PP::BindVar(ref var, ref pat) =>
            PatternNode::BindVar(var.clone(), Box::new(
                pat_node_from_parsed(&pat.0, values))),
        PP::Binary(ref elems) => {
            PatternNode::Binary(
                elems.iter().map(|(pat, opts)| {
                    let opts_ids: Vec<_> = opts.iter().map(|o| {
                        let curr_val_num = values.len();
                        values.push(SingleExpression::from_parsed_single(&o.0));
                        curr_val_num
                    }).collect();
                    (pat_node_from_parsed(&pat.0, values), opts_ids)
                }).collect(),
            )
        },
        PP::Tuple(ref pats) =>
            PatternNode::Tuple(
                pats.iter().map(|p| pat_node_from_parsed(&p.0, values)).collect()
            ),
        PP::List(ref pats, ref tail) =>
            PatternNode::List(
                pats.iter().map(|p| pat_node_from_parsed(&p.0, values)).collect(),
                Box::new(pat_node_from_parsed(&tail.0, values))
            ),
        PP::Map(ref kvs) => {
            PatternNode::Map(
                kvs.iter().map(|kv| {
                    let curr_val_num = values.len();
                    values.push(SingleExpression::from_parsed_single(&kv.0));
                    (
                        curr_val_num,
                        Box::new(pat_node_from_parsed(&(kv.1).0, values))
                    )
                }).collect(),
            )
        },
    }
}

impl Pattern {
    fn from_parsed(pat: &::parser::Pattern,
                   values: &mut Vec<SingleExpression>) -> Self {
        let node = pat_node_from_parsed(pat, values);

        let binds = node.get_bind_vars();

        Pattern {
            binds: binds.iter().map(|v| (v.clone(), INVALID_SSA)).collect(),
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
                SingleExpressionKind::NamedFunction {
                    name: AFunctionName::new(f.clone()),
                    is_lambda: false,
                }
            },
            PSE::ExternalFunctionName { ref module, ref name } => {
                SingleExpressionKind::ExternalNamedFunction {
                    module: module.clone(),
                    name: AFunctionName::new(name.clone()),
                }
            }

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
                // TODO: WTF?
                let r = AVariable { var: Variable::from("_r"),
                                    ssa: INVALID_SSA };

                let typ = AVariable { var: Variable::from("_t"),
                                      ssa: INVALID_SSA };
                let kind = AVariable { var: Variable::from("_k"),
                                       ssa: INVALID_SSA };
                let extra = AVariable { var: Variable::from("_e"),
                                        ssa: INVALID_SSA };

                SingleExpressionKind::Try {
                    body: Expression::from_parsed(body),
                    then_vars: vec![r.clone()],
                    then: Box::new(SingleExpression {
                        ssa: INVALID_SSA,
                        kind: SingleExpressionKind::Variable(r),
                    }),
                    catch_vars: vec![typ.clone(), kind.clone(), extra.clone()],
                    catch: Box::new(SingleExpression {
                        ssa: INVALID_SSA,
                        kind: SingleExpressionKind::Case {
                            val: Expression {
                                values: vec![
                                    SingleExpression {
                                        ssa: INVALID_SSA,
                                        kind: SingleExpressionKind::Variable(typ.clone()),
                                    }
                                ],
                            },
                            values: vec![],
                            clauses: vec![
                                // TODO
                            ],
                        },
                    }),
                }
            },
            PSE::Case { ref val, ref clauses } => {
                //let cfg = pattern::match_from_parsed(val.0.len(), clauses.as_slice());
                let mut values = Vec::new();

                SingleExpressionKind::Case {
                    val: Expression::from_parsed(val),
                    clauses: clauses.iter()
                        .map(|c| {
                            ::ir::hir::Clause {
                                patterns: c.0.patterns.iter()
                                    .map(|p| {
                                         Pattern::from_parsed(&p.0, &mut values)
                                    }).collect(),
                                guard: SingleExpression::from_parsed(&c.0.guard),
                                body: SingleExpression::from_parsed(&c.0.body),
                            }
                        }).collect(),
                    values: values,
                    //cfg: cfg,
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
                            ssa: INVALID_SSA,
                            var: v.0.clone(),
                        }
                    }).collect(),
                    then: Box::new(SingleExpression::from_parsed(then)),
                    catch_vars: catch_vars.iter().map(|v| {
                        AVariable {
                            ssa: INVALID_SSA,
                            var: v.0.clone(),
                        }
                    }).collect(),
                    catch: Box::new(SingleExpression::from_parsed(catch)),
                }
            },
            PSE::Receive { ref clauses, ref timeout_time, ref timeout_body } => {
                let mut values = Vec::new();
                SingleExpressionKind::Receive {
                    clauses: clauses.iter().map(|c| {
                        ::ir::hir::Clause {
                            patterns: c.0.patterns.iter()
                                .map(|p| {
                                    Pattern::from_parsed(&p.0, &mut values)
                                }).collect(),
                            guard: SingleExpression::from_parsed(&c.0.guard),
                            body: SingleExpression::from_parsed(&c.0.body),
                        }
                    }).collect(),
                    pattern_values: values,
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
                    env_ssa: INVALID_SSA,
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
                    env_ssa: INVALID_SSA,
                }
            },
            PSE::Map(ref kv, ref merge) => {
                let kv_h = kv.iter()
                    .map(|&(ref k, ref v)| {
                        (SingleExpression::from_parsed(k),
                         SingleExpression::from_parsed(v))
                    }).collect();
                let merge = merge.as_ref().map(
                    |v| Box::new(SingleExpression::from_parsed(&v)));
                SingleExpressionKind::Map {
                    values: kv_h,
                    merge: merge,
                }
            },
            PSE::Binary(ref elems) => {
                SingleExpressionKind::Binary(
                    elems.iter().map(|(ref value, ref opts)| {
                        (SingleExpression::from_parsed(value),
                         opts.iter().map(|o| SingleExpression::from_parsed(o)).collect())
                    }).collect()
                )
            },
            //ref e => panic!("Unhandled: {:?}", e),
        };
        SingleExpression {
            ssa: INVALID_SSA,
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
