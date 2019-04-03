use ::std::collections::HashSet;

use ::{ Atom };
use ::ir::{ Module, AVariable, AFunctionName, FunctionDefinition,
            FunctionVisibility, FunctionIdent };
use ::ir::hir::{ SingleExpression, SingleExpressionKind,
                 Function, Pattern, PatternNode, Closure };

use ::ssa::INVALID_SSA;

impl Module {
    fn from_parsed(module: &::parser::Module) -> Self {
        let exported: HashSet<(Atom, usize)> = module.declarations.iter()
            .map(|f| (f.name.clone(), f.arity)).collect();
        Module {
            name: module.name.clone(),
            attributes: module.attributes.clone(),
            functions: module.definitions.iter()
                .map(|f| {
                    let name = f.name.0.name.clone();
                    let arity = f.name.0.arity;
                    let is_visible = exported.contains(&(name.clone(), arity));
                    let fun_ident = FunctionIdent {
                        module: module.name.clone(),
                        name: name,
                        arity: arity,
                        lambda: None,
                    };
                    FunctionDefinition {
                        visibility: if is_visible {
                            FunctionVisibility::Public
                        } else {
                            FunctionVisibility::Private
                        },
                        hir_fun: ::ir::hir::Function::from_parsed(&f.fun.0, &fun_ident),
                        ident: fun_ident,
                        lambda_env_idx: None,
                        eir_fun: None,
                    }
                }).collect(),
            lambda_envs: None,
        }
    }
}

impl Function {
    fn from_parsed(fun: &::parser::Function, fun_ident: &FunctionIdent) -> Self {
        Function {
            args: fun.vars.iter().map(|a| AVariable::new(a.0.clone())).collect(),
            body: SingleExpression::from_parsed(&fun.body, fun_ident),
        }
    }
}

fn pat_node_from_parsed(node: &::parser::Pattern,
                        values: &mut Vec<SingleExpression>,
                        fun_ident: &FunctionIdent) -> PatternNode {

    let wildcard: ::parser::Variable = Atom::from("_");

    use ::parser::Pattern as PP;
    match *node {
        PP::Atomic(ref a) => PatternNode::Atomic(a.clone()),
        PP::Wildcard => PatternNode::Wildcard,
        PP::BindVar(ref var, ref pat) if var.0 == wildcard =>
            pat_node_from_parsed(&pat.0, values, fun_ident),
        PP::BindVar(ref var, ref pat) =>
            PatternNode::BindVar(var.0.clone(), Box::new(
                pat_node_from_parsed(&pat.0, values, fun_ident))),
        PP::Binary(ref elems) => {
            PatternNode::Binary(
                elems.iter().map(|(pat, opts)| {
                    let opts_n = opts.iter().map(|o| {
                        let curr_val_num = values.len();
                        values.push(SingleExpression::from_parsed_single(&o.0, fun_ident));
                        curr_val_num
                    }).collect();
                    (pat_node_from_parsed(&pat.0, values, fun_ident), opts_n)
                }).collect(),
            )
        },
        PP::Tuple(ref pats) =>
            PatternNode::Tuple(
                pats.iter().map(|p| pat_node_from_parsed(&p.0, values, fun_ident)).collect()
            ),
        PP::List(ref pats, ref tail) =>
            PatternNode::List(
                pats.iter().map(|p| pat_node_from_parsed(&p.0, values, fun_ident)).collect(),
                Box::new(pat_node_from_parsed(&tail.0, values, fun_ident))
            ),
        PP::Map(ref kvs) => {
            PatternNode::Map(
                kvs.iter().map(|kv| {
                    let curr_val_num = values.len();
                    values.push(SingleExpression::from_parsed_single(&((kv.0).0).0, fun_ident));
                    (
                        curr_val_num,
                        Box::new(pat_node_from_parsed(&((kv.0).1).0, values, fun_ident))
                    )
                }).collect(),
            )
        },
    }
}

impl Pattern {
    fn from_parsed(pat: &::parser::Pattern,
                   values: &mut Vec<SingleExpression>,
                   fun_ident: &FunctionIdent) -> Self {
        let node = pat_node_from_parsed(pat, values, fun_ident);

        let binds = node.get_bind_vars();

        Pattern {
            binds: binds.iter().map(|v| (v.clone(), INVALID_SSA)).collect(),
            node: node,
        }
    }
}

use ::parser::SingleExpression as PSE;
impl SingleExpression {
    fn from_parsed_single(expr: &PSE, fun_ident: &FunctionIdent) -> SingleExpression {
        let kind = match *expr {
            PSE::Variable(ref v) =>
                SingleExpressionKind::Variable(AVariable::new(v.clone())),
            PSE::FunctionName(ref f) => {
                let ident = FunctionIdent {
                    module: fun_ident.module.clone(),
                    name: f.name.clone(),
                    arity: f.arity,
                    lambda: None,
                };
                SingleExpressionKind::NamedFunction {
                    name: AFunctionName::new(ident),
                    is_lambda: false,
                }
            },
            PSE::ExternalFunctionName { ref module, ref name } => {
                SingleExpressionKind::ExternalNamedFunction {
                    name: AFunctionName::new(name.to_eir(module.clone())),
                }
            }

            PSE::AtomicLiteral(ref a) => SingleExpressionKind::Atomic(a.clone()),
            PSE::InterModuleCall { ref module, ref name, ref args } =>
                SingleExpressionKind::InterModuleCall {
                    module: Box::new(SingleExpression::from_parsed(&module, fun_ident)),
                    name: Box::new(SingleExpression::from_parsed(&name, fun_ident)),
                    args: args.iter()
                        .map(|a| SingleExpression::from_parsed(a, fun_ident))
                        .collect(),
                },
            PSE::Let { ref vars, ref val, ref body } =>
                SingleExpressionKind::Let {
                    vars: vars.iter().map(|v| AVariable::new(v.0.clone())).collect(),
                    val: Box::new(SingleExpression::from_parsed(val, fun_ident)),
                    body: Box::new(SingleExpression::from_parsed(body, fun_ident)),
                },
            PSE::ApplyCall { ref fun, ref args } =>
                SingleExpressionKind::ApplyCall {
                    fun: Box::new(SingleExpression::from_parsed(fun, fun_ident)),
                    args: args.iter()
                        .map(|v| SingleExpression::from_parsed(v, fun_ident))
                        .collect(),
                },
            PSE::Catch(ref body) => {
                SingleExpressionKind::Catch {
                    body: Box::new(SingleExpression::from_parsed(body, fun_ident)),
                }
            },
            PSE::Case { ref val, ref clauses } => {
                //let cfg = pattern::match_from_parsed(val.0.len(), clauses.as_slice());
                let mut values = Vec::new();

                SingleExpressionKind::Case {
                    val: Box::new(SingleExpression::from_parsed(val, fun_ident)),
                    clauses: clauses.iter()
                        .map(|c| {
                            ::ir::hir::Clause {
                                patterns: c.0.patterns.iter()
                                    .map(|p| {
                                         Pattern::from_parsed(&p.0, &mut values, fun_ident)
                                    }).collect(),
                                guard: SingleExpression::from_parsed(&c.0.guard, fun_ident),
                                body: SingleExpression::from_parsed(&c.0.body, fun_ident),
                            }
                        }).collect(),
                    values: values,
                    //cfg: cfg,
                }
            },
            PSE::Tuple(ref items) => {
                SingleExpressionKind::Tuple(
                    items.iter()
                        .map(|i| SingleExpression::from_parsed(i, fun_ident))
                        .collect())
            },
            PSE::List { ref head, ref tail } => {
                SingleExpressionKind::List {
                    head: head.iter()
                        .map(|i| SingleExpression::from_parsed(i, fun_ident))
                        .collect(),
                    tail: Box::new(SingleExpression::from_parsed(tail, fun_ident)),
                }
            },
            PSE::PrimOpCall(ref op) => {
                SingleExpressionKind::PrimOp {
                    name: op.name.0.clone(),
                    args: op.args.iter()
                        .map(|a| SingleExpression::from_parsed(a, fun_ident))
                        .collect(),
                }
            },
            PSE::Do(ref d1, ref d2) => {
                SingleExpressionKind::Do(
                    Box::new(SingleExpression::from_parsed(d1, fun_ident)),
                    Box::new(SingleExpression::from_parsed(d2, fun_ident))
                )
            },
            PSE::Try { ref body, ref catch_vars, ref catch,
                       ref then_vars, ref then } => {
                SingleExpressionKind::Try {
                    body: Box::new(SingleExpression::from_parsed(body, fun_ident)),
                    then_vars: then_vars.iter().map(|v| {
                        AVariable {
                            ssa: INVALID_SSA,
                            var: v.0.clone(),
                        }
                    }).collect(),
                    then: Box::new(SingleExpression::from_parsed(then, fun_ident)),
                    catch_vars: catch_vars.iter().map(|v| {
                        AVariable {
                            ssa: INVALID_SSA,
                            var: v.0.clone(),
                        }
                    }).collect(),
                    catch: Box::new(SingleExpression::from_parsed(catch, fun_ident)),
                }
            },
            PSE::Receive { ref clauses, ref timeout_time, ref timeout_body } => {
                let mut values = Vec::new();
                SingleExpressionKind::Receive {
                    clauses: clauses.iter().map(|c| {
                        ::ir::hir::Clause {
                            patterns: c.0.patterns.iter()
                                .map(|p| {
                                    Pattern::from_parsed(&p.0, &mut values, fun_ident)
                                }).collect(),
                            guard: SingleExpression::from_parsed(&c.0.guard, fun_ident),
                            body: SingleExpression::from_parsed(&c.0.body, fun_ident),
                        }
                    }).collect(),
                    pattern_values: values,
                    timeout_time: Box::new(SingleExpression::from_parsed(
                        timeout_time, fun_ident)),
                    timeout_body: Box::new(SingleExpression::from_parsed(
                        timeout_body, fun_ident)),
                }
            },
            PSE::Fun(ref fun) => {
                SingleExpressionKind::BindClosure {
                    closure: Closure {
                        alias: None,
                        ident: None,
                        parent_ident: fun_ident.clone(),
                        fun: Some(Box::new(Function::from_parsed(fun, fun_ident))),
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
                            alias: Some(AFunctionName::new(
                                f.0.to_eir(fun_ident.module.clone()))),
                            ident: None,
                            parent_ident: fun_ident.clone(),
                            fun: Some(Box::new(Function::from_parsed(&f.1, fun_ident))),
                            env: None,
                        }
                    }).collect(),
                    body: Box::new(SingleExpression::from_parsed(body, fun_ident)),
                    lambda_env: None,
                    env_ssa: INVALID_SSA,
                }
            },
            PSE::Map(ref kv, ref merge) => {
                let kv_h = kv.iter()
                    .map(|&(ref k, assoc, ref v)| {
                        (SingleExpression::from_parsed(k, fun_ident),
                         SingleExpression::from_parsed(v, fun_ident),
                         assoc)
                    }).collect();
                let merge = merge.as_ref().map(
                    |v| Box::new(SingleExpression::from_parsed(&v, fun_ident)));
                SingleExpressionKind::Map {
                    values: kv_h,
                    merge: merge,
                }
            },
            PSE::Binary(ref elems) => {
                SingleExpressionKind::Binary(
                    elems.iter().map(|(ref value, ref opts)| {
                        (SingleExpression::from_parsed(value, fun_ident),
                         opts.iter().map(|o| SingleExpression::from_parsed(o, fun_ident)).collect())
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
    fn from_parsed(fun: &::parser::Expression, fun_ident: &FunctionIdent) -> SingleExpression {
        let values: Vec<_> = fun.0.iter()
            .map(|val| SingleExpression::from_parsed_single(&val.0, fun_ident))
            .collect();
        SingleExpression {
            ssa: INVALID_SSA,
            kind: SingleExpressionKind::ValueList(values),
        }
    }
}

pub fn from_parsed(parsed: &::parser::Module) -> Module {
    Module::from_parsed(parsed)
}
