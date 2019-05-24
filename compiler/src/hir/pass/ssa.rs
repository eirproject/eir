//! Lowers the scoping semantics of core erlang into a permissive
//! form of SSA. It is not strict in the way that it does allow a
//! variable to be bound multiple times.
//!
//! While this might seem like a useless exercise at first, it is
//! simpler to convert this into a stricter form of SSA in a later
//! pass, since it dramatically simplifies some other compilation
//! passes. (See pattern match compilation)

use ::std::collections::{ HashMap, HashSet };
use ::hir::{ Expression, SingleExpression, SingleExpressionKind, PatternNode };

use ::hir::scope_tracker::VerboseLambdaEnv;
use ::hir::scope_tracker::{ ScopeTracker, ScopeDefinition };

pub fn assign_ssa_expression(env: &mut ScopeTracker, expr: &mut Expression) {
    for single in &mut expr.values {
        assign_ssa_single_expression(env, single);
    }
}

pub fn assign_ssa_single_expression(env: &mut ScopeTracker,
                                    expr: &mut SingleExpression) {
    match expr.kind {
        SingleExpressionKind::Variable(ref mut var) => {
            if let Some(ssa) = env.get(&ScopeDefinition::Variable(var.var.clone())) {
                var.ssa = ssa;
                expr.ssa = ssa;
            } else {
                panic!("variable {} not found in scope", var.var);
            }
        },
        SingleExpressionKind::InterModuleCall { ref mut module, ref mut name, ref mut args } => {
            assign_ssa_single_expression(env, module);
            assign_ssa_single_expression(env, name);
            for arg in args {
                assign_ssa_single_expression(env, arg);
            }
            expr.ssa = env.new_ssa();
        },
        SingleExpressionKind::Let { ref mut val, ref mut vars, ref mut body } => {
            assign_ssa_single_expression(env, val);

            let mut scope = HashMap::new();
            for var in vars.iter_mut() {
                var.ssa = env.new_ssa();
                scope.insert(ScopeDefinition::Variable(var.var.clone()), var.ssa);
            }
            env.push_scope(scope);
            assign_ssa_single_expression(env, body);
            env.pop_scope();
            expr.ssa = body.ssa;
        },
        SingleExpressionKind::ApplyCall { ref mut fun, ref mut args } => {
            for arg in args {
                assign_ssa_single_expression(env, arg);
            }
            assign_ssa_single_expression(env, fun);
            expr.ssa = env.new_ssa();
        },
        SingleExpressionKind::Try { ref mut body, ref mut then_vars, ref mut then,
                                    ref mut catch_vars, ref mut catch } => {
            //assert!(body.values.len() == then_vars.len());

            assign_ssa_single_expression(env, body);

            let mut scope = HashMap::new();
            for var in then_vars.iter_mut() {
                var.ssa = env.new_ssa(); //body.values[idx].ssa;
                scope.insert(ScopeDefinition::Variable(var.var.clone()), var.ssa);
            }
            env.push_scope(scope);
            assign_ssa_single_expression(env, then);
            env.pop_scope();

            let mut scope = HashMap::new();
            for var in catch_vars.iter_mut() {
                var.ssa = env.new_ssa();
                scope.insert(ScopeDefinition::Variable(var.var.clone()), var.ssa);
            }
            env.push_scope(scope);
            assign_ssa_single_expression(env, catch);
            env.pop_scope();

            expr.ssa = env.new_ssa();
        },
        SingleExpressionKind::Catch { ref mut body } => {
            assign_ssa_single_expression(env, body);
            expr.ssa = env.new_ssa();
        },
        // TODO
        SingleExpressionKind::Case { ref mut val, ref mut clauses,
                                     ref mut values } => {
            assign_ssa_single_expression(env, val);

            // Pattern values are not bound to variables, they are not inserted
            // into scope.
            for value in values {
                assign_ssa_single_expression(env, value);
            }

            // Assume that all matches in a pattern can see all variables here.
            // This should be validated later when compiling the pattern.
            for clause in clauses {
                let mut scope = HashMap::new();
                for pattern in clause.patterns.iter_mut() {
                    for &mut (ref var, ref mut ssa) in &mut pattern.binds {
                        *ssa = env.new_ssa();
                        scope.insert(ScopeDefinition::Variable(var.clone()), *ssa);
                    }
                }

                env.push_scope(scope.clone());
                for pattern in clause.patterns.iter_mut() {
                    assign_ssa_pattern_node(env, &mut pattern.node);
                }
                assign_ssa_single_expression(env, &mut clause.guard);
                assign_ssa_single_expression(env, &mut clause.body);
                env.pop_scope();
            }
            expr.ssa = env.new_ssa();
        },
        SingleExpressionKind::Atomic(_) => {
            expr.ssa = env.new_ssa();
        },
        // TODO
        SingleExpressionKind::NamedFunction { ref name, ref mut is_lambda } => {
            if let Some(ssa) = env.get(&ScopeDefinition::Function(name.var.clone())) {
                *is_lambda = true;
                expr.ssa = ssa;
            } else {
                *is_lambda = false;
                expr.ssa = env.new_ssa();
            }
        },
        SingleExpressionKind::ExternalNamedFunction { .. } => {
            expr.ssa = env.new_ssa();
        },
        SingleExpressionKind::Tuple(ref mut vals) => {
            for val in vals {
                assign_ssa_single_expression(env, val);
            }
            expr.ssa = env.new_ssa();
        },
        SingleExpressionKind::List { ref mut head, ref mut tail } => {
            for val in head {
                assign_ssa_single_expression(env, val);
            }
            assign_ssa_single_expression(env, tail);
            expr.ssa = env.new_ssa();
        },
        SingleExpressionKind::ValueList(ref mut vals) => {
            for val in vals {
                assign_ssa_single_expression(env, val);
            }
            expr.ssa = env.new_ssa();
        },
        SingleExpressionKind::Map { ref mut values, ref mut merge } => {
            for &mut (ref mut key, ref mut val, _assoc) in values.iter_mut() {
                assign_ssa_single_expression(env, key);
                assign_ssa_single_expression(env, val);
            }
            merge.as_mut().map(|v| assign_ssa_single_expression(env, v));
            expr.ssa = env.new_ssa();
        },
        SingleExpressionKind::Binary(ref mut elems) => {
            for (ref mut val, ref mut opts) in elems {
                assign_ssa_single_expression(env, val);
                for ref mut opt in opts {
                    assign_ssa_single_expression(env, opt);
                }
            }
            expr.ssa = env.new_ssa();
        },
        SingleExpressionKind::PrimOp { ref mut args, .. } => {
            for arg in args {
                assign_ssa_single_expression(env, arg);
            }
            expr.ssa = env.new_ssa();
        },
        SingleExpressionKind::Do(ref mut e1, ref mut e2) => {
            assign_ssa_single_expression(env, e1);
            assign_ssa_single_expression(env, e2);
            expr.ssa = e2.ssa;
        },
        SingleExpressionKind::Receive { ref mut clauses, ref mut pattern_values,
                                        ref mut timeout_time,
                                        ref mut timeout_body } => {
            for value in pattern_values {
                assign_ssa_single_expression(env, value);
            }

            for clause in clauses {
                let mut scope = HashMap::new();
                for pattern in clause.patterns.iter_mut() {
                    for &mut (ref var, ref mut ssa) in &mut pattern.binds {
                        *ssa = env.new_ssa();
                        scope.insert(ScopeDefinition::Variable(var.clone()), *ssa);
                    }
                }
                env.push_scope(scope);
                assign_ssa_single_expression(env, &mut clause.guard);
                assign_ssa_single_expression(env, &mut clause.body);
                env.pop_scope();
            }
            assign_ssa_single_expression(env, timeout_time);
            assign_ssa_single_expression(env, timeout_body);
            expr.ssa = env.new_ssa();
        },
        SingleExpressionKind::BindClosure { ref mut closure, ref mut lambda_env,
                                            ref mut env_ssa } => {
            // BindClosure makes a single closure and returns it.

            env.push_tracking();

            // Inject closure arguments into scope
            let mut scope = HashMap::new();
            for arg in &mut closure.fun.as_mut().unwrap().args {
                arg.ssa = env.new_ssa();
                scope.insert(ScopeDefinition::Variable(arg.var.clone()), arg.ssa);
            }

            // Body of closure
            env.push_scope(scope);
            assign_ssa_single_expression(
                env, &mut closure.fun.as_mut().unwrap().body);
            env.pop_scope();

            // SSA references crossing the closure boundary.
            // These will be wrapped in a LambdaEnv
            let captures_map = env.pop_tracking();
            let captures = captures_map.iter()
                .map(|(k, &(o, i))| (k.clone(), o, i))
                .collect();

            // Generate the LambdaEnvIdx and insert the LambdaEnv
            let env_idx = env.gen_env_idx();
            closure.gen_ident(env_idx, 0);
            *lambda_env = Some(env_idx);
            env.add_lambda_env(env_idx, VerboseLambdaEnv {
                env: env_idx,
                captures: captures,
                meta_binds: vec![],
                //meta_binds: vec![(closure.ident.clone().unwrap(),
                //                  closure.alias.as_ref().unwrap().ssa)],
            });

            // SSA results of env and expression
            *env_ssa = env.new_ssa();
            expr.ssa = env.new_ssa();
        },
        SingleExpressionKind::BindClosures { ref mut closures, ref mut body,
                                             ref mut lambda_env, ref mut env_ssa } => {
            // BindClosures makes a set of closures and makes them availible
            // inside the body. The result of body is returned.
            // The magic here is that each closure in the set of closures can
            // also access and call each other.

            // All closures can access and call each other.
            // Inject all closures into a scope.
            let mut closures_scope = HashMap::new();
            for closure in closures.iter_mut() {
                let alias = closure.alias.as_mut().unwrap();
                alias.ssa = env.new_ssa();
                closures_scope.insert(
                    ScopeDefinition::Function(alias.var.clone()),
                    alias.ssa.clone());
            }
            env.push_scope(closures_scope);

            // Since all closures can call each other, they share a single
            // LambdaEnv. This tracking scope captures all SSA references
            // that cross the boundary between any one of the closures
            // and outside.
            env.push_tracking();

            for closure in closures.iter_mut() {

                // Inject closure arguments into scope
                let mut scope = HashMap::new();
                for arg in &mut closure.fun.as_mut().unwrap().args {
                    arg.ssa = env.new_ssa();
                    scope.insert(ScopeDefinition::Variable(arg.var.clone()), arg.ssa);
                }

                // Body of closure
                env.push_scope(scope);
                assign_ssa_single_expression(
                    env, &mut closure.fun.as_mut().unwrap().body);
                env.pop_scope();

            }

            // SSA references crossing the closures boundary.
            // These will be wrapped in a LambdaEnv
            let captures_map = env.pop_tracking();
            let mut captures: Vec<_> = captures_map.iter()
                .map(|(k, &(o, i))| (k.clone(), o, i))
                .collect();

            // Generate the LambdaEnvIdx and insert the LambdaEnv
            let env_idx = env.gen_env_idx();
            *lambda_env = Some(env_idx);
            for (idx, closure) in closures.iter_mut().enumerate() {
                closure.gen_ident(env_idx, idx);
            }

            // For lambdas from same env which are referenced from within,
            // add an alternate SSA alias since they are removed from the
            // explicit captures. This is kinda hacky.
            let inside_vars_map: HashMap<_, _> =
                captures.iter().map(|v| (v.1, v.2)).collect();
            let meta_binds: Vec<_> = closures.iter()
                .map(|c| {
                    let ident = c.ident.clone().unwrap();
                    let outside_ssa = c.alias.as_ref().unwrap().ssa;
                    let inside_ssa = inside_vars_map.get(&outside_ssa).cloned();
                    (ident, outside_ssa, inside_ssa)
                })
                .collect();

            // Remove references to meta binds from captures
            let meta_binds_vars: HashSet<_> =
                meta_binds.iter().map(|v| v.1).collect();
            captures.retain(|v| !meta_binds_vars.contains(&v.1));

            env.add_lambda_env(env_idx, VerboseLambdaEnv {
                env: env_idx,
                captures: captures,
                meta_binds: meta_binds,
            });

            // Outer body. All lambdas are still in scope.
            assign_ssa_single_expression(env, body);
            env.pop_scope();

            // SSA results of env and expression
            *env_ssa = env.new_ssa();
            expr.ssa = body.ssa;
        },
        ref e => panic!("Unhandled: {:?}", e),
    }
}

pub fn assign_ssa_pattern_node(env: &mut ScopeTracker, node: &mut PatternNode) {
    match node {
        PatternNode::Wildcard => (),
        PatternNode::Atomic(_) => (),
        PatternNode::BindVar(_var, node) => assign_ssa_pattern_node(env, node),
        PatternNode::Binary(entries) => {
            for (node, _args) in entries.iter_mut() {
                assign_ssa_pattern_node(env, node);
                //for arg in args.iter_mut() {
                //    //match arg {
                //    //    ::parser::ConstantOrVariable::Variable(var) =>
                //    //        var.ssa = env.get(
                //    //            &ScopeDefinition::Variable(var.var.clone()))
                //    //        .unwrap(),
                //    //    _ => (),
                //    //}
                //}
            }
        },
        PatternNode::Tuple(nodes) => {
            for node in nodes.iter_mut() {
                assign_ssa_pattern_node(env, node);
            }
        },
        PatternNode::List(head, tail) => {
            for node in head.iter_mut() {
                assign_ssa_pattern_node(env, node);
            }
            assign_ssa_pattern_node(env, tail);
        }
        PatternNode::Map(entries) => {
            for (_key_num, val) in entries.iter_mut() {
                assign_ssa_pattern_node(env, val);
            }
        }
    }
}
