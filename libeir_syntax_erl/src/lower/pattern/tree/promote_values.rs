//! Resolves values, promotes nodes to values, applies constraints

use either::Either;
use std::collections::{BTreeSet, HashMap};

use libeir_ir::{FunctionBuilder, Value as IrValue, ValueKind};

use libeir_intern::Ident;

use libeir_util_datastructures::hashmap_stack::HashMapStack;

use super::{ConstraintKind, Tree, TreeNode, TreeNodeKind};
use crate::lower::{LowerCtx, LowerError};

struct PromoteCtx<'a, 'b> {
    ctx: &'a mut LowerCtx<'b>,
    binds: HashMap<Ident, TreeNode>,
    binds_scope: HashMapStack<Ident, TreeNode>,
    shadow: bool,
}
impl<'a, 'b> PromoteCtx<'a, 'b> {
    fn resolve_or_bind(
        &mut self,
        hier: bool,
        ident: Ident,
        node: TreeNode,
    ) -> Option<Either<TreeNode, IrValue>> {
        if hier {
            self.binds_scope.insert(ident, node);
        }
        if self.shadow {
            if let Some(bound_node) = self.binds.get(&ident) {
                Some(Either::Left(*bound_node))
            } else {
                self.bind(ident, node);
                None
            }
        } else {
            if let Ok(prev_bound) = self.ctx.scope.resolve(ident) {
                Some(Either::Right(prev_bound))
            } else {
                if let Some(bound_node) = self.binds.get(&ident) {
                    Some(Either::Left(*bound_node))
                } else {
                    self.bind(ident, node);
                    None
                }
            }
        }
    }

    fn resolve_only(&self, ident: Ident) -> Option<Either<TreeNode, IrValue>> {
        if let Some(bound_node) = self.binds_scope.get(&ident) {
            Some(Either::Left(*bound_node))
        } else {
            if let Ok(prev_bound) = self.ctx.scope.resolve(ident) {
                Some(Either::Right(prev_bound))
            } else {
                None
            }
        }
    }

    fn bind(&mut self, ident: Ident, node: TreeNode) -> Option<TreeNode> {
        let res = if let Some(prev) = self.binds.get(&ident) {
            if *prev == node {
                None
            } else {
                Some(*prev)
            }
        } else {
            self.binds.insert(ident, node);
            None
        };
        res
    }
}

pub(crate) fn promote_values(
    ctx: &mut LowerCtx,
    b: &mut FunctionBuilder,
    t: &mut Tree,
    shadow: bool,
) {
    let mut prom = PromoteCtx {
        ctx,
        binds: HashMap::new(),
        binds_scope: HashMapStack::new(),
        shadow,
    };

    prom.binds_scope.push();

    let num_root = t.roots.len();
    for n in 0..num_root {
        let root = t.roots[n];
        process_constants_node(b, &mut prom, t, root, false);
        promote_values_node(b, &mut prom, t, root);
    }

    prom.binds_scope.push();

    t.resolved_binds = Some(prom.binds);
}

fn process_constants_node(
    b: &mut FunctionBuilder,
    prom: &mut PromoteCtx,
    t: &mut Tree,
    node: TreeNode,
    hier_bind: bool,
) {
    let constraints: BTreeSet<_> = t.binds[node]
        .iter()
        .flat_map(|ident| prom.resolve_or_bind(hier_bind, *ident, node))
        .map(|v| match v {
            Either::Left(node) => ConstraintKind::Node(node),
            Either::Right(val) => match b.fun().value_kind(val) {
                ValueKind::Const(cons) => ConstraintKind::Const(cons),
                ValueKind::PrimOp(prim) => ConstraintKind::PrimOp(prim),
                _ => ConstraintKind::Value(val),
            },
        })
        .collect();

    t.constraints[node] = constraints;
}

fn promote_values_node(
    b: &mut FunctionBuilder,
    prom: &mut PromoteCtx,
    t: &mut Tree,
    node: TreeNode,
) {
    let kind = t.nodes[node].clone();

    let constraints = &t.constraints[node];
    let mut const_iter = constraints.iter().flat_map(|v| v.constant());

    // Get first constant constraint
    let const_constraint = const_iter.next();

    // If we have multiple different constant constraints,
    // the pattern can never be matched.
    if let Some(first) = const_constraint {
        for other in const_iter {
            if first != other {
                prom.ctx.warn(LowerError::UnmatchablePatternWarning {
                    pat: Some(t.node_span(node)),
                    reason: None,
                });
                t.unmatchable = true;
                return;
            }
        }
    }

    // If we have multiple different PrimOp constraints,
    // the pattern can never be matched
    // TODO implement this warning when eir typing utilities
    // are more complete.

    // TODO: Check compatibility with TreeNode constraints.

    match kind {
        TreeNodeKind::Atomic(span, c) => {
            // If we have another constant constraint, and they are not
            // equal, we can never match the pattern.
            if let Some(nc) = const_constraint {
                if c != nc {
                    prom.ctx.warn(LowerError::UnmatchablePatternWarning {
                        pat: Some(span),
                        reason: None,
                    });
                    t.unmatchable = true;
                    return;
                }
            }
        }
        TreeNodeKind::Wildcard(span) => {
            // Prefer to promote to constants, here we have
            // the most information.
            if let Some(cons) = const_constraint {
                t.nodes[node] = TreeNodeKind::Atomic(span, cons);
                return;
            }
            // Second choice is PrimOps. Here we have a certain
            // amount of information.
            if let Some(prim) = constraints.iter().flat_map(|c| c.primop()).nth(0) {
                let val = b.value(prim);
                t.nodes[node] = TreeNodeKind::Value(span, Either::Left(val));
                return;
            }
            // Third choice is any other value
            if let Some(val) = constraints.iter().flat_map(|c| c.value()).nth(0) {
                t.nodes[node] = TreeNodeKind::Value(span, Either::Left(val));
                return;
            }
        }
        TreeNodeKind::Tuple { elems, .. } => {
            for idx in 0..elems.len(&t.node_pool) {
                let child = elems.get(idx, &t.node_pool).unwrap();
                process_constants_node(b, prom, t, child, false);
            }

            for idx in 0..elems.len(&t.node_pool) {
                let child = elems.get(idx, &t.node_pool).unwrap();
                promote_values_node(b, prom, t, child);
            }
        }
        TreeNodeKind::Cons { head, tail, .. } => {
            process_constants_node(b, prom, t, head, false);
            process_constants_node(b, prom, t, tail, false);

            promote_values_node(b, prom, t, head);
            promote_values_node(b, prom, t, tail);
        }
        TreeNodeKind::Binary {
            value, tail, size, ..
        } => {
            let size_res = size.map(|v| match v {
                // The size references another node
                Either::Left(ident) => {
                    // We try to resolve it in the current pattern
                    match prom.resolve_only(ident) {
                        // We found a node in the pattern
                        Some(inner) => inner,
                        // No value found, error and return sentinel
                        None => {
                            prom.ctx
                                .error(LowerError::UnresolvedVariable { span: ident.span });
                            Either::Right(prom.ctx.sentinel())
                        }
                    }
                }
                Either::Right(val) => Either::Right(val),
            });
            if let TreeNodeKind::Binary { size_resolved, .. } = &mut t.nodes[node] {
                *size_resolved = size_res;
            } else {
                unreachable!();
            }

            prom.binds_scope.push();
            process_constants_node(b, prom, t, value, true);
            if let Some(tail_node) = tail {
                process_constants_node(b, prom, t, tail_node, false);
            }

            promote_values_node(b, prom, t, value);
            if let Some(tail_node) = tail {
                promote_values_node(b, prom, t, tail_node);
            }

            prom.binds_scope.pop();
        }
        TreeNodeKind::Map { entries, .. } => {
            for (_k, v) in entries.iter() {
                process_constants_node(b, prom, t, *v, false);
            }

            for (_k, v) in entries.iter() {
                promote_values_node(b, prom, t, *v);
            }
        }

        // While this cannot occur in a matchable pattern, we still
        // need to resolve scoping, so we handle it.
        TreeNodeKind::And { left, right } => {
            assert!(t.unmatchable);
            process_constants_node(b, prom, t, left, false);
            process_constants_node(b, prom, t, right, false);

            promote_values_node(b, prom, t, left);
            promote_values_node(b, prom, t, right);
        }

        TreeNodeKind::Value(_, _) => unreachable!(),
    }
}
