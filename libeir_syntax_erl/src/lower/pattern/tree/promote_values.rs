//! Resolves values, promotes nodes to values, applies constraints

use std::collections::{ HashMap, BTreeSet };
use either::Either;

use libeir_ir::{
    FunctionBuilder,
    Value as IrValue,
    ValueKind,
};

use libeir_intern::Ident;
use libeir_diagnostics::DUMMY_SPAN;

use libeir_util::hashmap_stack::HashMapStack;

use crate::lower::{ LowerCtx, LowerError };
use super::{ Tree, TreeNode, TreeNodeKind, ConstraintKind };

struct PromoteCtx<'a, 'b> {
    ctx: &'a mut LowerCtx<'b>,
    binds: HashMap<Ident, TreeNode>,
    binds_scope: HashMapStack<Ident, TreeNode>,
}
impl<'a, 'b> PromoteCtx<'a, 'b> {

    fn resolve(&self, ident: Ident) -> Option<Either<TreeNode, IrValue>> {
        if let Ok(prev_bound) = self.ctx.scope.resolve(ident) {
            assert!(!self.binds.contains_key(&ident));
            Some(Either::Right(prev_bound))
        } else {
            if let Some(bound_node) = self.binds.get(&ident) {
                Some(Either::Left(*bound_node))
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
        //if hier {
        //    self.binds.insert(ident, node);
        //    self.binds_scope.insert(node, ());
        //}
        res
    }

    fn hier_bind(&mut self, ident: Ident, node: TreeNode) {
        self.binds_scope.insert(ident, node);
    }

}

pub(crate) fn promote_values(
    ctx: &mut LowerCtx,
    b: &mut FunctionBuilder,
    t: &mut Tree,
) {
    let mut prom = PromoteCtx {
        ctx,
        binds: HashMap::new(),
        binds_scope: HashMapStack::new(),
    };

    let num_root = t.roots.len();
    for n in 0..num_root {
        let root = t.roots[n];
        promote_values_node(b, &mut prom, t, root, false);
    }

    t.resolved_binds = Some(prom.binds);
}

fn promote_values_node(
    b: &mut FunctionBuilder,
    prom: &mut PromoteCtx,
    t: &mut Tree,
    node: TreeNode,
    hier_bind: bool,
) {
    let kind = t.nodes[node].clone();

    if hier_bind {
        println!("HIER {:?}", t.binds[node]);
        println!("BINDS BEFORE {:?}", prom.binds_scope);
    }

    let constraints: BTreeSet<_> =
        t.binds[node].iter()
        .flat_map(|ident| {
            if hier_bind {
                prom.hier_bind(*ident, node);
            }
            match prom.resolve(*ident) {
                None => {
                    prom.bind(*ident, node);
                    None
                }
                Some(v) => Some(v),
            }
        })
        .map(|v| {
            match v {
                Either::Left(node) => ConstraintKind::Node(node),
                Either::Right(val) => {
                    match b.fun().value_kind(val) {
                        ValueKind::Const(cons) => ConstraintKind::Const(cons),
                        ValueKind::PrimOp(prim) => ConstraintKind::PrimOp(prim),
                        _ => ConstraintKind::Value(val),
                    }
                }
            }
        })
        .collect();

    if hier_bind {
        println!("BINDS AFTER {:?}", prom.binds_scope);
    }

    let mut const_iter = constraints.iter()
        .flat_map(|v| v.constant());

    // Get first constant constraint
    let const_constraint = const_iter.next();

    // If we have multiple different constant constraints,
    // the pattern can never be matched.
    if let Some(first) = const_constraint {
        for other in const_iter {
            if first != other {
                prom.ctx.warn(LowerError::UnmatchablePatternWarning {
                    pat: t.node_span(node),
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

    // Add constraints
    t.constraints[node] = constraints.iter().cloned().collect();

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
        TreeNodeKind::Wildcard => {
            // Prefer to promote to constants, here we have
            // the most information.
            if let Some(cons) = const_constraint {
                t.nodes[node] = TreeNodeKind::Atomic(DUMMY_SPAN, cons);
                return;
            }
            // Second choice is PrimOps. Here we have a certain
            // amount of information.
            if let Some(prim) = constraints.iter()
                .flat_map(|c| c.primop())
                .nth(0)
            {
                let val = b.value(prim);
                t.nodes[node] = TreeNodeKind::Value(DUMMY_SPAN, Either::Left(val));
                return;
            }
            // Third choice is any other value
            if let Some(val) = constraints.iter()
                .flat_map(|c| c.value())
                .nth(0)
            {
                t.nodes[node] = TreeNodeKind::Value(DUMMY_SPAN, Either::Left(val));
                return;
            }
        }
        TreeNodeKind::Tuple { elems, .. } => {
            for idx in 0..elems.len(&t.node_pool) {
                let child = elems.get(idx, &t.node_pool).unwrap();
                promote_values_node(b, prom, t, child, false);
            }
        }
        TreeNodeKind::Cons { head, tail, .. } => {
            promote_values_node(b, prom, t, head, false);
            promote_values_node(b, prom, t, tail, false);
        }
        TreeNodeKind::Binary { value, tail, size, .. } => {
            let size_res = size.map(|v| match v {
                // The size references another node
                Either::Left(ident) => {
                    println!("{:?}", ident);
                    println!("RESOLVED {:?}", prom.resolve(ident));
                    println!("BINDS_SCOPE {:?}", prom.binds_scope);
                    // We try to resolve it in the current pattern
                    match prom.resolve(ident) {
                        // We found a node in the pattern
                        Some(Either::Left(_node)) => {
                            // Test if it's actually within scope
                            if let Some(node) = prom.binds_scope.get(&ident) {
                                Either::Left(*node)
                            } else {
                                prom.ctx.error(LowerError::UnresolvedVariable {
                                    span: ident.span,
                                });
                                Either::Right(prom.ctx.sentinel())
                            }
                        }
                        // Found a value, should never really happen
                        Some(Either::Right(val)) => Either::Right(val),
                        // No value found, error and return sentinel
                        None => {
                            prom.ctx.error(LowerError::UnresolvedVariable {
                                span: ident.span,
                            });
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
            promote_values_node(b, prom, t, value, true);
            promote_values_node(b, prom, t, tail, false);
            prom.binds_scope.pop();
        }
        TreeNodeKind::Map { entries, .. } => {
            for (_k, v) in entries.iter() {
                promote_values_node(b, prom, t, *v, false);
            }
        }

        // While this cannot occur in a matchable pattern, we still
        // need to resolve scoping, so we handle it.
        TreeNodeKind::And { left, right } => {
            assert!(t.unmatchable);
            promote_values_node(b, prom, t, left, false);
            promote_values_node(b, prom, t, right, false);
        },

        TreeNodeKind::Value(_, _) => unreachable!(),
    }
}
