use std::collections::{BTreeMap, HashMap};

use either::Either;

use libeir_intern::Ident;
use libeir_ir::pattern::PatternContainer;
use libeir_ir::{FunctionBuilder, PatternNode};

use super::super::{ClauseLowerCtx, EqGuard};
use super::{ConstraintKind, Tree, TreeNode, TreeNodeKind};

impl Tree {
    pub(in super::super) fn lower(
        &self,
        b: &mut FunctionBuilder,
        pat: &mut PatternContainer,
        cl_ctx: &mut ClauseLowerCtx,
    ) {
        let mut node_map = BTreeMap::new();

        for root in self.roots.iter() {
            create_nodes(b, pat, self, &mut node_map, *root);
        }

        for root in self.roots.iter() {
            let lowered = lower_tree_node(b, pat, cl_ctx, &node_map, self, *root);
            pat.clause_node_push(cl_ctx.pat_clause, lowered);
        }

        let mut node_binds_map = HashMap::new();
        for (ident, node) in self.resolved_binds.as_ref().unwrap() {
            let new_bind_idx = cl_ctx.binds.len();
            node_binds_map.insert(*node, new_bind_idx);

            cl_ctx.binds.push(Some(*ident));
            let pat_node = node_map[node];
            pat.clause_bind_push(cl_ctx.pat_clause, pat_node);
        }

        for (node, constraints) in self.constraints.iter() {
            if constraints.len() == 0 {
                continue;
            }
            let pat_node = node_map[&node];

            let new_bind_idx = cl_ctx.binds.len();
            cl_ctx.binds.push(None);
            pat.clause_bind_push(cl_ctx.pat_clause, pat_node);

            for constraint in constraints.iter() {
                match constraint {
                    ConstraintKind::Value(val) => {
                        cl_ctx.eq_guards.push(EqGuard::EqValue(new_bind_idx, *val));
                    }
                    ConstraintKind::Const(cons) => {
                        let val = b.value(*cons);
                        cl_ctx.eq_guards.push(EqGuard::EqValue(new_bind_idx, val));
                    }
                    ConstraintKind::PrimOp(prim) => {
                        let val = b.value(*prim);
                        cl_ctx.eq_guards.push(EqGuard::EqValue(new_bind_idx, val));
                    }
                    ConstraintKind::Node(node) => {
                        let bind_idx = node_binds_map[node];
                        cl_ctx
                            .eq_guards
                            .push(EqGuard::EqBind(new_bind_idx, bind_idx));
                    }
                }
            }
        }
    }

    pub fn pseudo_binds(&self) -> Vec<Ident> {
        self.resolved_binds
            .as_ref()
            .unwrap()
            .keys()
            .cloned()
            .collect()
    }
}

fn create_nodes(
    b: &mut FunctionBuilder,
    pat: &mut PatternContainer,
    t: &Tree,
    map: &mut BTreeMap<TreeNode, PatternNode>,
    node: TreeNode,
) {
    assert!(!map.contains_key(&node));

    let span = t.node_span(node);
    let p_node = pat.node_empty(Some(span));
    map.insert(node, p_node);

    match &t.nodes[node] {
        TreeNodeKind::Atomic(_, _) => (),
        TreeNodeKind::Value(_, _) => (),
        TreeNodeKind::Wildcard(_) => (),
        TreeNodeKind::Tuple { elems, .. } => {
            for elem in elems.as_slice(&t.node_pool) {
                create_nodes(b, pat, t, map, *elem);
            }
        }
        TreeNodeKind::Cons { head, tail, .. } => {
            create_nodes(b, pat, t, map, *head);
            create_nodes(b, pat, t, map, *tail);
        }
        TreeNodeKind::Binary { value, tail, .. } => {
            create_nodes(b, pat, t, map, *value);
            if let Some(tail_node) = tail {
                create_nodes(b, pat, t, map, *tail_node);
            }
        }
        TreeNodeKind::Map { entries, .. } => {
            for (_, v) in entries {
                create_nodes(b, pat, t, map, *v);
            }
        }
        _ => unreachable!(),
    }
}

fn lower_tree_node(
    b: &mut FunctionBuilder,
    pat: &mut PatternContainer,
    cl_ctx: &mut ClauseLowerCtx,
    map: &BTreeMap<TreeNode, PatternNode>,
    t: &Tree,
    node: TreeNode,
) -> PatternNode {
    let p_node = map[&node];

    match &t.nodes[node] {
        TreeNodeKind::Atomic(span, cons) => {
            pat.constant(p_node, *cons);
            pat.node_set_span(p_node, *span);
        }
        TreeNodeKind::Value(span, Either::Left(val)) => {
            let cl_val = cl_ctx.clause_value(pat, *val);

            pat.value(p_node, cl_val);
            pat.node_set_span(p_node, *span);
        }
        TreeNodeKind::Value(_, Either::Right(_node)) => unimplemented!(),
        TreeNodeKind::Wildcard(_) => {
            pat.wildcard(p_node);
        }
        TreeNodeKind::Tuple { span, elems } => {
            pat.tuple(p_node);
            for elem in elems.as_slice(&t.node_pool) {
                let child = lower_tree_node(b, pat, cl_ctx, map, t, *elem);
                pat.tuple_elem_push(p_node, child);
            }
            pat.node_finish(p_node);
            pat.node_set_span(p_node, *span);
        }
        TreeNodeKind::Cons { span, head, tail } => {
            let head_n = lower_tree_node(b, pat, cl_ctx, map, t, *head);
            let tail_n = lower_tree_node(b, pat, cl_ctx, map, t, *tail);
            pat.list(p_node, head_n, tail_n);
            pat.node_set_span(p_node, *span);
        }
        TreeNodeKind::Binary {
            span,
            specifier,
            size_resolved,
            value,
            tail,
            ..
        } => {
            let size = size_resolved.map(|s| match s {
                Either::Left(node) => {
                    let pat_node = map[&node];
                    pat.clause_node_value(cl_ctx.pat_clause, pat_node)
                }
                Either::Right(val) => cl_ctx.clause_value(pat, val),
            });

            let value_n = lower_tree_node(b, pat, cl_ctx, map, t, *value);
            let tail_n = tail.map(|n| lower_tree_node(b, pat, cl_ctx, map, t, n));
            pat.binary(p_node, *specifier, value_n, size, tail_n);
            pat.node_set_span(p_node, *span);
        }
        TreeNodeKind::Map { span, entries } => {
            pat.map(p_node);
            for (k, v) in entries {
                let key = cl_ctx.clause_value(pat, *k);
                let val = lower_tree_node(b, pat, cl_ctx, map, t, *v);
                pat.map_push(p_node, key, val);
            }
            pat.node_set_span(p_node, *span);
        }
        _ => unreachable!(),
    }

    p_node
}
