use std::collections::{ HashMap, BTreeMap };

use either::Either;

use libeir_ir::{
    FunctionBuilder,
    PatternNode,
};
use libeir_intern::Ident;
use libeir_diagnostics::DUMMY_SPAN;

use super::{ Tree, TreeNode, TreeNodeKind, ConstraintKind };
use super::super::{ ClauseLowerCtx, EqGuard };

impl Tree {

    pub(in super::super) fn lower(
        &self,
        b: &mut FunctionBuilder,
        cl_ctx: &mut ClauseLowerCtx,
    ) {
        let mut node_map = BTreeMap::new();

        for root in self.roots.iter() {
            create_nodes(b, self, &mut node_map, *root);
        }

        for root in self.roots.iter() {
            let lowered = lower_tree_node(b, cl_ctx, &node_map, self, *root);
            b.pat_mut().clause_node_push(cl_ctx.pat_clause, lowered);
        }

        let mut node_binds_map = HashMap::new();
        for (ident, node) in self.resolved_binds.as_ref().unwrap() {
            let new_bind_idx = cl_ctx.binds.len();
            node_binds_map.insert(*node, new_bind_idx);

            cl_ctx.binds.push(Some(*ident));
            let pat_node = node_map[node];
            b.pat_mut().clause_bind_push(cl_ctx.pat_clause, pat_node);
        }

        for (node, constraints) in self.constraints.iter() {
            if constraints.len() == 0 { continue; }
            let pat_node = node_map[&node];

            let new_bind_idx = cl_ctx.binds.len();
            cl_ctx.binds.push(None);
            b.pat_mut().clause_bind_push(cl_ctx.pat_clause, pat_node);

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
                        cl_ctx.eq_guards.push(EqGuard::EqBind(new_bind_idx, bind_idx));
                    }
                }
            }
        }
    }

    pub fn pseudo_binds(&self) -> Vec<Ident> {
        self.resolved_binds.as_ref().unwrap().keys().cloned().collect()
    }

}


fn create_nodes(
    b: &mut FunctionBuilder,
    t: &Tree,
    map: &mut BTreeMap<TreeNode, PatternNode>,
    node: TreeNode,
) {
    assert!(!map.contains_key(&node));

    let p_node = b.pat_mut().node_empty(Some(DUMMY_SPAN));
    map.insert(node, p_node);

    match &t.nodes[node] {
        TreeNodeKind::Atomic(_, _) => (),
        TreeNodeKind::Value(_, _) => (),
        TreeNodeKind::Wildcard => (),
        TreeNodeKind::Tuple { elems, .. } => {
            for elem in elems.as_slice(&t.node_pool) {
                create_nodes(b, t, map, *elem);
            }
        }
        TreeNodeKind::Cons { head, tail, .. } => {
            create_nodes(b, t, map, *head);
            create_nodes(b, t, map, *tail);
        }
        TreeNodeKind::Binary { value, tail, .. } => {
            create_nodes(b, t, map, *value);
            create_nodes(b, t, map, *tail);
        }
        TreeNodeKind::Map { entries, .. } => {
            for (_, v) in entries {
                create_nodes(b, t, map, *v);
            }
        }
        _ => unreachable!(),
    }

}

fn lower_tree_node(
    b: &mut FunctionBuilder,
    cl_ctx: &mut ClauseLowerCtx,
    map: &BTreeMap<TreeNode, PatternNode>,
    t: &Tree,
    node: TreeNode,
) -> PatternNode
{
    let p_node = map[&node];

    match &t.nodes[node] {
        TreeNodeKind::Atomic(span, cons) => {
            b.pat_mut().constant(p_node, *cons);
            b.pat_mut().node_set_span(p_node, *span);
        }
        TreeNodeKind::Value(span, Either::Left(val)) => {
            let cl_val = cl_ctx.clause_value(b, *val);

            b.pat_mut().value(p_node, cl_val);
            b.pat_mut().node_set_span(p_node, *span);
        }
        TreeNodeKind::Value(_, Either::Right(_node)) => {
            unimplemented!()
        }
        TreeNodeKind::Wildcard => {
            b.pat_mut().wildcard(p_node);
        }
        TreeNodeKind::Tuple { span, elems } => {
            b.pat_mut().tuple(p_node);
            for elem in elems.as_slice(&t.node_pool) {
                let child = lower_tree_node(b, cl_ctx, map, t, *elem);
                b.pat_mut().tuple_elem_push(p_node, child);
            }
            b.pat_mut().node_finish(p_node);
            b.pat_mut().node_set_span(p_node, *span);
        }
        TreeNodeKind::Cons { span, head, tail } => {
            let head_n = lower_tree_node(b, cl_ctx, map, t, *head);
            let tail_n = lower_tree_node(b, cl_ctx, map, t, *tail);
            b.pat_mut().list(p_node, head_n, tail_n);
            b.pat_mut().node_set_span(p_node, *span);
        }
        TreeNodeKind::Binary { span, specifier, size_resolved, value, tail, .. } => {
            let size = size_resolved.map(|s| match s {
                Either::Left(node) => {
                    let pat_node = map[&node];
                    b.pat_mut().clause_node_value(cl_ctx.pat_clause, pat_node)
                }
                Either::Right(val) => cl_ctx.clause_value(b, val),
            });

            let value_n = lower_tree_node(b, cl_ctx, map, t, *value);
            let tail_n = lower_tree_node(b, cl_ctx, map, t, *tail);
            b.pat_mut().binary(p_node, *specifier, value_n, size, tail_n);
            b.pat_mut().node_set_span(p_node, *span);
        }
        TreeNodeKind::Map { span, entries } => {
            b.pat_mut().map(p_node);
            for (k, v) in entries {
                let key = cl_ctx.clause_value(b, *k);
                let val = lower_tree_node(b, cl_ctx, map, t, *v);
                b.pat_mut().map_push(p_node, key, val);
            }
            b.pat_mut().node_set_span(p_node, *span);
        }
        _ => unreachable!(),
    }

    p_node
}
