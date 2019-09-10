//! Merges And nodes in the pattern tree

use std::collections::{ BTreeMap, BTreeSet };

use libeir_ir::{
    FunctionBuilder,
};

use cranelift_entity::EntityList;

use crate::lower::{ LowerCtx, LowerError };
use super::{ Tree, TreeNode, TreeNodeKind };

pub(crate) fn merge_tree_nodes(
    ctx: &mut LowerCtx,
    b: &mut FunctionBuilder,
    t: &mut Tree,
) {
    let num_root = t.roots.len();
    for n in 0..num_root {
        let root = t.roots[n];
        let new = map_node(ctx, b, t, root);
        t.roots[n] = new;
    }
}

fn map_node(
    ctx: &mut LowerCtx,
    b: &mut FunctionBuilder,
    t: &mut Tree,
    node: TreeNode,
) -> TreeNode {
    let new = match t.nodes[node].clone() {
        TreeNodeKind::Atomic(_, _) => node,
        TreeNodeKind::Wildcard => node,
        TreeNodeKind::Tuple { span, elems } => {
            let mut new_elems = EntityList::new();
            for idx in 0..elems.len(&t.node_pool) {
                let node = elems.get(idx, &t.node_pool).unwrap();
                let new = map_node(ctx, b, t, node);
                new_elems.push(new, &mut t.node_pool);
            }
            t.nodes.push(TreeNodeKind::Tuple {
                span: span,
                elems: new_elems,
            })
        }
        TreeNodeKind::Cons { span, head, tail } => {
            let new_head = map_node(ctx, b, t, head);
            let new_tail = map_node(ctx, b, t, tail);
            t.nodes.push(TreeNodeKind::Cons {
                span: span,
                head: new_head,
                tail: new_tail,
            })
        }
        TreeNodeKind::Binary { span, specifier, size, size_resolved, value, tail } => {
            let n_value = map_node(ctx, b, t, value);
            let n_tail = map_node(ctx, b, t, tail);
            t.nodes.push(TreeNodeKind::Binary {
                span: span,
                specifier,
                size,
                size_resolved,
                value: n_value,
                tail: n_tail,
            })
        }
        TreeNodeKind::Map { span, entries } => {
            let mut dedup = BTreeMap::new();
            for (k, v) in entries.iter() {
                if let Some(old) = dedup.get(k) {
                    let new = t.nodes.push(
                        TreeNodeKind::And { left: *old, right: *v });
                    dedup.insert(*k, new);
                } else {
                    dedup.insert(*k, *v);
                }
            }
            let new_entries: Vec<_> = dedup.iter()
                .map(|(k, v)| (*k, map_node(ctx, b, t, *v)))
                .collect();
            t.nodes.push(TreeNodeKind::Map {
                span: span,
                entries: new_entries,
            })
        }
        TreeNodeKind::And { left, right } => {
            merge_nodes(ctx, b, t, left, right)
        }
        _ => unreachable!(),
    };
    t.rename(node, new);
    new
}

fn merge_nodes(
    ctx: &mut LowerCtx,
    b: &mut FunctionBuilder,
    t: &mut Tree,
    left: TreeNode,
    right: TreeNode,
) -> TreeNode {
    let left = map_node(ctx, b, t, left);
    let l_kind = t.nodes[left].clone();

    let right = map_node(ctx, b, t, right);
    let r_kind = t.nodes[right].clone();

    let new = match (l_kind, r_kind) {
        (TreeNodeKind::Wildcard, _) => right,
        (_, TreeNodeKind::Wildcard) => left,
        (TreeNodeKind::Atomic(s1, c1), TreeNodeKind::Atomic(s2, c2)) => {
            if c1 == c2 {
                t.nodes.push(TreeNodeKind::Atomic(s1, c1))
            } else {
                ctx.warn(LowerError::DisjointPatternUnionWarning {
                    left: Some(s1),
                    right: Some(s2),
                });
                t.unmatchable = true;
                t.nodes.push(TreeNodeKind::Wildcard)
            }
        }
        (TreeNodeKind::Tuple { span: s1, elems: e1 },
         TreeNodeKind::Tuple { span: s2, elems: e2 }) => {
            let len1 = e1.len(&t.node_pool);
            if len1 != e2.len(&t.node_pool) {
                ctx.warn(LowerError::DisjointPatternUnionWarning {
                    left: Some(s1),
                    right: Some(s2),
                });
                t.unmatchable = true;
                t.nodes.push(TreeNodeKind::Wildcard)
            } else {
                let mut elems = EntityList::new();
                for idx in 0..len1 {
                    let l = e1.get(idx, &t.node_pool).unwrap();
                    let r = e2.get(idx, &t.node_pool).unwrap();
                    let merged = merge_nodes(ctx, b, t, l, r);
                    elems.push(merged, &mut t.node_pool);
                }
                t.nodes.push(TreeNodeKind::Tuple {
                    span: s1,
                    elems,
                })
            }
        }
        (TreeNodeKind::Cons { span: s1, head: h1, tail: t1 },
         TreeNodeKind::Cons { span: _s2, head: h2, tail: t2 }) => {
            let h_m = merge_nodes(ctx, b, t, h1, h2);
            let t_m = merge_nodes(ctx, b, t, t1, t2);
            t.nodes.push(TreeNodeKind::Cons {
                span: s1,
                head: h_m,
                tail: t_m,
            })
        }
        (TreeNodeKind::Binary { .. },
         TreeNodeKind::Binary { .. }) => {
            unimplemented!()
        }
        (TreeNodeKind::Map { entries: e_l, span },
         TreeNodeKind::Map { entries: e_r, .. }) => {
            // Entries vectors should already be sorted
            debug_assert!(e_l.windows(2).all(|w| w[0].0 < w[1].0));
            debug_assert!(e_r.windows(2).all(|w| w[0].0 < w[1].0));

            let left: BTreeSet<_> = e_l.iter().map(|(k, _v)| *k).collect();
            let right: BTreeSet<_> = e_r.iter().map(|(k, _v)| *k).collect();

            let mut new = Vec::new();

            // Merge the intersection of the two maps
            let mut l_iter = e_l.iter();
            let mut r_iter = e_r.iter();
            for key in left.intersection(&right) {
                let l_n = l_iter.find(|(k, _v)| k == key)
                    .map(|(_k, v)| *v).unwrap();
                let r_n = r_iter.find(|(k, _v)| k == key)
                    .map(|(_k, v)| *v).unwrap();

                let merged = merge_nodes(ctx, b, t, l_n, r_n);
                new.push((*key, merged));
            }

            // Map over the left and right differences
            let mut l_iter = e_l.iter();
            for key in left.difference(&right) {
                let l_n = l_iter.find(|(k, _v)| k == key)
                    .map(|(_k, v)| *v).unwrap();

                let node = map_node(ctx, b, t, l_n);
                new.push((*key, node));
            }

            let mut r_iter = e_r.iter();
            for key in right.difference(&left) {
                let r_n = r_iter.find(|(k, _v)| k == key)
                    .map(|(_k, v)| *v).unwrap();

                let node = map_node(ctx, b, t, r_n);
                new.push((*key, node));
            }

            new.sort_by(|(k1, _), (k2, _)| k1.cmp(k2));

            t.nodes.push(TreeNodeKind::Map {
                span,
                entries: new,
            })
        }
        (TreeNodeKind::And { .. }, TreeNodeKind::And { .. }) => unreachable!(),
        _ => {
            ctx.warn(LowerError::DisjointPatternUnionWarning {
                left: t.node_span(left),
                right: t.node_span(right),
            });
            t.unmatchable = true;
            t.nodes.push(TreeNodeKind::And { left, right })
        }
    };

    t.rename(left, new);
    t.rename(right, new);

    new
}
