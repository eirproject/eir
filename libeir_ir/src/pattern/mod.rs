use std::collections::HashMap;

use cranelift_entity::{entity_impl, EntityList, ListPool, PrimaryMap};

use libeir_diagnostics::SourceSpan;

use crate::binary::BinaryEntrySpecifier;
use crate::constant::ConstantContainer;
use crate::Const;

#[derive(Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct PatternNode(u32);
entity_impl!(PatternNode, "pattern_node");

/// A input value for a pattern, used in map matching
/// and binary matching
#[derive(Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct PatternValue(u32);
entity_impl!(PatternValue, "pattern_value");

#[derive(Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct PatternClause(u32);
entity_impl!(PatternClause, "pattern_clause");

#[derive(Debug, Clone)]
pub struct PatternContainer {
    nodes: PrimaryMap<PatternNode, PatternNodeData>,
    values: PrimaryMap<PatternValue, ()>,
    clauses: PrimaryMap<PatternClause, PatternClauseData>,

    pub node_pool: ListPool<PatternNode>,
    pub value_pool: ListPool<PatternValue>,

    /// Temporary map used for copies
    tmp_val_map: Option<HashMap<PatternValue, PatternValue>>,
    tmp_node_map: Option<HashMap<PatternNode, PatternNode>>,
}

impl Default for PatternContainer {
    fn default() -> PatternContainer {
        PatternContainer {
            nodes: PrimaryMap::new(),
            values: PrimaryMap::new(),
            clauses: PrimaryMap::new(),

            node_pool: ListPool::new(),
            value_pool: ListPool::new(),

            tmp_val_map: Some(HashMap::new()),
            tmp_node_map: Some(HashMap::new()),
        }
    }
}

#[derive(Debug, Clone)]
struct PatternClauseData {
    span: SourceSpan,
    root_nodes: EntityList<PatternNode>,

    node_binds_keys: EntityList<PatternNode>,
    node_binds_vals: EntityList<PatternValue>,

    /// This is the set of nodes that are bound from the pattern.
    /// This will correspond to another list of the same length stored
    /// outside of the container, the two will together form a pair.
    /// This way of doing things enables this single representation
    /// of patterns to work through several IRs.
    binds: EntityList<PatternNode>,

    /// This is the set of values that are accessible from the pattern.
    /// Same as `binds`, except for external values used within the clause.
    values: EntityList<PatternValue>,

    finished: bool,
}

#[derive(Debug, Clone)]
struct PatternNodeData {
    kind: Option<PatternNodeKind>,
    finished: bool,
    span: SourceSpan,
}

#[derive(Debug, Clone)]
pub enum PatternNodeKind {
    Wildcard,
    Const(Const),
    Value(PatternValue),
    Binary {
        specifier: BinaryEntrySpecifier,
        value: PatternNode,
        size: Option<PatternValue>,

        remaining: PatternNode,
    },
    Tuple(EntityList<PatternNode>),
    List {
        head: PatternNode,
        tail: PatternNode,
    },
    Map {
        keys: EntityList<PatternValue>,
        values: EntityList<PatternNode>,
    },
}

#[derive(Debug, Clone)]
pub struct PatternBinaryEntryData {}

pub enum PatternMergeFail {
    /// The two patterns are disjoint, they can never match at the
    /// same time.
    /// The two nodes are the reason for this.
    Disjoint {
        left: Option<PatternNode>,
        right: PatternNode,
    },
    /// Merging is not supported. Only happens with binaries
    /// The two nodes are the reason for this.
    Failure {
        left: Option<PatternNode>,
        right: PatternNode,
    },
}

impl PatternContainer {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn clause_value(&mut self, clause: PatternClause) -> PatternValue {
        let val = self.values.push(());
        self.clauses[clause].values.push(val, &mut self.value_pool);
        val
    }

    pub fn clause_node_value(&mut self, clause: PatternClause, node: PatternNode) -> PatternValue {
        let val = self.values.push(());

        let data = &mut self.clauses[clause];
        data.node_binds_keys.push(node, &mut self.node_pool);
        data.node_binds_vals.push(val, &mut self.value_pool);

        val
    }

    pub fn node_empty(&mut self, span: Option<SourceSpan>) -> PatternNode {
        self.nodes.push(PatternNodeData {
            kind: None,
            finished: false,
            span: span.unwrap_or(SourceSpan::UNKNOWN),
        })
    }

    pub fn binary(
        &mut self,
        node: PatternNode,
        specifier: BinaryEntrySpecifier,
        value: PatternNode,
        size: Option<PatternValue>,
        remaining: PatternNode,
    ) {
        let mut data = &mut self.nodes[node];
        assert!(data.kind.is_none());
        data.kind = Some(PatternNodeKind::Binary {
            specifier,
            value,
            size,

            remaining,
        });
        data.finished = true;
    }

    pub fn wildcard(&mut self, node: PatternNode) {
        let mut data = &mut self.nodes[node];
        assert!(data.kind.is_none());
        data.kind = Some(PatternNodeKind::Wildcard);
        data.finished = true;
    }

    pub fn constant(&mut self, node: PatternNode, val: Const) {
        let mut data = &mut self.nodes[node];
        assert!(data.kind.is_none());
        data.kind = Some(PatternNodeKind::Const(val));
        data.finished = true;
    }

    pub fn value(&mut self, node: PatternNode, val: PatternValue) {
        let mut data = &mut self.nodes[node];
        assert!(data.kind.is_none());
        data.kind = Some(PatternNodeKind::Value(val));
        data.finished = true;
    }

    pub fn tuple(&mut self, node: PatternNode) {
        let mut data = &mut self.nodes[node];
        assert!(data.kind.is_none());
        data.kind = Some(PatternNodeKind::Tuple(EntityList::new()));
    }

    pub fn tuple_elem_push(&mut self, tup: PatternNode, node: PatternNode) {
        let data = &mut self.nodes[tup];
        assert!(!data.finished);
        assert!(data.kind.is_some());
        if let PatternNodeKind::Tuple(ref mut list) = data.kind.as_mut().unwrap() {
            list.push(node, &mut self.node_pool);
        } else {
            panic!();
        }
    }

    pub fn list(&mut self, node: PatternNode, head: PatternNode, tail: PatternNode) {
        let mut data = &mut self.nodes[node];
        assert!(data.kind.is_none());
        data.kind = Some(PatternNodeKind::List { head, tail });
        data.finished = true;
    }

    pub fn map(&mut self, node: PatternNode) {
        let mut data = &mut self.nodes[node];
        assert!(data.kind.is_none());
        data.kind = Some(PatternNodeKind::Map {
            keys: EntityList::new(),
            values: EntityList::new(),
        });
    }

    pub fn map_push(&mut self, map: PatternNode, key: PatternValue, value: PatternNode) {
        let data = &mut self.nodes[map];
        assert!(!data.finished);
        assert!(data.kind.is_some());
        if let PatternNodeKind::Map {
            ref mut keys,
            ref mut values,
        } = data.kind.as_mut().unwrap()
        {
            keys.push(key, &mut self.value_pool);
            values.push(value, &mut self.node_pool);
        } else {
            panic!();
        }
    }

    pub fn node_finish(&mut self, node: PatternNode) {
        let data = &mut self.nodes[node];
        assert!(data.kind.is_some());
        data.finished = true;
    }

    pub fn node_set_span(&mut self, node: PatternNode, span: SourceSpan) {
        self.nodes[node].span = span;
    }

    pub fn clause_start(&mut self, span: SourceSpan) -> PatternClause {
        self.clauses.push(PatternClauseData {
            span,

            root_nodes: EntityList::new(),

            node_binds_keys: EntityList::new(),
            node_binds_vals: EntityList::new(),

            binds: EntityList::new(),
            values: EntityList::new(),

            finished: false,
        })
    }
    pub fn clause_node_push(&mut self, clause: PatternClause, node: PatternNode) {
        let data = &mut self.clauses[clause];
        assert!(!data.finished);

        data.root_nodes.push(node, &mut self.node_pool);
    }
    pub fn clause_bind_push(&mut self, clause: PatternClause, node: PatternNode) {
        let data = &mut self.clauses[clause];
        assert!(!data.finished);

        data.binds.push(node, &mut self.node_pool);
    }
    pub fn clause_value_push(&mut self, clause: PatternClause, val: PatternValue) {
        let data = &mut self.clauses[clause];
        assert!(!data.finished);

        data.values.push(val, &mut self.value_pool);
    }
    pub fn clause_finish(&mut self, clause: PatternClause) {
        let data = &mut self.clauses[clause];
        assert!(!data.finished);

        data.finished = true;
    }

    pub fn copy_from(&mut self, clause: PatternClause, from: &PatternContainer) -> PatternClause {
        let from_clause = &from.clauses[clause];

        let mut value_map = self.tmp_val_map.take().unwrap();
        let mut node_map = self.tmp_node_map.take().unwrap();

        let mut new_values = EntityList::new();
        for from_val in from_clause.values.as_slice(&from.value_pool) {
            let new_val = self.values.push(());
            new_values.push(new_val, &mut self.value_pool);
            value_map.insert(*from_val, new_val);
        }

        let mut new_roots = EntityList::new();
        for node in from_clause.root_nodes.as_slice(&from.node_pool) {
            let new = copy_pattern_node(&value_map, &mut node_map, *node, from, self);
            new_roots.push(new, &mut self.node_pool);
        }

        let mut new_binds = EntityList::new();
        for bind in from_clause.binds.as_slice(&from.node_pool) {
            new_binds.push(node_map[bind], &mut self.node_pool);
        }

        value_map.clear();
        self.tmp_val_map = Some(value_map);
        node_map.clear();
        self.tmp_node_map = Some(node_map);

        unimplemented!()
        //self.clauses.push(PatternClauseData {
        //    root_nodes: new_roots,

        //    node_binds_keys: unimplemented!(),
        //    node_binds_vals: unimplemented!(),

        //    binds: new_binds,
        //    values: new_values,

        //    finished: true,
        //})
    }

    /// Given a HashMap containing the mapping, this will go through all the
    /// binds of a clause and update them. Useful when merging nodes while
    /// constructing patterns.
    pub fn update_binds(&mut self, clause: PatternClause, map: &HashMap<PatternNode, PatternNode>) {
        let clause_d = &mut self.clauses[clause];

        let len = clause_d.binds.len(&self.node_pool);
        for n in 0..len {
            let entry = clause_d.binds.get_mut(n, &mut self.node_pool).unwrap();
            if let Some(new) = map.get(&*entry) {
                *entry = *new;
            }
        }
    }
}

impl PatternContainer {
    pub fn clause_root_nodes(&self, clause: PatternClause) -> &[PatternNode] {
        let data = &self.clauses[clause];
        data.root_nodes.as_slice(&self.node_pool)
    }

    pub fn clause_binds(&self, clause: PatternClause) -> &[PatternNode] {
        let data = &self.clauses[clause];
        data.binds.as_slice(&self.node_pool)
    }

    pub fn clause_values(&self, clause: PatternClause) -> &[PatternValue] {
        let data = &self.clauses[clause];
        data.values.as_slice(&self.value_pool)
    }

    pub fn clause_node_binds_iter<'a>(
        &'a self,
        clause: PatternClause,
    ) -> impl Iterator<Item = (PatternValue, PatternNode)> + 'a {
        let data = &self.clauses[clause];
        data.node_binds_vals
            .as_slice(&self.value_pool)
            .iter()
            .cloned()
            .zip(
                data.node_binds_keys
                    .as_slice(&self.node_pool)
                    .iter()
                    .cloned(),
            )
    }

    pub fn node_span(&self, node: PatternNode) -> SourceSpan {
        self.nodes[node].span
    }

    pub fn node_kind(&self, node: PatternNode) -> &PatternNodeKind {
        self.nodes[node].kind.as_ref().unwrap()
    }
}

fn copy_pattern_node(
    value_map: &HashMap<PatternValue, PatternValue>,
    node_map: &mut HashMap<PatternNode, PatternNode>,
    node: PatternNode,
    from: &PatternContainer,
    to: &mut PatternContainer,
) -> PatternNode {
    let data = &from.nodes[node];
    match data.kind.as_ref().unwrap() {
        PatternNodeKind::Wildcard => {
            let new = to.node_empty(Some(data.span));
            to.wildcard(new);
            node_map.insert(node, new);
            new
        }
        PatternNodeKind::Value(val) => {
            let new = to.node_empty(Some(data.span));
            to.value(new, *val);
            node_map.insert(node, new);
            new
        }
        PatternNodeKind::Tuple(elems) => {
            let new = to.node_empty(Some(data.span));
            to.tuple(new);

            for elem in elems.as_slice(&from.node_pool) {
                let copied = copy_pattern_node(value_map, node_map, *elem, from, to);
                to.tuple_elem_push(new, copied);
            }

            to.node_finish(new);
            node_map.insert(node, new);
            new
        }
        PatternNodeKind::List { head, tail } => {
            let head_copied = copy_pattern_node(value_map, node_map, *head, from, to);
            let tail_copied = copy_pattern_node(value_map, node_map, *tail, from, to);

            let new = to.node_empty(Some(data.span));
            to.list(new, head_copied, tail_copied);

            node_map.insert(node, new);
            new
        }
        PatternNodeKind::Map { keys, values } => {
            let new = to.node_empty(Some(data.span));
            to.map(new);

            for (key, val) in keys
                .as_slice(&from.value_pool)
                .iter()
                .zip(values.as_slice(&from.node_pool))
            {
                let copied = copy_pattern_node(value_map, node_map, *val, from, to);
                to.map_push(new, value_map[key], copied);
            }

            to.node_finish(new);
            node_map.insert(node, new);
            new
        }
        _ => unimplemented!("{:?}", data.kind),
    }
}

impl PatternContainer {
    pub fn merge_patterns(
        &mut self,
        cons: &ConstantContainer,
        map: &mut HashMap<PatternNode, PatternNode>,
        lhs: PatternNode,
        rhs: PatternNode,
    ) -> Result<PatternNode, PatternMergeFail> {
        // This clone is really cheap since entitylists are basically just an usize
        let lhs_kind = self.nodes[lhs].kind.clone().unwrap();
        let rhs_kind = self.nodes[rhs].kind.clone().unwrap();

        // TODO: Fuse locations
        let fused_span = self.nodes[lhs].span;
        match (lhs_kind, rhs_kind) {
            (PatternNodeKind::Value(l_val), PatternNodeKind::Value(r_val)) if l_val == r_val => {
                map.insert(rhs, lhs);
                Ok(lhs)
            }
            (PatternNodeKind::Const(l), _) => {
                self.merge_pattern_constant(cons, map, fused_span, rhs, l)
            }
            (_, PatternNodeKind::Const(r)) => {
                self.merge_pattern_constant(cons, map, fused_span, lhs, r)
            }
            (PatternNodeKind::Wildcard, _) => {
                map.insert(lhs, rhs);
                Ok(rhs)
            }
            (_, PatternNodeKind::Wildcard) => {
                map.insert(rhs, lhs);
                Ok(lhs)
            }
            (PatternNodeKind::Tuple(l1), PatternNodeKind::Tuple(l2)) => {
                let l1_len = l1.len(&self.node_pool);
                let l2_len = l2.len(&self.node_pool);
                if l1_len == l2_len {
                    let new_tup = self.node_empty(Some(fused_span));
                    self.tuple(new_tup);
                    for idx in 0..l1_len {
                        let ln = l1.get(idx, &self.node_pool).unwrap();
                        let rn = l2.get(idx, &self.node_pool).unwrap();

                        let merged = self.merge_patterns(cons, map, ln, rn)?;

                        self.tuple_elem_push(new_tup, merged);
                    }
                    self.node_finish(new_tup);

                    map.insert(lhs, new_tup);
                    map.insert(rhs, new_tup);

                    Ok(new_tup)
                } else {
                    Err(PatternMergeFail::Disjoint {
                        left: Some(lhs),
                        right: rhs,
                    })
                }
            }
            (
                PatternNodeKind::Map {
                    keys: k1,
                    values: v1,
                },
                PatternNodeKind::Map {
                    keys: k2,
                    values: v2,
                },
            ) => {
                let new_map = self.node_empty(Some(fused_span));
                self.map(new_map);

                for idx in 0..(k1.len(&self.value_pool)) {
                    let k = k1.get(idx, &self.value_pool).unwrap();
                    let v = v1.get(idx, &self.node_pool).unwrap();
                    self.map_push(new_map, k, v);
                }
                for idx in 0..(k2.len(&self.value_pool)) {
                    let k = k2.get(idx, &self.value_pool).unwrap();
                    let v = v2.get(idx, &self.node_pool).unwrap();
                    self.map_push(new_map, k, v);
                }

                self.node_finish(new_map);

                map.insert(lhs, new_map);
                map.insert(rhs, new_map);

                Ok(new_map)
            }
            (
                PatternNodeKind::List { head: h1, tail: t1 },
                PatternNodeKind::List { head: h2, tail: t2 },
            ) => {
                let new_head = self.merge_patterns(cons, map, h1, h2)?;
                let new_tail = self.merge_patterns(cons, map, t1, t2)?;

                let list = self.node_empty(Some(fused_span));
                self.list(list, new_head, new_tail);

                map.insert(lhs, list);
                map.insert(rhs, list);

                Ok(list)
            }
            // Patterns are incompatible
            _ => Err(PatternMergeFail::Disjoint {
                left: Some(lhs),
                right: rhs,
            }),
        }
    }

    pub fn merge_pattern_constant(
        &mut self,
        cons: &ConstantContainer,
        map: &mut HashMap<PatternNode, PatternNode>,
        fused_span: SourceSpan,
        lhs: PatternNode,
        rhs: Const,
    ) -> Result<PatternNode, PatternMergeFail> {
        let lhs_kind = self.nodes[lhs].kind.clone().unwrap();
        let rhs_kind = cons.const_kind(rhs).clone();

        use crate::{AtomicTerm, ConstKind};

        match (lhs_kind, rhs_kind) {
            (PatternNodeKind::Const(l_const), _) if l_const == rhs => Ok(lhs),
            (PatternNodeKind::Const(_), _) => Err(PatternMergeFail::Disjoint {
                left: None,
                right: lhs,
            }),
            (PatternNodeKind::Wildcard, _) => {
                let node = self.node_empty(Some(fused_span));
                self.constant(node, rhs);
                map.insert(lhs, node);
                Ok(node)
            }
            (
                PatternNodeKind::Tuple(pat_entries),
                ConstKind::Tuple {
                    entries: const_entries,
                },
            ) => {
                let pat_len = pat_entries.len(&self.node_pool);
                let const_len = const_entries.len(&cons.const_pool);

                if pat_len != const_len {
                    return Err(PatternMergeFail::Disjoint {
                        left: None,
                        right: lhs,
                    });
                }

                let node = self.node_empty(Some(fused_span));
                self.tuple(node);

                for n in 0..pat_len {
                    let pat_node = pat_entries.get(n, &self.node_pool).unwrap();
                    let const_node = const_entries.get(n, &cons.const_pool).unwrap();

                    // TODO: Fuse locations
                    let fspan = self.nodes[pat_node].span;
                    let new =
                        self.merge_pattern_constant(cons, map, fspan, pat_node, const_node)?;
                    self.tuple_elem_push(node, new);
                }

                self.node_finish(node);
                map.insert(lhs, node);

                Ok(node)
            }
            (PatternNodeKind::Tuple(_), _) => Err(PatternMergeFail::Disjoint {
                left: None,
                right: lhs,
            }),
            (
                PatternNodeKind::List {
                    head: pat_head,
                    tail: pat_tail,
                },
                ConstKind::ListCell {
                    head: cons_head,
                    tail: cons_tail,
                },
            ) => {
                let head =
                    self.merge_pattern_constant(cons, map, fused_span, pat_head, cons_head)?;
                let tail =
                    self.merge_pattern_constant(cons, map, fused_span, pat_tail, cons_tail)?;

                let node = self.node_empty(Some(fused_span));
                self.list(node, head, tail);
                map.insert(lhs, node);

                Ok(node)
            }
            (PatternNodeKind::List { .. }, _) => Err(PatternMergeFail::Disjoint {
                left: None,
                right: lhs,
            }),
            (PatternNodeKind::Binary { .. }, ConstKind::Atomic(AtomicTerm::Binary(_))) => {
                Err(PatternMergeFail::Failure {
                    left: None,
                    right: lhs,
                })
            }
            (PatternNodeKind::Binary { .. }, _) => Err(PatternMergeFail::Disjoint {
                left: None,
                right: lhs,
            }),
            (a, b) => unimplemented!("{:?} {:?}", a, b),
        }
    }
}
