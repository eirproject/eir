use std::collections::HashMap;

use cranelift_entity::{ PrimaryMap, EntityList, ListPool, entity_impl };

use libeir_diagnostics::{ ByteSpan, DUMMY_SPAN };

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

#[derive(Debug, Clone)]
struct PatternClauseData {
    root_nodes: EntityList<PatternNode>,

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
    kind: PatternNodeKind,
    finished: bool,
    span: ByteSpan,
}

#[derive(Debug, Clone)]
pub enum PatternNodeKind {
    Wildcard,
    Value(PatternValue),
    // TODO: Binary
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

pub enum PatternMergeFail {
    /// The two patterns are disjoint, they can never match at the
    /// same time.
    /// The two nodes are the reason for this.
    Disjoint {
        left: PatternNode,
        right: PatternNode,
    },
    /// Merging is not supported. Only happens with binaries
    /// The two nodes are the reason for this.
    Failure {
        left: PatternNode,
        right: PatternNode,
    },
}

impl PatternContainer {

    pub fn new() -> Self {
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

    pub fn clause_value(&mut self, clause: PatternClause) -> PatternValue {
        let val = self.values.push(());
        self.clauses[clause].values.push(val, &mut self.value_pool);
        val
    }

    pub fn wildcard(&mut self) -> PatternNode {
        self.nodes.push(PatternNodeData {
            kind: PatternNodeKind::Wildcard,
            finished: true,
            span: DUMMY_SPAN,
        })
    }

    pub fn value(&mut self, val: PatternValue) -> PatternNode {
        self.nodes.push(PatternNodeData {
            kind: PatternNodeKind::Value(val),
            finished: true,
            span: DUMMY_SPAN,
        })
    }

    pub fn tuple(&mut self) -> PatternNode {
        self.nodes.push(PatternNodeData {
            kind: PatternNodeKind::Tuple(EntityList::new()),
            finished: false,
            span: DUMMY_SPAN,
        })
    }

    pub fn tuple_elem_push(&mut self, tup: PatternNode, node: PatternNode) {
        let data = &mut self.nodes[tup];
        assert!(!data.finished);
        if let PatternNodeKind::Tuple(ref mut list) = data.kind {
            list.push(node, &mut self.node_pool);
        } else {
            panic!();
        }
    }

    pub fn list(&mut self, head: PatternNode, tail: PatternNode) -> PatternNode {
        self.nodes.push(PatternNodeData {
            kind: PatternNodeKind::List { head, tail },
            finished: true,
            span: DUMMY_SPAN,
        })
    }

    pub fn map(&mut self) -> PatternNode {
        self.nodes.push(PatternNodeData {
            kind: PatternNodeKind::Map {
                keys: EntityList::new(),
                values: EntityList::new(),
            },
            finished: false,
            span: DUMMY_SPAN,
        })
    }

    pub fn map_push(&mut self, map: PatternNode, key: PatternValue, value: PatternNode) {
        let data = &mut self.nodes[map];
        assert!(!data.finished);
        if let PatternNodeKind::Map { ref mut keys, ref mut values } = data.kind {
            keys.push(key, &mut self.value_pool);
            values.push(value, &mut self.node_pool);
        } else {
            panic!();
        }
    }

    pub fn node_finish(&mut self, node: PatternNode) {
        self.nodes[node].finished = true;
    }

    pub fn node_set_span(&mut self, node: PatternNode, span: ByteSpan) {
        self.nodes[node].span = span;
    }

    pub fn clause_start(&mut self) -> PatternClause {
        self.clauses.push(PatternClauseData {
            root_nodes: EntityList::new(),
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

    pub fn merge(&mut self, map: &mut HashMap<PatternNode, PatternNode>,
                 lhs: PatternNode, rhs: PatternNode) -> Result<PatternNode, PatternMergeFail>
    {
        // This clone is really cheap since entitylists are basically just an usize
        let lhs_kind = self.nodes[lhs].kind.clone();
        let rhs_kind = self.nodes[rhs].kind.clone();

        match (lhs_kind, rhs_kind) {
            (PatternNodeKind::Wildcard, _) => {
                map.insert(lhs, rhs);
                Ok(rhs)
            },
            (_, PatternNodeKind::Wildcard) => {
                map.insert(rhs, lhs);
                Ok(lhs)
            },
            (PatternNodeKind::Tuple(l1), PatternNodeKind::Tuple(l2)) => {
                let l1_len = l1.len(&self.node_pool);
                let l2_len = l2.len(&self.node_pool);
                if l1_len == l2_len {
                    let new_tup = self.tuple();
                    for idx in 0..l1_len {
                        let ln = l1.get(idx, &self.node_pool).unwrap();
                        let rn = l2.get(idx, &self.node_pool).unwrap();

                        let merged = self.merge(map, ln, rn)?;

                        self.tuple_elem_push(new_tup, merged);
                    }
                    self.node_finish(new_tup);

                    map.insert(lhs, new_tup);
                    map.insert(rhs, new_tup);

                    Ok(new_tup)
                } else {
                    Err(PatternMergeFail::Disjoint {
                        left: lhs,
                        right: rhs,
                    })
                }
            }
            (PatternNodeKind::Map { keys: k1, values: v1 }, PatternNodeKind::Map { keys: k2, values: v2 }) => {
                let new_map = self.map();

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
            (PatternNodeKind::List { head: h1, tail: t1 }, PatternNodeKind::List { head: h2, tail: t2 }) => {
                let new_head = self.merge(map, h1, h2)?;
                let new_tail = self.merge(map, t1, t2)?;
                Ok(self.list(new_head, new_tail))
            }
            // Patterns are incompatible
            _ => {
                Err(PatternMergeFail::Disjoint {
                    left: lhs,
                    right: rhs,
                })
            },
        }
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

        self.clauses.push(PatternClauseData {
            root_nodes: new_roots,
            binds: new_binds,
            values: new_values,

            finished: true,
        })
    }

    /// Given a HashMap containing the mapping, this will go through all the
    /// binds of a clause and update them. Useful when merging nodes while
    /// constructing patterns.
    pub fn update_binds(&mut self, clause: PatternClause,
                        map: &HashMap<PatternNode, PatternNode>)
    {
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

    pub fn node_span(&self, node: PatternNode) -> ByteSpan {
        self.nodes[node].span
    }

    pub fn node_kind<'a>(&'a self, node: PatternNode) -> &'a PatternNodeKind {
        &self.nodes[node].kind
    }

}

fn copy_pattern_node(
    value_map: &HashMap<PatternValue, PatternValue>,
    node_map: &mut HashMap<PatternNode, PatternNode>,
    node: PatternNode,
    from: &PatternContainer, to: &mut PatternContainer) -> PatternNode
{
    let data = &from.nodes[node];
    match &data.kind {
        PatternNodeKind::Wildcard => {
            let new = to.wildcard();
            node_map.insert(node, new);
            new
        },
        PatternNodeKind::Value(val) => {
            let new = to.value(*val);
            node_map.insert(node, new);
            new
        }
        PatternNodeKind::Tuple(elems) => {
            let new = to.tuple();

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
            let new = to.list(head_copied, tail_copied);

            node_map.insert(node, new);
            new
        }
        PatternNodeKind::Map { keys, values } => {
            let new = to.map();

            for (key, val) in keys.as_slice(&from.value_pool).iter()
                .zip(values.as_slice(&from.node_pool))
            {
                let copied = copy_pattern_node(value_map, node_map, *val, from, to);
                to.map_push(new, value_map[key], copied);
            }

            to.node_finish(new);
            node_map.insert(node, new);
            new
        }
    }
}

