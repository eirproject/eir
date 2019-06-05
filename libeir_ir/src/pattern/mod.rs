//use std::collections::HashSet;
//use super::{ AtomicTerm };
//
//mod fmt;
//
//#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
//pub struct ValueAssign(pub usize);
//
//#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
//pub struct ValueRef(pub usize);
//
//#[derive(Debug, Clone)]
//pub struct Clause {
//    pub patterns: Vec<Pattern>,
//    pub assigns: Vec<ValueAssign>,
//}
//
//#[derive(Debug, Clone)]
//pub struct Pattern {
//    pub node: PatternNode,
//}
//
//#[derive(Debug, Clone)]
//pub struct BinaryPatternElem {
//    pub node: PatternNode,
//    pub args: Vec<ValueRef>,
//}
//
//#[derive(Debug, Clone)]
//pub enum PatternNode {
//    Wildcard,
//    Assign(ValueAssign, Box<PatternNode>),
//    Atomic(AtomicTerm),
//    Binary(Vec<BinaryPatternElem>),
//    Tuple(Vec<PatternNode>),
//    List(Vec<PatternNode>, Box<PatternNode>),
//    Map(Vec<(ValueRef, Box<PatternNode>)>),
//}

use std::collections::HashMap;

use cranelift_entity::{ PrimaryMap, SecondaryMap, EntityList, ListPool, entity_impl };
use super::{ AtomicTerm };

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

    node_pool: ListPool<PatternNode>,
    value_pool: ListPool<PatternValue>,
    clause_pool: ListPool<PatternClause>,

    /// Temporary map used for copies
    tmp_val_map: Option<HashMap<PatternValue, PatternValue>>,
    tmp_node_map: Option<HashMap<PatternNode, PatternNode>>,
}

#[derive(Debug, Clone)]
struct PatternClauseData {
    root_nodes: EntityList<PatternNode>,
    binds: EntityList<PatternNode>,
    values: EntityList<PatternValue>,
}

#[derive(Debug, Clone)]
struct PatternNodeData {
    kind: PatternNodeKind,
    finished: bool,
}

#[derive(Debug, Clone)]
enum PatternNodeKind {
    Wildcard,
    Atomic(AtomicTerm),
    // TODO: Binary
    Tuple(EntityList<PatternNode>),
    List {
        head: EntityList<PatternNode>,
        tail: PatternNode,
    },
    Map {
        keys: EntityList<PatternValue>,
        values: EntityList<PatternNode>,
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
            clause_pool: ListPool::new(),

            tmp_val_map: Some(HashMap::new()),
            tmp_node_map: Some(HashMap::new()),
        }
    }

    pub fn wildcard(&mut self) -> PatternNode {
        self.nodes.push(PatternNodeData {
            kind: PatternNodeKind::Wildcard,
            finished: true,
        })
    }

    pub fn atomic(&mut self, atomic: AtomicTerm) -> PatternNode {
        self.nodes.push(PatternNodeData {
            kind: PatternNodeKind::Atomic(atomic),
            finished: true,
        })
    }

    pub fn tuple(&mut self) -> PatternNode {
        self.nodes.push(PatternNodeData {
            kind: PatternNodeKind::Tuple(EntityList::new()),
            finished: false,
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

    pub fn list(&mut self, tail: PatternNode) -> PatternNode {
        self.nodes.push(PatternNodeData {
            kind: PatternNodeKind::List { head: EntityList::new(), tail: tail },
            finished: false,
        })
    }

    pub fn list_head_push(&mut self, list: PatternNode, node: PatternNode) {
        let data = &mut self.nodes[list];
        assert!(!data.finished);
        if let PatternNodeKind::List { ref mut head, .. } = data.kind {
            head.push(node, &mut self.node_pool);
        } else {
            panic!();
        }
    }

    pub fn map(&mut self) -> PatternNode {
        self.nodes.push(PatternNodeData {
            kind: PatternNodeKind::Map {
                keys: EntityList::new(),
                values: EntityList::new(),
            },
            finished: false,
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
        })
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
        PatternNodeKind::Atomic(atomic) => to.atomic(atomic.clone()),
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
            let tail_copied = copy_pattern_node(value_map, node_map, *tail, from, to);
            let new = to.list(tail_copied);

            for elem in head.as_slice(&from.node_pool) {
                let copied = copy_pattern_node(value_map, node_map, *elem, from, to);
                to.list_head_push(new, copied);
            }

            to.node_finish(new);
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

