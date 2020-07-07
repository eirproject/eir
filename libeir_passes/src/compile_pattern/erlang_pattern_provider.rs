use cranelift_entity::{entity_impl, EntityList, ListPool, PrimaryMap};
use libeir_util_pattern_compiler::{ExpandedClauseNodes, PatternProvider};

use hashbrown::HashMap;
use std::collections::BTreeMap;

use libeir_ir::constant::Const;
use libeir_ir::pattern::{
    PatternClause, PatternContainer, PatternNode, PatternNodeKind, PatternValue,
};
use libeir_ir::BinaryEntrySpecifier;
use libeir_ir::{Function, PrimOp, Value, ValueKind};

use super::ValueBind;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub enum ValueOrConst {
    Value(Value),
    Const(Const),
    PrimOp(PrimOp),
    Node(PatternNode),
}
impl ValueOrConst {
    fn from_value(val_bind: ValueBind, fun: &Function) -> Self {
        match val_bind {
            ValueBind::Value(val) => match fun.value_kind(val) {
                ValueKind::Const(cons) => ValueOrConst::Const(cons),
                ValueKind::Argument(_, _) => ValueOrConst::Value(val),
                ValueKind::PrimOp(prim) => ValueOrConst::PrimOp(prim),
                kind => panic!("{:?}", kind),
            },
            ValueBind::Node(node) => ValueOrConst::Node(node),
        }
    }
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum NodeKind {
    /// Matched on equality to value
    Value(ValueOrConst),

    /// Binary operations take an optional size value.
    /// When expanded, it expands to the value node, and the tail of the
    /// binary.
    Binary {
        specifier: BinaryEntrySpecifier,
        size: Option<ValueOrConst>,
    },
    /// Tuple is matched its length
    TupleSize(usize),
    /// A list cell is a singleton
    ListCell,
    /// Matches that the value is a map.
    /// If there are matches on items within the map,
    /// those are expanded to several MapItems.
    Map,
    /// A map is matched by an interned value.
    /// This matches only a single k=>v mapping in a map.
    MapItem(ValueOrConst),

    // Meta
    ValueList,
    Wildcard,
}

impl NodeKind {
    pub fn num_children(self) -> usize {
        match self {
            NodeKind::Value(_) => 0,
            NodeKind::TupleSize(num) => num,
            NodeKind::ListCell => 2,
            NodeKind::Map => panic!(),
            NodeKind::MapItem(_) => 1,
            NodeKind::ValueList => panic!(),
            NodeKind::Wildcard => 0,
            NodeKind::Binary { .. } => 2,
        }
    }
}

#[derive(Debug, Clone)]
pub struct NodeData {
    kind: NodeKind,
    children: EntityList<Node>,
}

#[derive(Debug, Clone)]
pub struct ErlangPatternProvider<'a> {
    fun: Option<&'a Function>,

    nodes: PrimaryMap<Node, NodeData>,
    vars: PrimaryMap<Var, ()>,

    /// Root node for each clause
    root_nodes: Vec<Node>,
    /// Root variable in the match
    root_var: Var,

    wildcard: Node,

    node_pool: ListPool<Node>,

    node_map: BTreeMap<PatternNode, Node>,
}

#[derive(Copy, Clone, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub struct Node(u32);
entity_impl!(Node, "node");

#[derive(Copy, Clone, Eq, PartialEq, Hash, PartialOrd, Ord)]
pub struct Var(u32);
entity_impl!(Var, "var");

impl<'a> ErlangPatternProvider<'a> {
    pub fn new() -> Self {
        let mut nodes = PrimaryMap::new();
        let wildcard = nodes.push(NodeData {
            kind: NodeKind::Wildcard,
            children: EntityList::new(),
        });

        let mut vars = PrimaryMap::new();
        let root_var = vars.push(());

        ErlangPatternProvider {
            fun: None,

            nodes,
            vars,

            root_nodes: Vec::new(),
            root_var,

            wildcard,

            node_pool: ListPool::new(),

            node_map: BTreeMap::new(),
        }
    }

    pub fn add_child(&mut self, node: Node, kind: NodeKind, pat_node: PatternNode) -> Node {
        let next = self.nodes.push(NodeData {
            kind: kind,
            children: EntityList::new(),
        });
        self.nodes[node].children.push(next, &mut self.node_pool);
        self.node_map.insert(pat_node, next);
        next
    }

    pub fn add_clause(&mut self) -> Node {
        let next = self.nodes.push(NodeData {
            kind: NodeKind::ValueList,
            children: EntityList::new(),
        });
        self.root_nodes.push(next);
        next
    }

    fn wildcard(&self) -> Node {
        self.wildcard
    }

    pub fn pattern_node_to_cfg_node(&self, pat: PatternNode) -> Node {
        *self.node_map.get(&pat).unwrap()
    }
}

impl<'a> PatternProvider for ErlangPatternProvider<'a> {
    type PatternNodeKey = Node;
    type PatternNodeKind = NodeKind;
    type CfgVariable = Var;

    const WILDCARD: NodeKind = NodeKind::Wildcard;

    fn get_root(&self) -> ExpandedClauseNodes<Var, Node> {
        ExpandedClauseNodes {
            variables: vec![self.root_var],
            clauses: self.root_nodes.len(),
            nodes: self.root_nodes.clone(),
        }
    }

    fn kind_includes(&self, kind: NodeKind, key: Node) -> bool {
        let node_kind = self.nodes[key].kind;
        if node_kind == kind {
            return true;
        }

        //match (kind, node_kind) {
        //    (NodeKind::Value(l), NodeKind::Value(r)) =>
        //        if l
        //    _ => false,
        //}
        false
    }

    fn get_kind(&self, key: Node) -> NodeKind {
        self.nodes[key].kind
    }

    fn get_wildcard_node(&self) -> Node {
        self.wildcard
    }

    fn expand_clause_nodes(
        &mut self,
        clause_nodes: Vec<Node>,
        kind: NodeKind,
    ) -> ExpandedClauseNodes<Var, Node> {
        if clause_nodes.len() == 0 {
            return ExpandedClauseNodes {
                clauses: 0,
                variables: vec![],
                nodes: vec![],
            };
        }

        for node in clause_nodes.iter() {
            let node_kind = self.nodes[*node].kind;
            assert!(node_kind == kind);
        }

        //let typ = &self.nodes[clause_nodes[0]];
        //let kind = typ.kind;
        //let base_len = typ.children.len(&self.node_pool);
        //for node in &clause_nodes {
        //    assert!(self.nodes[*node].kind == kind);
        //}

        match kind {
            NodeKind::ValueList => {
                let len = self.nodes[clause_nodes[0]].children.len(&self.node_pool);

                let mut exp = ExpandedClauseNodes {
                    clauses: clause_nodes.len(),
                    variables: {
                        let vars = &mut self.vars;
                        (0..len).map(|_| vars.push(())).collect()
                    },
                    nodes: vec![],
                };

                for node_id in clause_nodes {
                    let node = &self.nodes[node_id];
                    let children = node.children.as_slice(&self.node_pool);

                    assert!(children.len() == len);

                    for child in children {
                        exp.nodes.push(*child);
                    }
                }

                exp
            }
            NodeKind::Map => {
                // Map is a special case in expansion.
                // We want to expand each value into a separate column.

                let mut values_map = HashMap::new();
                for node in &clause_nodes {
                    let sub = &self.nodes[*node];
                    for child_id in sub.children.as_slice(&self.node_pool) {
                        let child = &self.nodes[*child_id];
                        if let NodeKind::MapItem(idx) = child.kind {
                            let key = (*node, idx);
                            if !values_map.contains_key(&key) {
                                values_map.insert(key, Vec::new());
                            }
                            values_map.get_mut(&key).unwrap().push(*child_id);
                        } else {
                            unreachable!();
                        }
                    }
                }

                let mut values = BTreeMap::new();
                for ((_node, key_value), val) in values_map.iter() {
                    if let Some(max) = values.get_mut(key_value) {
                        let n_max = val.len();
                        if n_max > *max {
                            *max = n_max;
                        }
                    } else {
                        values.insert(*key_value, val.len());
                    }
                }

                let value_map = self.vars.push(());
                let mut exp = ExpandedClauseNodes {
                    clauses: clause_nodes.len(),
                    variables: values
                        .values()
                        .flat_map(|num| 0..*num)
                        .map(|_| value_map)
                        .collect(),
                    nodes: vec![],
                };

                for node in clause_nodes.iter() {
                    for (value, num) in values.iter() {
                        let vals = values_map.get(&(*node, *value));
                        for n in 0..*num {
                            if let Some(child_id) = vals.and_then(|v| v.get(n)) {
                                exp.nodes.push(*child_id);
                            } else {
                                exp.nodes.push(self.wildcard());
                            }
                        }
                    }
                }

                exp
            }
            _ => {
                let expected_children = kind.num_children();

                let mut exp = ExpandedClauseNodes {
                    clauses: clause_nodes.len(),
                    variables: {
                        let vars = &mut self.vars;
                        (0..expected_children).map(|_| vars.push(())).collect()
                    },
                    nodes: vec![],
                };

                for node_id in clause_nodes {
                    let node = &self.nodes[node_id];
                    let children = node.children.as_slice(&self.node_pool);

                    assert!(children.len() == expected_children);
                    for child in children {
                        exp.nodes.push(*child);
                    }
                }

                exp
            }
        }
    }
}

fn pattern_node_to_provider(
    fun: &Function,
    pat: &PatternContainer,
    //node_map: &mut HashMap<PatternNode, Node>,
    value_map: &HashMap<PatternValue, ValueBind>,
    provider: &mut ErlangPatternProvider,
    node: PatternNode,
    parent: Node,
    n: usize,
) -> Node {
    // Cheap and safe since we don't modify any patterns
    let kind = pat.node_kind(node).clone();

    match &kind {
        PatternNodeKind::Wildcard => provider.add_child(parent, NodeKind::Wildcard, node),
        PatternNodeKind::Tuple(entries) => {
            let tuple = provider.add_child(
                parent,
                NodeKind::TupleSize(entries.len(&pat.node_pool)),
                node,
            );
            for entry_num in 0..(entries.len(&pat.node_pool)) {
                let entry = entries.get(entry_num, &pat.node_pool).unwrap();
                pattern_node_to_provider(fun, pat, value_map, provider, entry, tuple, n + 1);
            }
            tuple
        }
        PatternNodeKind::List { head, tail } => {
            let node = provider.add_child(parent, NodeKind::ListCell, node);
            pattern_node_to_provider(fun, pat, value_map, provider, *head, node, n + 1);
            pattern_node_to_provider(fun, pat, value_map, provider, *tail, node, n + 1);
            node
        }
        PatternNodeKind::Map { keys, values } => {
            let map_parent = provider.add_child(parent, NodeKind::Map, node);

            let len = keys.len(&pat.value_pool);
            assert!(values.len(&pat.node_pool) == len);

            let mut dedup = HashMap::new();
            for n in 0..len {
                let key = keys.get(n, &pat.value_pool).unwrap();
                let key_val_or_const = ValueOrConst::from_value(value_map[&key], fun);

                let val = values.get(n, &pat.node_pool).unwrap();

                if !dedup.contains_key(&key_val_or_const) {
                    dedup.insert(key_val_or_const, Vec::new());
                }
                dedup.get_mut(&key_val_or_const).unwrap().push(val);
            }

            // TODO: Currently we just give up here and put duplicate
            // values in separate columns. We most likely want to take
            // care of merging these in a separate stage within pattern
            // match compilation.
            for (key, values) in dedup.iter() {
                for value in values {
                    let entry_parent =
                        provider.add_child(map_parent, NodeKind::MapItem(*key), node);
                    pattern_node_to_provider(
                        fun,
                        pat,
                        value_map,
                        provider,
                        *value,
                        entry_parent,
                        n + 1,
                    );
                }
            }

            map_parent
        }
        PatternNodeKind::Const(cons) => {
            let val = ValueOrConst::Const(*cons);
            provider.add_child(parent, NodeKind::Value(val), node)
        }
        PatternNodeKind::Value(pat_val) => {
            let val = value_map[&pat_val];
            let val_or_const = ValueOrConst::from_value(val, fun);
            provider.add_child(parent, NodeKind::Value(val_or_const), node)
        }
        PatternNodeKind::Binary {
            specifier,
            value,
            size,
            remaining,
        } => {
            let size_val = size.map(|v| ValueOrConst::from_value(value_map[&v], fun));

            let binary_entry = provider.add_child(
                parent,
                NodeKind::Binary {
                    specifier: *specifier,
                    size: size_val,
                },
                node,
            );

            pattern_node_to_provider(fun, pat, value_map, provider, *value, binary_entry, n + 1);
            pattern_node_to_provider(
                fun,
                pat,
                value_map,
                provider,
                *remaining,
                binary_entry,
                n + 1,
            );

            binary_entry
        }
    }
}

pub(super) fn pattern_to_provider<'a>(
    fun: &Function,
    pat: &PatternContainer,
    clauses: &[PatternClause],
    //node_map: &mut HashMap<PatternNode, Node>,
    value_map: &HashMap<PatternValue, ValueBind>,
) -> ErlangPatternProvider<'a> {
    let mut provider = ErlangPatternProvider::new();
    let mut roots = Vec::new();
    for clause in clauses {
        let node = provider.add_clause();
        roots.push(node);

        for value_num in 0..pat.clause_root_nodes(*clause).len() {
            let value = pat.clause_root_nodes(*clause)[value_num];
            pattern_node_to_provider(fun, pat, value_map, &mut provider, value, node, 0);
        }
    }
    provider
}
