use cranelift_entity::{ PrimaryMap, EntityList, ListPool, entity_impl };
use ::pattern_compiler::{ PatternProvider, ExpandedClauseNodes };

use std::collections::{ HashMap, HashSet };
use std::hash::{ Hash, Hasher };

use libeir_ir::{ Function, FunctionBuilder, Value, ValueKind };
use libeir_ir::pattern::{ PatternContainer, PatternClause, PatternNode, PatternNodeKind, PatternValue };
use libeir_ir::constant::{ Const };

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub enum ValueOrConst {
    Value(Value),
    /// ConstValue is used for hashing and equality, Const is used for span
    Const(Const),
}
impl ValueOrConst {
    pub fn from_value(val: Value, b: &FunctionBuilder) -> Self {
        match b.fun().value_kind(val) {
            ValueKind::Const(cons) => {
                ValueOrConst::Const(cons)
            }
            ValueKind::Argument(_, _) => {
                ValueOrConst::Value(val)
            }
            kind => panic!("{:?}", kind),
        }
    }
    pub fn to_value(self, b: &mut FunctionBuilder) -> Value {
        match self {
            ValueOrConst::Value(val) => val,
            ValueOrConst::Const(cons) => b.value(cons),
        }
    }
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq)]
pub enum NodeKind {
    /// Matched on equality to value
    Value(ValueOrConst),

    /// Binary operations are matched by an interned value.
    /// When expanded, it expands to the next element in the matching operation.
    Binary(usize, usize),
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
            _ => unimplemented!(),
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
        }
    }

    pub fn add_child(&mut self, node: Node, kind: NodeKind) -> Node {
        let next = self.nodes.push(NodeData {
            kind: kind,
            children: EntityList::new(),
        });
        self.nodes[node].children.push(next, &mut self.node_pool);
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
        node_kind == kind
    }

    fn get_kind(&self, key: Node) -> NodeKind {
        self.nodes[key].kind
    }

    fn get_wildcard_node(&self) -> Node {
        self.wildcard
    }

    fn expand_clause_nodes(&mut self, clause_nodes: Vec<Node>, kind: NodeKind)
                           -> ExpandedClauseNodes<Var, Node> {
        //println!("EXPAND: {:?}", clause_nodes);

        if clause_nodes.len() == 0 {
            return ExpandedClauseNodes {
                clauses: 0,
                variables: vec![],
                nodes: vec![],
            };
        }

        //println!("ClauseNodes: {:?}", clause_nodes);

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

                panic!();

                let mut values_map = HashMap::new();
                for node in &clause_nodes {
                    let sub = &self.nodes[*node];
                    for child_id in sub.children.as_slice(&self.node_pool) {
                        let child = &self.nodes[*child_id];
                        if let NodeKind::MapItem(idx) = child.kind {
                            values_map.insert((*node, idx), *child_id);
                        } else {
                            unreachable!();
                        }
                    }
                }

                let mut values: HashSet<_> = values_map.iter()
                    .map(|v| (v.0).1).collect();

                let map_var = self.vars.push(());

                let mut exp = ExpandedClauseNodes {
                    clauses: clause_nodes.len(),
                    variables: (0..values.len()).map(|_| map_var).collect(),
                    nodes: vec![],
                };

                for node in clause_nodes.iter() {
                    for value in values.iter() {
                        if let Some(child_id) = values_map.get(&(*node, *value)) {
                            exp.nodes.push(*child_id);
                        } else {
                            exp.nodes.push(self.wildcard());
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
                        (0..expected_children)
                            .map(|_| vars.push(())).collect()
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
    b: &mut FunctionBuilder,
    node_map: &mut HashMap<PatternNode, Node>,
    value_map: &HashMap<PatternValue, Value>,
    provider: &mut ErlangPatternProvider,
    node: PatternNode,
    parent: Node,
    n: usize,
) -> Node
{
    // Cheap and safe since we don't modify any patterns
    let kind = b.pat().node_kind(node).clone();

    let res_node = match &kind {
        PatternNodeKind::Wildcard => {
            provider.add_child(parent, NodeKind::Wildcard)
        }
        //PatternNodeKind::Assign(assign, inner) => {
        //    let child = pattern_node_to_provider(provider, collector, inner, parent);
        //    if !collector.node_bindings.contains_key(&child) {
        //        collector.node_bindings.insert(child, vec![]);
        //    }
        //    collector.node_bindings.get_mut(&child).unwrap().push(*assign);
        //    child
        //}
        //PatternNodeKind::Value(atomic_term) => {
        //    let num = collector.atomic_terms.len();
        //    collector.atomic_terms.push(atomic_term.clone());
        //    provider.add_child(parent, NodeKind::Atomic(num))
        //}
        PatternNodeKind::Tuple(entries) => {
            let tuple = provider.add_child(parent, NodeKind::TupleSize(entries.len(&b.pat().node_pool)));
            for entry_num in 0..(entries.len(&b.pat().node_pool)) {
                let entry = entries.get(entry_num, &b.pat().node_pool).unwrap();
                pattern_node_to_provider(b, node_map, value_map, provider, entry, tuple, n+1);
            }
            tuple
        }
        PatternNodeKind::List{ head, tail } => {
            let node = provider.add_child(parent, NodeKind::ListCell);
            pattern_node_to_provider(b, node_map, value_map, provider, *head, node, n+1);
            pattern_node_to_provider(b, node_map, value_map, provider, *tail, node, n+1);
            node
        }
        PatternNodeKind::Map { keys, values } => {
            let map_parent = provider.add_child(parent, NodeKind::Map);

            let len = keys.len(&b.pat().value_pool);
            assert!(values.len(&b.pat().node_pool) == len);

            for n in 0..len {
                let key = keys.get(n, &b.pat().value_pool).unwrap();
                let key_val_or_const = ValueOrConst::from_value(value_map[&key], b);

                let val = values.get(n, &b.pat().node_pool).unwrap();

                let entry_parent = provider.add_child(
                    map_parent, NodeKind::MapItem(key_val_or_const));
                pattern_node_to_provider(b, node_map, value_map, provider, val, entry_parent, n+1);
            }
            map_parent
        }
        PatternNodeKind::Value(pat_val) => {
            let val = value_map[&pat_val];
            let val_or_const = ValueOrConst::from_value(val, b);
            provider.add_child(parent, NodeKind::Value(val_or_const))
        }
        k => unimplemented!("{:?}", k),
    };

    println!("{} {} {:?}", res_node, n, kind);

    node_map.insert(node, res_node);
    res_node
}

pub fn pattern_to_provider<'a>(b: &mut FunctionBuilder, clauses: &[PatternClause],
                               node_map: &mut HashMap<PatternNode, Node>,
                               value_map: &HashMap<PatternValue, Value>) -> ErlangPatternProvider<'a> {
    let mut provider = ErlangPatternProvider::new();
    let mut roots = Vec::new();
    for clause in clauses {
        println!("CL: {}", clause);
        let node = provider.add_clause();
        roots.push(node);

        for value_num in 0..b.pat().clause_root_nodes(*clause).len() {
            let value = b.pat().clause_root_nodes(*clause)[value_num];
            pattern_node_to_provider(b, node_map, value_map, &mut provider, value, node, 0);
        }
    }
    provider
}









