use ::pattern_compiler::{ PatternProvider, ExpandedClauseNodes };
use ::eir::{ AtomicTerm, SSAVariable };
use ::eir::pattern::{ Clause, Pattern, PatternNode };
use ::std::collections::{ HashMap, HashSet };

#[derive(Copy, Clone, Hash, Debug, PartialEq, Eq)]
pub enum NodeKind {
    // Atomic is matched by an interned value
    Atomic(usize),
    // Binary operations are matched by an interned value.
    // When expanded, it expands to the next element in the matching operation.
    Binary(usize, usize),
    // Tuple is matched its length
    TupleSize(usize),
    // A list cell is a singleton
    ListCell,
    // Matches that the value is a map.
    // If there are matches on items within the map,
    // those are expanded to several MapItems.
    Map,
    // A map is matched by an interned value.
    // This matches only a single k=>v mapping in a map.
    MapItem(usize),

    // Meta
    ValueList,
    Wildcard,
}

#[derive(Debug, Clone)]
pub struct Node {
    kind: NodeKind,
    children: Vec<PatternRef>,
}

#[derive(Debug, Clone)]
pub struct ErlangPatternProvider {
    pattern: Vec<Node>,
    roots: Vec<PatternRef>,
    root_var: PatternVar,
    next_var: PatternVar,
    root_ref: PatternRef,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct PatternRef(usize);
#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct PatternVar(usize);

impl ErlangPatternProvider {

    pub fn new() -> Self {
        ErlangPatternProvider {
            pattern: vec![Node { kind: NodeKind::Wildcard, children: vec![]}],
            roots: Vec::new(),
            root_var: PatternVar(0),
            next_var: PatternVar(1),
            root_ref: PatternRef(0),
        }
    }

    pub fn add_child(&mut self, node: PatternRef, kind: NodeKind) -> PatternRef {
        let var = PatternRef(self.pattern.len());
        self.pattern.push(Node {
            kind: kind,
            children: Vec::new(),
        });
        self.pattern[node.0].children.push(var);
        var
    }

    pub fn add_clause(&mut self) -> PatternRef {
        let var = PatternRef(self.pattern.len());
        self.pattern.push(Node {
            kind: NodeKind::ValueList,
            children: Vec::new(),
        });
        self.roots.push(var);
        var
    }

    fn wildcard(&self) -> PatternRef {
        PatternRef(0)
    }

}

impl PatternProvider for ErlangPatternProvider {

    type PatternNodeKey = PatternRef;
    type PatternNodeKind = NodeKind;
    type CfgVariable = PatternVar;

    const WILDCARD: NodeKind = NodeKind::Wildcard;

    fn get_root(&self) -> ExpandedClauseNodes<PatternVar, PatternRef> {
        ExpandedClauseNodes {
            variables: vec![self.root_var],
            clauses: self.roots.len(),
            nodes: self.roots.clone(),
        }
    }

    fn kind_includes(&self, kind: NodeKind, key: PatternRef) -> bool {
        self.pattern[key.0].kind == kind
    }

    fn get_kind(&self, kind: PatternRef) -> NodeKind {
        self.pattern[kind.0].kind
    }

    fn expand_clause_nodes(&mut self, clause_nodes: Vec<PatternRef>)
                           -> ExpandedClauseNodes<PatternVar, PatternRef> {
        //println!("EXPAND: {:?}", clause_nodes);

        if clause_nodes.len() == 0 {
            return ExpandedClauseNodes {
                clauses: 0,
                variables: vec![],
                nodes: vec![],
            };
        }

        println!("ClauseNodes: {:?}", clause_nodes);

        let typ = &self.pattern[clause_nodes[0].0];
        let kind = typ.kind;
        let base_len = typ.children.len();
        for node in &clause_nodes {
            assert!(self.pattern[node.0].kind == kind);
        }

        match kind {
            NodeKind::Map => {
                // Map is a special case in expansion.
                // We want to expand each value into a separate column.

                let mut values_map = HashMap::new();
                for node in &clause_nodes {
                    let sub = &self.pattern[node.0];
                    for child_id in sub.children.iter() {
                        let child = &self.pattern[child_id.0];
                        if let NodeKind::MapItem(idx) = child.kind {
                            values_map.insert((*node, idx), *child_id);
                        } else {
                            unreachable!();
                        }
                    }
                }

                let mut values: Vec<_> = values_map.iter().map(|v| (v.0).1).collect();
                values.sort();
                values.dedup();

                let map_var = self.next_var;
                self.next_var.0 += 1;

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
                for node in &clause_nodes {
                    assert!(self.pattern[node.0].children.len() == base_len);
                }

                let mut next_var = self.next_var;
                let mut exp = ExpandedClauseNodes {
                    clauses: clause_nodes.len(),
                    variables: self.pattern[clause_nodes[0].0]
                        .children.iter()
                        .map(|_| {
                            let r = next_var;
                            next_var.0 += 1;
                            r
                        }).collect(),
                    nodes: vec![],
                };
                self.next_var = next_var;

                for node in clause_nodes {
                    for child in self.pattern[node.0].children.iter() {
                        exp.nodes.push(*child);
                    }
                }

                exp
            }
        }

    }

}

pub struct PatternValueCollector {
    pub atomic_terms: Vec<AtomicTerm>,
    pub node_bindings: HashMap<PatternRef, SSAVariable>,
}
impl PatternValueCollector {
    fn new() -> Self {
        PatternValueCollector {
            atomic_terms: Vec::new(),
            node_bindings: HashMap::new(),
        }
    }
}

fn pattern_node_to_provider(provider: &mut ErlangPatternProvider,
                            collector: &mut PatternValueCollector,
                            node: &PatternNode,
                            parent: PatternRef) -> PatternRef {
    match node {
        PatternNode::Wildcard => {
            provider.add_child(parent, NodeKind::Wildcard)
        }
        PatternNode::Bind(ssa, inner) => {
            let child = pattern_node_to_provider(provider, collector, inner, parent);
            collector.node_bindings.insert(child, *ssa);
            child
        }
        PatternNode::Atomic(atomic_term) => {
            let num = collector.atomic_terms.len();
            collector.atomic_terms.push(atomic_term.clone());
            provider.add_child(parent, NodeKind::Atomic(num))
        }
        PatternNode::Tuple(entries) => {
            let tuple = provider.add_child(parent, NodeKind::TupleSize(entries.len()));
            for entry in entries {
                pattern_node_to_provider(provider, collector, entry, tuple);
            }
            tuple
        }
        PatternNode::List(head, tail) => {
            let mut top = None;
            let mut new_parent = parent;
            for entry in head {
                new_parent = provider.add_child(new_parent, NodeKind::ListCell);
                if top == None {
                    top = Some(new_parent);
                }
                pattern_node_to_provider(
                    provider, collector, entry, new_parent);
            }
            let tail = pattern_node_to_provider(provider, collector, tail, new_parent);
            if let Some(top) = top { top } else { tail }
        }
        PatternNode::Map(entries) => {
            let map_parent = provider.add_child(parent, NodeKind::Map);
            for entry in entries {
                let entry_parent = provider.add_child(
                    map_parent, NodeKind::MapItem(entry.0));
                pattern_node_to_provider(provider, collector, &entry.1, entry_parent);
            }
            map_parent
        }
        _ => unimplemented!(),
    }
}

pub fn pattern_to_provider(clauses: &[Clause]) -> (PatternValueCollector, ErlangPatternProvider) {
    let mut provider = ErlangPatternProvider::new();
    let mut collector = PatternValueCollector::new();
    let mut roots = Vec::new();
    for clause in clauses {
        let node = provider.add_clause();
        roots.push(node);

        for value in clause.patterns.iter() {
            pattern_node_to_provider(&mut provider, &mut collector,
                                     &value.node, node);
        }
    }
    (collector, provider)
}









