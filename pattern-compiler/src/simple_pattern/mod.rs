#[cfg(test)]
mod test;

use ::{ PatternProvider, ExpandedClauseNodes };

use ::petgraph::{ Graph, Direction };
use ::petgraph::graph::NodeIndex;

#[derive(Copy, Clone, Hash, PartialEq, Eq)]
struct CfgVar(usize);

use ::std::fmt;
impl fmt::Debug for CfgVar {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        write!(fmt, "${}", self.0)
    }
}

#[derive(Copy, Clone, Hash, Debug, PartialEq, Eq)]
enum NodeKind {
    Tuple,
    ListCell,
    Terminal,
    RootValues,
    Wildcard,
}

#[derive(Debug, Clone)]
struct SimplePatternProvider {
    pattern: Graph<NodeKind, ()>,
    roots: Vec<NodeIndex>,
    root_var: CfgVar,
    curr_var: CfgVar,
}

impl SimplePatternProvider {

    fn new() -> Self {
        SimplePatternProvider {
            pattern: Graph::new(),
            roots: Vec::new(),
            root_var: CfgVar(0),
            curr_var: CfgVar(0),
        }
    }

    fn add_child(&mut self, node: NodeIndex, kind: NodeKind) -> NodeIndex {
        let res = self.pattern.add_node(kind);
        self.pattern.add_edge(node, res, ());
        res
    }

    fn add_clause(&mut self, kind: NodeKind) -> NodeIndex {
        let res = self.pattern.add_node(kind);
        self.roots.push(res);
        res
    }
}

impl PatternProvider for SimplePatternProvider {

    type PatternNodeKey = NodeIndex;
    type PatternNodeKind = NodeKind;
    type CfgVariable = CfgVar;

    const WILDCARD: NodeKind = NodeKind::Wildcard;

    fn get_root(&self) -> ExpandedClauseNodes<
            Self::CfgVariable, Self::PatternNodeKey> {
        ExpandedClauseNodes {
            variables: vec![self.root_var],
            clauses: self.roots.len(),
            nodes: self.roots.clone(),
        }
    }

    fn kind_includes(&self, kind: Self::PatternNodeKind,
                     key: Self::PatternNodeKey) -> bool {
        self.pattern[key] == kind
    }

    fn expand_clause_nodes(&mut self, clause_nodes: Vec<Self::PatternNodeKey>)
                           -> ExpandedClauseNodes<
            Self::CfgVariable, Self::PatternNodeKey>
    {
        if clause_nodes.len() == 0 {
            return ExpandedClauseNodes {
                clauses: 0,
                variables: vec![],
                nodes: vec![],
            };
        }

        let typ = self.pattern[clause_nodes[0]];
        let base_len = self.pattern.edges_directed(clause_nodes[0], Direction::Outgoing).count();
        for node in &clause_nodes {
            assert!(self.pattern.edges_directed(clause_nodes[0], Direction::Outgoing).count()
                    == base_len);
            assert!(self.pattern[*node] == typ);
        }

        let mut curr_var = self.curr_var;
        let mut exp = ExpandedClauseNodes {
            clauses: clause_nodes.len(),
            variables: self.pattern
                .edges_directed(clause_nodes[0], Direction::Outgoing)
                .map(|_| {
                    curr_var.0 += 1;
                    curr_var
                })
                .collect(),
            nodes: vec![],
        };
        self.curr_var = curr_var;

        match typ {
            NodeKind::RootValues => {
                for node in &clause_nodes {
                    for child in self.pattern.edges_directed(*node, Direction::Outgoing) {
                        use ::petgraph::visit::EdgeRef;
                        exp.nodes.push(child.target());
                    }
                }
            },
            NodeKind::ListCell => {
                for node in &clause_nodes {
                    for child in self.pattern.edges_directed(*node, Direction::Outgoing) {
                        use ::petgraph::visit::EdgeRef;
                        exp.nodes.push(child.target());
                    }
                }
            },
            NodeKind::Wildcard => {},
            NodeKind::Terminal => {},
            typ => unimplemented!("{:?}", typ),
        }

        println!("{:?}", exp);
        exp
    }

    fn get_kind(&self, key: Self::PatternNodeKey) -> Self::PatternNodeKind {
        self.pattern[key]
    }

}
