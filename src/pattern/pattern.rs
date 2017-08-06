use ::petgraph::{ Graph, Direction };
use ::petgraph::graph::NodeIndex;

pub type PatternNodeIndex = NodeIndex;

pub type PatternGraph = Graph<PatternNodeO, ()>;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct PatternVariable(pub usize);

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum PatternNodeKind {
    ListCell,
    Sentinel,
    Tuple(usize),
    Map(),
    Wildcard,
}
impl PatternNodeKind {

    pub fn children(&self) -> usize {
        match *self {
            PatternNodeKind::Wildcard => 0,
            PatternNodeKind::Sentinel => 0,
            PatternNodeKind::Tuple(arity) => arity,
            PatternNodeKind::ListCell => 2,
            _ => unimplemented!(),
        }
    }

}

#[derive(Debug)]
pub struct Pattern {
    curr_var: usize,
    graph: PatternGraph,
    root: Vec<PatternNodeIndex>,
    variables_len: usize,
    clauses_len: usize,
    wildcard: PatternNodeIndex,
}

#[derive(Clone)]
pub struct PatternNodeO {
    pub kind: PatternNodeKind,
    pub bind: PatternVariable,
}
impl ::std::fmt::Debug for PatternNodeO {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(f, "{:?} V{}", self.kind, self.bind.0)
    }
}

impl Pattern {

    pub fn new(variables: usize) -> Self {
        let mut graph = Graph::new();
        let wildcard = graph.add_node(PatternNodeO {
            kind: PatternNodeKind::Wildcard,
            bind: PatternVariable(0),
        });
        Pattern {
            curr_var: 0,
            graph: graph,
            root: Vec::new(),
            variables_len: variables,
            clauses_len: 0,
            wildcard: wildcard,
        }
    }

    pub fn wildcard(&self) -> PatternNodeIndex {
        self.wildcard
    }

    // Temporary
    pub fn clauses(&self) -> Vec<Vec<PatternNodeIndex>> {
        self.root
            .chunks(self.variables_len)
            .map(|clause| clause.into())
            .collect()
    }

    pub fn node(&self, idx: PatternNodeIndex) -> &PatternNodeO {
        &self.graph[idx]
    }

    pub fn node_children(&self, idx: PatternNodeIndex
    ) -> ::petgraph::graph::Neighbors<()> {
        self.graph.neighbors_directed(idx, Direction::Outgoing)
    }

    pub fn add_node(&mut self, node: PatternNodeKind) -> PatternNodeIndex {
        self.curr_var += 1;
        self.graph.add_node(PatternNodeO {
            kind: node,
            bind: PatternVariable(self.curr_var),
        })
    }

    pub fn add_child(&mut self, parent: PatternNodeIndex, child: PatternNodeKind) -> PatternNodeIndex {
        self.curr_var += 1;
        let res = self.graph.add_node(PatternNodeO {
            kind: child,
            bind: PatternVariable(self.curr_var),
        });
        self.graph.add_edge(parent, res, ());
        res
    }

    pub fn add_clause(&mut self, patterns: Vec<PatternNodeIndex>) {
        assert!(patterns.len() == self.variables_len);
        for pattern in patterns.iter() {
            self.validate_node(*pattern);
        }
        self.root.extend(patterns.iter());
        self.clauses_len += 1;
    }

    fn validate_node(&self, node: PatternNodeIndex) {
        let outgoing = self.graph.neighbors_directed(node, Direction::Outgoing);

        let mut child_count = 0;
        for child in outgoing {
            child_count += 1;
            self.validate_node(child);
        }

        assert!(child_count == self.graph[node].kind.children());
    }

    pub fn dimentions(&self) -> (usize, usize) {
        (self.variables_len, self.clauses_len)
    }

}
