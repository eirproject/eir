use ::std::slice::{ Chunks, ChunksMut };

use ::petgraph::{ Graph, Direction };
use ::petgraph::graph::NodeIndex;

pub type PatternNodeIndex = NodeIndex;

pub type PatternGraph = Graph<PatternNode, ()>;

#[derive(Debug, PartialEq, Eq, Clone, Hash)]
pub enum PatternNode {
    ListCell,
    Sentinel,
    Tuple(usize),
    Map(),
    Wildcard,
}
impl PatternNode {

    pub fn children(&self) -> usize {
        match *self {
            PatternNode::Wildcard => 0,
            PatternNode::Sentinel => 0,
            PatternNode::Tuple(arity) => arity,
            PatternNode::ListCell => 2,
            _ => unimplemented!(),
        }
    }

}

#[derive(Debug)]
pub struct Pattern {
    graph: PatternGraph,
    root: Vec<PatternNodeIndex>,
    variables_len: usize,
    clauses_len: usize,
    wildcard: PatternNodeIndex,
}

impl Pattern {

    pub fn new(variables: usize) -> Self {
        let mut graph = Graph::new();
        let wildcard = graph.add_node(PatternNode::Wildcard);
        Pattern {
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

    pub fn node(&self, idx: PatternNodeIndex) -> &PatternNode {
        &self.graph[idx]
    }

    pub fn node_children(&self, idx: PatternNodeIndex
    ) -> ::petgraph::graph::Neighbors<()> {
        self.graph.neighbors_directed(idx, Direction::Outgoing)
    }

    pub fn add_node(&mut self, node: PatternNode) -> PatternNodeIndex {
        self.graph.add_node(node)
    }

    pub fn add_child(&mut self, parent: PatternNodeIndex, child: PatternNode) -> PatternNodeIndex {
        let res = self.graph.add_node(child);
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

        assert!(child_count == self.graph[node].children());
    }

    pub fn dimentions(&self) -> (usize, usize) {
        (self.variables_len, self.clauses_len)
    }

}
