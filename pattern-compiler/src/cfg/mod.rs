use ::std::collections::HashMap;

use ::petgraph::Graph;
use ::petgraph::graph::NodeIndex;

mod generate_dot;

use super::pattern::PatternProvider;

pub type CfgNodeIndex = NodeIndex;

//#[derive(Debug, Clone)]
//pub struct Leaf<P> where P: PatternProvider {
//    bindings: HashMap<P::CfgVariable, P::PatternNodeKey>,
//}

#[derive(Debug, Clone)]
pub struct PatternCfg<P> where P: PatternProvider {
    pub entry: CfgNodeIndex,
    pub graph: Graph<CfgNodeKind<P::CfgVariable>, CfgEdge<P>>,
    pub leaves: HashMap<usize, NodeIndex>,
    pub leaf_bindings: HashMap<NodeIndex, HashMap<P::CfgVariable, P::PatternNodeKey>>,
}

impl<P> PatternCfg<P> where P: PatternProvider {

    pub fn new() -> Self {
        let mut graph = Graph::new();
        PatternCfg {
            entry: graph.add_node(CfgNodeKind::Root),
            graph: graph,
            leaves: HashMap::new(),
            leaf_bindings: HashMap::new(),
        }
    }

    pub fn add_fail(&mut self) -> CfgNodeIndex {
        self.graph.add_node(CfgNodeKind::Fail)
    }

    pub fn add_leaf(&mut self, num: usize) -> CfgNodeIndex {
        assert!(!self.leaves.contains_key(&num));
        let index = self.graph.add_node(CfgNodeKind::Leaf(num));
        self.leaves.insert(num, index);
        index
    }

    pub fn get_entry(&self) -> CfgNodeIndex {
        self.entry
    }

    pub fn add_node(&mut self, var: P::CfgVariable) -> CfgNodeIndex {
        self.graph.add_node(CfgNodeKind::Match(var))
    }

    pub fn add_edge(&mut self, parent: CfgNodeIndex, child: CfgNodeIndex,
                    edge: CfgEdge<P>) {
        self.graph.add_edge(parent, child, edge);
    }

    pub fn add_child(&mut self, parent: CfgNodeIndex, typ: CfgEdge<P>,
                     var: P::CfgVariable) -> CfgNodeIndex {
        let child = self.graph.add_node(CfgNodeKind::Match(var));
        self.graph.add_edge(parent, child, typ);
        child
    }

}

#[derive(Clone)]
pub struct CfgEdge<P> where P: PatternProvider {
    //_provider: ::std::marker::PhantomData<P>,
    pub kind: P::PatternNodeKind,
    pub variable_binds: Vec<P::CfgVariable>,
    //pub pattern_node: super::pattern::PatternNodeIndex,
}
impl<P> ::std::fmt::Debug for CfgEdge<P> where P: PatternProvider {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(f, "{:?} {:?}", self.kind, self.variable_binds)
    }
}

#[derive(Debug, Clone)]
pub enum CfgNodeKind<CVT> {
    Root,
    Match(CVT),
    Fail,
    Leaf(usize),
}
