use ::std::collections::HashMap;

use ::petgraph::Graph;
use ::petgraph::graph::NodeIndex;

mod generate_dot;

use super::LeafId;
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
    pub leaf_bindings: HashMap<NodeIndex, HashMap<P::PatternNodeKey, P::CfgVariable>>,
}

impl<P> PatternCfg<P> where P: PatternProvider {

    pub(crate) fn new() -> Self {
        let mut graph = Graph::new();
        PatternCfg {
            entry: graph.add_node(CfgNodeKind::Root),
            graph: graph,
            leaf_bindings: HashMap::new(),
        }
    }

    pub(crate) fn add_fail(&mut self) -> CfgNodeIndex {
        self.graph.add_node(CfgNodeKind::Fail)
    }

    pub(crate) fn add_leaf(&mut self, parent: CfgNodeIndex, leaf_num: usize, edge: CfgEdge<P>,
                           binds: HashMap<P::PatternNodeKey, P::CfgVariable>) -> CfgNodeIndex {
        let index = self.graph.add_node(CfgNodeKind::Leaf(leaf_num));
        self.graph.add_edge(parent, index, edge);
        self.leaf_bindings.insert(index, binds);
        index
    }

    pub fn get_entry(&self) -> CfgNodeIndex {
        self.entry
    }

    //pub fn add_node(&mut self, var: P::CfgVariable) -> CfgNodeIndex {
    //    self.graph.add_node(CfgNodeKind::Match(var))
    //}

    pub(crate) fn add_edge(&mut self, parent: CfgNodeIndex, child: CfgNodeIndex,
                           edge: CfgEdge<P>) {
        self.graph.add_edge(parent, child, edge);
    }

    pub(crate) fn add_child(&mut self, parent: CfgNodeIndex, typ: CfgEdge<P>,
                            var: P::CfgVariable) -> CfgNodeIndex {
        let child = self.graph.add_node(CfgNodeKind::Match(var));
        self.graph.add_edge(parent, child, typ);
        child
    }

}

#[derive(Clone)]
pub struct CfgEdge<P> where P: PatternProvider {
    //_provider: ::std::marker::PhantomData<P>,
    pub kind: Option<P::PatternNodeKind>,
    pub variable_binds: Vec<P::CfgVariable>,
    //pub pattern_node: super::pattern::PatternNodeIndex,
}
impl<P> ::std::fmt::Debug for CfgEdge<P> where P: PatternProvider {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        if let Some(kind) = self.kind {
            write!(f, "{:?} {:?}", kind, self.variable_binds)
        } else {
            write!(f, "")
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CfgNodeKind<CVT> {
    Root,
    Match(CVT),
    Fail,
    Leaf(usize),
}
