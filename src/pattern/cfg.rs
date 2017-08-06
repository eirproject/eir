use ::petgraph::Graph;
use ::petgraph::graph::NodeIndex;

pub type CfgNodeIndex = NodeIndex;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct PatternCfgVariable(pub usize);

#[derive(Debug)]
pub struct PatternCfg {
    current_variable: usize,
    pub graph: Graph<CfgNodeKind, CfgEdge>,
}

impl PatternCfg {

    pub fn new() -> Self {
        PatternCfg {
            current_variable: 0,
            graph: Graph::new(),
        }
    }

    pub fn new_variable(&mut self) -> PatternCfgVariable {
        self.current_variable += 1;
        PatternCfgVariable(self.current_variable)
    }

    pub fn add_fail(&mut self) -> CfgNodeIndex {
        self.graph.add_node(CfgNodeKind::Fail)
    }

    pub fn add_leaf(&mut self, num: usize) -> CfgNodeIndex {
        self.graph.add_node(CfgNodeKind::Leaf(num))
    }

    pub fn add_root(&mut self) -> CfgNodeIndex {
        self.graph.add_node(CfgNodeKind::Root)
    }

    pub fn add_node(&mut self, var: PatternCfgVariable) -> CfgNodeIndex {
        self.graph.add_node(CfgNodeKind::Match(var))
    }

    pub fn add_edge(&mut self, parent: CfgNodeIndex, child: CfgNodeIndex,
                    edge: CfgEdge) {
        self.graph.add_edge(parent, child, edge);
    }

    pub fn add_child(&mut self, parent: CfgNodeIndex, typ: CfgEdge,
                     var: PatternCfgVariable) -> CfgNodeIndex {
        let child = self.graph.add_node(CfgNodeKind::Match(var));
        self.graph.add_edge(parent, child, typ);
        child
    }

}

pub struct CfgEdge {
    pub kind: super::pattern::PatternNode,
    pub variable_binds: Vec<PatternCfgVariable>,
}
impl ::std::fmt::Debug for CfgEdge {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(f, "{:?} {:?}", self.kind, self.variable_binds)
    }
}

#[derive(Debug)]
pub enum CfgNodeKind {
    Root,
    Match(PatternCfgVariable),
    Fail,
    Leaf(usize),
}
