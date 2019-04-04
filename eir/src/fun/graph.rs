use super::{ Ebb, Op, EbbCall };

use std::collections::HashMap;

use petgraph::graph::{ Graph, NodeIndex };

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum CfgNode {
    Ebb(Ebb),
    Op(Op),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum CfgEdge {
    Flow,
    Call(EbbCall),
}

#[derive(Debug, Clone)]
pub struct FunctionCfg {
    pub graph: Graph<CfgNode, CfgEdge>,
    pub ops: HashMap<Op, NodeIndex>,
    pub ebbs: HashMap<Ebb, NodeIndex>,
}
