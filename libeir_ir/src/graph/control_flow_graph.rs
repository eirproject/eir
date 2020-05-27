use std::collections::BTreeSet;

use petgraph::visit::{GraphBase, IntoNeighbors, IntoNeighborsDirected};
use petgraph::Direction;

use cranelift_entity::{EntityList, ListPool, SecondaryMap};

use itertools::Either;

use crate::Function;
use crate::Value;

impl Function {
    pub fn control_flow_graph(&self) -> ControlFlowGraph {
        ControlFlowGraph::new()
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct CfgBranch(pub usize);

/// This is a newtype that contains implementations of petgraphs graph traits.
///
/// The semantics of the below graph are as follows:
/// - Nodes are values
/// - Only explicit control flow are graph edges
pub struct ControlFlowGraph {
    forward: SecondaryMap<Value, EntityList<Value>>,
    back: SecondaryMap<Value, EntityList<Value>>,
    pool: ListPool<Value>,
}

impl ControlFlowGraph {
    pub fn new() -> Self {
        ControlFlowGraph {
            forward: SecondaryMap::new(),
            back: SecondaryMap::new(),
            pool: ListPool::new(),
        }
    }

    pub fn calculate(&mut self, fun: &Function, entry: Value) {
        let mut to_walk = Vec::new();
        to_walk.push(entry);
        let mut walked = BTreeSet::new();

        while let Some(val) = to_walk.pop() {
            if walked.contains(&val) {
                continue;
            }
            walked.insert(val);

            if let Some(block) = fun.value_block(val) {
                let forward = &mut self.forward[val];
                for out in fun.op_branch_iter(block) {
                    forward.push(out, &mut self.pool);
                    if !walked.contains(&out) {
                        to_walk.push(out);
                    }
                }
            }
        }
    }
}

impl GraphBase for ControlFlowGraph {
    type NodeId = Value;
    type EdgeId = CfgBranch;
}

pub struct ControlFlowSuccessors<'a> {
    graph: &'a ControlFlowGraph,
    value: Value,
    curr: usize,
}
impl<'a> Iterator for ControlFlowSuccessors<'a> {
    type Item = Value;
    fn next(&mut self) -> Option<Value> {
        if let Some(val) = self.graph.forward[self.value].get(self.curr, &self.graph.pool) {
            self.curr += 1;
            Some(val)
        } else {
            None
        }
    }
}

pub struct ControlFlowPredecessors<'a> {
    graph: &'a ControlFlowGraph,
    value: Value,
    curr: usize,
}
impl<'a> ControlFlowPredecessors<'a> {
    pub fn new(graph: &'a ControlFlowGraph, value: Value) -> Self {
        ControlFlowPredecessors {
            graph,
            value,
            curr: 0,
        }
    }
}
impl<'a> Iterator for ControlFlowPredecessors<'a> {
    type Item = Value;
    fn next(&mut self) -> Option<Value> {
        if let Some(val) = self.graph.back[self.value].get(self.curr, &self.graph.pool) {
            self.curr += 1;
            Some(val)
        } else {
            None
        }
    }
}

impl<'a> IntoNeighbors for &'a ControlFlowGraph {
    type Neighbors = ControlFlowSuccessors<'a>;
    fn neighbors(self, value: Value) -> Self::Neighbors {
        ControlFlowSuccessors {
            graph: self,
            value,
            curr: 0,
        }
    }
}
impl<'a> IntoNeighborsDirected for &'a ControlFlowGraph {
    type NeighborsDirected = Either<ControlFlowSuccessors<'a>, ControlFlowPredecessors<'a>>;
    fn neighbors_directed(self, value: Value, dir: Direction) -> Self::NeighborsDirected {
        match dir {
            Direction::Outgoing => Either::Left(self.neighbors(value)),
            Direction::Incoming => Either::Right(ControlFlowPredecessors::new(self, value)),
        }
    }
}
