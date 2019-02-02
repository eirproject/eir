use ::ir::SSAVariable;
use super::{ Phi, Op };

mod builder;
pub use self::builder::FunctionCfgBuilder;

use ::util::graph::*;

pub type LabelN = NodeLabel;
pub type EdgeN = EdgeLabel;

#[derive(Debug)]
pub struct BasicBlock {
    pub phi_nodes: Vec<Phi>,
    pub ops: Vec<Op>,
}

#[derive(Debug)]
pub struct FunctionCfg {
    pub entry: LabelN,
    pub args: Vec<SSAVariable>,
    pub graph: Graph<BasicBlock, BasicBlockEdge>,
}

#[derive(Debug)]
pub struct BasicBlockEdge {
    writes: Vec<SSAVariable>,
}

impl FunctionCfg {

    pub fn new() -> Self {
        let mut graph = Graph::new();

        let entry = graph.add_node(BasicBlock {
            phi_nodes: vec![],
            ops: vec![],
        });

        FunctionCfg {
            entry: entry,
            args: vec![],
            graph: graph,
        }
    }

    pub fn entry(&self) -> LabelN {
        self.entry
    }

    pub fn remove_edge(&mut self, lbl: EdgeN) {
        let (edge_from_label, edge_to_label) = {
            let edge_container = &self.graph[lbl];
            (edge_container.from, edge_container.to)
        };

        {
            let dst_container = &self.graph[edge_to_label];
            let mut dst = dst_container.inner.borrow_mut();
            for phi in dst.phi_nodes.iter_mut() {
                let pos = phi.entries.iter()
                    .position(|(from, _src)| *from == edge_from_label)
                    .expect("Phi node was invalid!");
                phi.entries.remove(pos);
            }
        }

        self.graph.remove_edge(lbl);
    }

    pub fn branch_slots(&self, lbl: LabelN) -> Vec<LabelN> {
        self.graph[lbl].outgoing.iter().map(|v| v.1).collect()
    }

    pub fn remove_block(&mut self, lbl: LabelN) {
        // Validate that the node is not the entry point.
        assert!(lbl != self.entry());

        {
            // Validate that the node has no incoming edges.
            let block_container = &self.graph[lbl];
            assert!(block_container.incoming.len() == 0);

            // Remove from PHI nodes
            for (_edge, node) in block_container.outgoing.iter() {
                let dst_block_container = &self.graph[*node];
                let mut dst_block = dst_block_container.inner.borrow_mut();
                for phi in dst_block.phi_nodes.iter_mut() {
                    let pos = phi.entries.iter()
                        .position(|(label, _ssa)| *label == lbl)
                        .unwrap();
                    phi.entries.remove(pos);
                }
            }
        }

        // Remove actual node
        self.graph.remove_node(lbl);
    }

}
