use super::{ SSAVariable, FunctionIdent };
use crate::op::{ Op, OpKind };
use crate::Source;
use crate::ssa::SSAVariableGenerator;

use ::std::collections::{ HashMap, HashSet };

mod builder;
pub use self::builder::FunctionCfgBuilder;

mod validate;

use util::graph::*;

pub type LabelN = NodeLabel;
pub type EdgeN = EdgeLabel;

#[derive(Debug, Clone)]
pub struct BasicBlock {
    pub phi_nodes: Vec<Phi>,
    pub ops: Vec<Op>,
}

#[derive(Debug, Clone)]
pub struct Phi {
    pub entries: Vec<(EdgeN, SSAVariable)>,
    pub ssa: SSAVariable,
}

#[derive(Debug, Clone)]
pub struct FunctionCfg {
    pub entry: LabelN,
    pub args: Vec<SSAVariable>,
    pub graph: Graph<BasicBlock, BasicBlockEdge>,
    pub ssa_gen: SSAVariableGenerator,
}



#[derive(Debug, Clone)]
pub struct BasicBlockEdge {
    writes: Vec<SSAVariable>,
}

impl FunctionCfg {

    pub fn new(ssa_gen: SSAVariableGenerator) -> Self {
        let mut graph = Graph::new();

        let entry = graph.add_node(BasicBlock {
            phi_nodes: vec![],
            ops: vec![],
        });

        FunctionCfg {
            entry: entry,
            args: vec![],
            graph: graph,
            ssa_gen: ssa_gen,
        }
    }

    pub fn entry(&self) -> LabelN {
        self.entry
    }

    pub fn remove_edge(&mut self, lbl: EdgeN) {
        let (_edge_from_label, edge_to_label) = {
            let edge_container = &self.graph[lbl];
            (edge_container.from, edge_container.to)
        };

        {
            let dst_container = &self.graph[edge_to_label];
            let mut dst = dst_container.inner.borrow_mut();
            for phi in dst.phi_nodes.iter_mut() {
                let pos = phi.entries.iter()
                    .position(|(from, _src)| *from == lbl)
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
            for (edge, node) in block_container.outgoing.iter() {
                let dst_block_container = &self.graph[*node];
                let mut dst_block = dst_block_container.inner.borrow_mut();
                for phi in dst_block.phi_nodes.iter_mut() {
                    let pos = phi.entries.iter()
                        .position(|(label, _ssa)| label == edge)
                        .unwrap();
                    phi.entries.remove(pos);
                }
            }
        }

        // Remove actual node
        unsafe { self.graph.remove_node(lbl) };
    }

    pub fn compress_numbering(&mut self) {

        // Generate set of all written SSA variables in CFG
        let mut ssa_vars = HashSet::new();
        for node in self.graph.nodes() {
            let node_inner = node.inner.borrow();
            for phi in node_inner.phi_nodes.iter() {
                ssa_vars.insert(phi.ssa);
            }
            for op in node_inner.ops.iter() {
                for ssa in op.writes.iter() {
                    ssa_vars.insert(*ssa);
                }
            }
        }

        // Reset SSA generator
        self.ssa_gen = SSAVariableGenerator::initial();

        // Sort the set of old SSA variables
        let mut ssa_vars_vec: Vec<_> = ssa_vars.iter().cloned().collect();
        ssa_vars_vec.sort_unstable();

        // Generate a new mapping
        let mut mapping = HashMap::new();
        for old_ssa in ssa_vars_vec.iter() {
            mapping.insert(*old_ssa, self.ssa_gen.next());
        }

        // Update CFG with new mapping
        for node in self.graph.nodes() {
            let mut node_inner = node.inner.borrow_mut();
            for phi in node_inner.phi_nodes.iter_mut() {
                phi.ssa = mapping[&phi.ssa];
                for (_entry_edge, ref mut ssa) in phi.entries.iter_mut() {
                    *ssa = mapping[ssa];
                }
            }
            for op in node_inner.ops.iter_mut() {
                for write in op.writes.iter_mut() {
                    *write = mapping[write];
                }
                for read in op.reads.iter_mut() {
                    if let Source::Variable(ref mut ssa) = read {
                        *ssa = mapping[ssa];
                    }
                }
                if let OpKind::TombstoneSSA(ref mut ssa) = &mut op.kind {
                    *ssa = mapping[ssa];
                }
            }
        }

    }

    //pub fn reparent_edge(&mut self, edge: EdgeN, new_parent: LabelN) {
    //    self.graph.
    //}

    // Gets all static calls from the function.
    pub fn get_all_calls(&self) -> HashSet<FunctionIdent> {
        let mut calls = HashSet::new();
        for node in self.graph.nodes() {
            let node_inner = node.inner.borrow();
            for op in node_inner.ops.iter() {
                match op.kind {
                    OpKind::Call { .. } => {
                        let arity = op.reads.len() - 2;
                        let module = op.reads[0].constant()
                            .map_or(None, |c| c.atom());
                        let name = op.reads[1].constant()
                            .map_or(None, |c| c.atom());
                        if module.is_some() && name.is_some() {
                            let module = module.unwrap();
                            let name = name.unwrap();
                            calls.insert(FunctionIdent {
                                module: module,
                                name: name,
                                arity: arity,
                                lambda: None,
                            });
                        }
                    }
                    OpKind::CaptureNamedFunction(ref ident) => {
                        calls.insert(ident.clone());
                    }
                    OpKind::BindClosure { ref ident } => {
                        calls.insert(ident.clone());
                    }
                    _ => (),
                }
            }
        }
        calls
    }

}



