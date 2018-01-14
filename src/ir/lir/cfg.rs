use ::ir::SSAVariable;
use super::{ Label, Phi, OpKind, Source, Op };
use ::petgraph::Graph;

fn idx_of(lbl: Label) -> usize {
    (lbl.0 - 1) as usize
}

#[derive(Debug, Copy, Clone)]
pub struct LabelN(pub ::petgraph::graph::NodeIndex);
impl ::std::fmt::Display for LabelN {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(f, "L{}", self.0.index())
    }
}

#[derive(Debug)]
pub struct BasicBlock {
    pub phi_nodes: Vec<Phi>,
    pub ops: Vec<Op>,
}

#[derive(Debug)]
pub struct FunctionCfg {
    pub entry: LabelN,
    pub cfg: Graph<BasicBlock, BasicBlockEdge>,
}

#[derive(Debug)]
pub struct BasicBlockEdge {
    writes: Vec<SSAVariable>,
}

#[derive(Debug)]
pub struct FunctionCfgBuilder<'a> {
    target: &'a mut FunctionCfg,
    current: LabelN,
}

impl FunctionCfg {

    pub fn new() -> Self {
        let mut cfg = Graph::new();

        let entry = cfg.add_node(BasicBlock {
            phi_nodes: vec![],
            ops: vec![],
        });

        FunctionCfg {
            entry: LabelN(entry),
            cfg: cfg,
        }
    }

    pub fn blocks_iter<'a>(&'a self) -> Box<Iterator<Item = &BasicBlock> + 'a> {
        Box::new(self.cfg.raw_nodes().iter()
                 .map(|n| &n.weight))
    }

    pub fn blocks_iter_mut<'a>(&'a mut self) -> Box<Iterator<Item = &mut BasicBlock> + 'a> {
        Box::new(self.cfg.node_weights_mut())
    }

    pub fn labels_iter(&self) -> Box<Iterator<Item = LabelN>> {
        Box::new(self.cfg.node_indices().map(|i| LabelN(i)))
    }

    pub fn entry(&self) -> LabelN {
        self.entry
    }

    pub fn block<'a>(&'a self, lbl: LabelN) -> &BasicBlock {
        self.cfg.node_weight(lbl.0).unwrap()
    }

    pub fn jumps_iter<'a>(&'a self, lbl: LabelN) -> ::petgraph::graph::Edges<BasicBlockEdge, ::petgraph::Directed> {
        self.cfg.edges_directed(lbl.0, ::petgraph::Direction::Outgoing)
    }

    pub fn branch_slots(&self, lbl: LabelN) -> Vec<LabelN> {
        self.cfg
            .neighbors_directed(lbl.0, ::petgraph::Direction::Outgoing)
            .map(|edge_id| LabelN(edge_id))
            .collect()
    }

}

impl<'a> FunctionCfgBuilder<'a> {

    pub fn new(cfg: &'a mut FunctionCfg) -> Self {
        FunctionCfgBuilder {
            current: cfg.entry,
            target: cfg,
        }
    }

    pub fn basic_op(&mut self, op: OpKind,
                    reads: Vec<Source>, writes: Vec<SSAVariable>) {
        let node = self.target.cfg.node_weight_mut(self.current.0).unwrap();
        node.ops.push(Op {
            kind: op,
            reads: reads,
            writes: writes,
        });
    }

    pub fn add_phi(&mut self,
                   pred: LabelN, pred_instr: SSAVariable,
                   node: LabelN, node_instr: SSAVariable) {

        assert!(self.target.cfg.contains_edge(pred.0, node.0));
        let block = self.target.cfg.node_weight_mut(node.0).unwrap();
        assert!(block.ops.len() == 0);

        let add_new = if let Some(inner) =
            block.phi_nodes.iter_mut().find(|p| p.ssa == node_instr) {
                inner.entries.push((pred, pred_instr));
                false
            } else {
                true
            };
        if add_new {
            block.phi_nodes.push(Phi {
                entries: vec![(pred, pred_instr)],
                ssa: node_instr,
            });
        }
    }

    pub fn add_jump(&mut self, source: LabelN, dest: LabelN) {
        self.target.cfg.add_edge(source.0, dest.0, BasicBlockEdge {
            writes: vec![],
        });
    }

    pub fn add_block(&mut self) -> LabelN {
        let idx = self.target.cfg.add_node(BasicBlock {
            ops: vec![],
            phi_nodes: vec![],
        });
        LabelN(idx)
    }

    pub fn set_block(&mut self, block: LabelN) {
        self.current = block;
    }

    pub fn get_block(&self) -> LabelN {
        self.current
    }

}

//#[derive(Debug, Clone)]
//pub struct FunctionCfg {
//    args: Vec<SSAVariable>,
//    entry: Label,
//    current: Label,
//    blocks: Vec<BasicBlock>,
//}
//impl FunctionCfg {
//    pub fn new(args: Vec<SSAVariable>) -> Self {
//        let mut builder = FunctionCfg {
//            args: args,
//            entry: Label(0),
//            current: Label(0),
//            blocks: vec![],
//        };
//        let entry = builder.add_block();
//        builder.entry = entry;
//        builder.current = entry;
//        builder
//    }
//
//    pub fn entry(&self) -> Label {
//        self.entry
//    }
//
//    pub fn add_block(&mut self) -> Label {
//        let label = Label((self.blocks.len() + 1) as u32);
//        self.blocks.push(BasicBlock {
//            label: label,
//            back_refs: vec![],
//            phi_nodes: vec![],
//            ops: vec![],
//            jumps: vec![],
//        });
//        label
//    }
//
//    pub fn set_block(&mut self, label: Label) {
//        self.current = label;
//    }
//
//    pub fn get_block(&self) -> Label {
//        self.current
//    }
//
//    pub fn add_phi(&mut self,
//                   pred: Label, pred_instr: SSAVariable,
//                   node: Label, node_instr: SSAVariable) {
//        let index = (node.0 - 1) as usize;
//        let block = &mut self.blocks[index];
//
//        assert!(block.ops.len() == 0);
//        let add_new = if let Some(inner) =
//            block.phi_nodes.iter_mut().find(|p| p.ssa == node_instr) {
//                inner.entries.push((pred, pred_instr));
//                false
//            } else {
//                true
//            };
//        if add_new {
//            block.phi_nodes.push(Phi {
//                entries: vec![(pred, pred_instr)],
//                ssa: node_instr,
//            });
//        }
//    }
//
//    pub fn basic_op(&mut self, op: OpKind,
//                    reads: Vec<Source>, writes: Vec<SSAVariable>) {
//        let index = (self.current.0 - 1) as usize;
//        if let Some(last_op) = self.blocks[index].ops.last() {
//            assert!(last_op.kind.num_jumps().is_none());
//        }
//
//        self.blocks[index].ops.push(Op {
//            kind: op,
//            reads: reads,
//            writes: writes,
//        });
//    }
//
//    pub fn blocks_iter<'a>(&'a self) -> ::std::slice::Iter<'a, BasicBlock> {
//        self.blocks.iter()
//    }
//    pub fn blocks_iter_mut<'a>(&'a mut self) -> ::std::slice::IterMut<'a, BasicBlock> {
//        self.blocks.iter_mut()
//    }
//    pub fn args_iter<'a>(&'a self) -> ::std::slice::Iter<'a, SSAVariable> {
//        self.args.iter()
//    }
//
//    pub fn add_jump(&mut self, source: Label, dest: Label) {
//        let source_index = idx_of(source);
//        let dest_index = idx_of(dest);
//        self.blocks[source_index].jumps.push(dest);
//        self.blocks[dest_index].back_refs.push(source);
//    }
//
//}
