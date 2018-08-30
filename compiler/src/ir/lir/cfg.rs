use ::ir::SSAVariable;
use super::{ Phi, OpKind, Source, Op };
use ::petgraph::Graph;

#[derive(Debug, Copy, Clone)]
pub struct LabelN(pub ::petgraph::graph::NodeIndex);
impl ::std::fmt::Display for LabelN {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(f, "L{}", self.0.index())
    }
}

#[derive(Debug, Copy, Clone)]
pub struct EdgeN(pub ::petgraph::graph::EdgeIndex);

#[derive(Debug)]
pub struct BasicBlock {
    pub label: Option<LabelN>,
    pub phi_nodes: Vec<Phi>,
    pub ops: Vec<Op>,
    pub outgoing_edges: Vec<EdgeN>,
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
            label: None,
            phi_nodes: vec![],
            ops: vec![],
            outgoing_edges: vec![],
        });
        cfg[entry].label = Some(LabelN(entry));

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

    pub fn jumps_iter<'a>(&'a self, lbl: LabelN) -> impl Iterator<Item = EdgeN> + 'a {
        let node = self.cfg.node_weight(lbl.0).unwrap();
        node.outgoing_edges.iter().map(|e| *e)
    }

    pub fn branch_slots(&self, lbl: LabelN) -> Vec<LabelN> {
        self.jumps_iter(lbl)
            .map(|edge| self.edge_target(edge))
            .collect()
    }

    pub fn edge_target(&self, lbl: EdgeN) -> LabelN {
        LabelN(self.cfg.edge_endpoints(lbl.0).unwrap().1)
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
        let edge_id = self.target.cfg.add_edge(source.0, dest.0, BasicBlockEdge {
            writes: vec![],
        });
        let node = self.target.cfg.node_weight_mut(source.0).unwrap();
        node.outgoing_edges.push(EdgeN(edge_id));
    }

    pub fn add_block(&mut self) -> LabelN {
        let idx = self.target.cfg.add_node(BasicBlock {
            label: None,
            ops: vec![],
            phi_nodes: vec![],
            outgoing_edges: vec![],
        });
        self.target.cfg[idx].label = Some(LabelN(idx));

        LabelN(idx)
    }

    pub fn set_block(&mut self, block: LabelN) {
        self.current = block;
    }

    pub fn get_block(&self) -> LabelN {
        self.current
    }

    pub fn op_tombstone(&mut self, ssa: SSAVariable) {
        self.basic_op(OpKind::TombstoneSSA(ssa), vec![], vec![]);
    }

    pub fn op_jump(&mut self, target: LabelN) {
        self.basic_op(OpKind::Jump, vec![], vec![]);
        let source = self.get_block();
        self.add_jump(source, target);
    }

}

