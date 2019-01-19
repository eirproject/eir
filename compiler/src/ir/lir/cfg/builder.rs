use ::std::collections::HashSet;

use ::ir::SSAVariable;
use ::ir::lir::{ FunctionCfg, LabelN, OpKind, Source, Op };
use super::{ BasicBlock, Phi, BasicBlockEdge, EdgeN };

#[derive(Debug)]
pub struct FunctionCfgBuilder<'a> {
    target: &'a mut FunctionCfg,
    finished: HashSet<LabelN>,
    //current: LabelN,
}

impl<'a> FunctionCfgBuilder<'a> {

    pub fn new(cfg: &'a mut FunctionCfg) -> Self {
        FunctionCfgBuilder {
            //current: cfg.entry,
            target: cfg,
            finished: HashSet::new(),
        }
    }

    pub fn get_entry(&self) -> LabelN {
        self.target.entry
    }

    pub fn basic_op(&mut self, block: LabelN, op: OpKind,
                    reads: Vec<Source>, writes: Vec<SSAVariable>) {
        self.assert_not_finished(block);
        let node = self.target.cfg.node_weight_mut(block.0).unwrap();
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
        self.assert_not_finished(pred);
        let block = self.target.cfg.node_weight_mut(node.0).unwrap();
        assert!(block.ops.len() == 0);

        let add_new = if let Some(inner) =
            block.phi_nodes.iter_mut().find(|p| p.ssa == node_instr) {
                inner.entries.push((pred, Source::Variable(pred_instr)));
                false
            } else {
                true
            };
        if add_new {
            block.phi_nodes.push(Phi {
                entries: vec![(pred, Source::Variable(pred_instr))],
                ssa: node_instr,
            });
        }
    }

    pub fn add_jump(&mut self, source: LabelN, dest: LabelN) {

        self.assert_not_finished(source);
        //self.assert_not_finished(dest);

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

    //pub fn set_block(&mut self, block: LabelN) {
    //    self.current = block;
    //}

    //pub fn get_block(&self) -> LabelN {
    //    self.current
    //}

    pub fn op_move(&mut self, block: LabelN, source: Source, dest: SSAVariable) {
        self.basic_op(block, OpKind::Move, vec![source], vec![dest]);
    }

    pub fn op_tombstone(&mut self, block: LabelN, ssa: SSAVariable) {
        self.basic_op(block, OpKind::TombstoneSSA(ssa), vec![], vec![]);
    }

    pub fn op_jump(&mut self, block: LabelN, target: LabelN) {
        self.basic_op(block, OpKind::Jump, vec![], vec![]);
        self.add_jump(block, target);
    }

    pub fn op_unpack_value_list(&mut self, block: LabelN, val_list: SSAVariable, values: &[SSAVariable]) {
        self.basic_op(
            block,
            OpKind::UnpackValueList,
            vec![Source::Variable(val_list)],
            values.into()
        );
    }

    pub fn op_pack_value_list(&mut self, block: LabelN, values: Vec<Source>, val_list: SSAVariable) {
        self.basic_op(
            block,
            OpKind::PackValueList,
            values,
            vec![val_list]
        );
    }

    pub fn finish(&mut self, block: LabelN) {
        self.finished.insert(block);
    }

    pub fn assert_finished(&self, block: LabelN) {
        assert!(self.finished.contains(&block),
                "Block {} should be finished, was not", block);
    }

    pub fn assert_not_finished(&self, block: LabelN) {
        assert!(!self.finished.contains(&block),
                "Block {} was finished", block);
    }

}
