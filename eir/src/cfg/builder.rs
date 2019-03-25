use ::std::collections::HashSet;

use crate::SSAVariable;
use crate::Atom;
use crate::op::{ Op, OpKind };
use super::{ FunctionCfg, LabelN, Source };
use super::{ BasicBlock, Phi, BasicBlockEdge, EdgeN };
use crate::AtomicTerm;

#[derive(Debug)]
pub struct FunctionCfgBuilder<'a> {
    pub target: &'a mut FunctionCfg,
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

    pub fn new_ssa(&mut self) -> SSAVariable {
        self.target.ssa_gen.next()
    }

    pub fn get_entry(&self) -> LabelN {
        self.target.entry
    }

    pub fn basic_op(&mut self, block: LabelN, op: OpKind,
                    reads: Vec<Source>, writes: Vec<SSAVariable>) {
        self.assert_not_finished(block);
        //let node = self.target.cfg.node_weight_mut(block.0).unwrap();
        let node_container = self.target.graph.node(block);
        let mut node = node_container.inner.borrow_mut();
        node.ops.push(Op {
            kind: op,
            reads: reads,
            writes: writes,
        });
    }

    pub fn ensure_phi(&mut self, node: LabelN, node_instr: SSAVariable) {
        let node_container = self.target.graph.node(node);
        let mut block = node_container.inner.borrow_mut();
        assert!(block.ops.len() == 0);

        let has_phi = block.phi_nodes.iter().any(|p| p.ssa == node_instr);

        if !has_phi {
            block.phi_nodes.push(Phi {
                entries: vec![],
                ssa: node_instr,
            })
        }
    }

    pub fn add_phi(&mut self,
                   edge: EdgeN, from_ssa: SSAVariable,
                   to_ssa: SSAVariable) {

        let from_node = self.target.graph.edge_from(edge);
        let to_node = self.target.graph.edge_to(edge);

        self.assert_not_finished(from_node);
        let block_container = &self.target.graph[to_node];
        let mut block = block_container.inner.borrow_mut();
        assert!(block.ops.len() == 0);

        let add_new = if let Some(inner) =
            block.phi_nodes.iter_mut().find(|p| p.ssa == to_ssa) {
                inner.entries.push((edge, from_ssa));
                false
            } else {
                true
            };
        if add_new {
            block.phi_nodes.push(Phi {
                entries: vec![(edge, from_ssa)],
                ssa: to_ssa,
            });
        }
    }

    pub fn add_jump(&mut self, source: LabelN, dest: LabelN) -> EdgeN {
        self.assert_not_finished(source);
        self.target.graph.add_edge(source, dest, BasicBlockEdge {
            writes: vec![],
        })
    }

    pub fn add_block(&mut self) -> LabelN {
        self.target.graph.add_node(BasicBlock {
            ops: vec![],
            phi_nodes: vec![],
        })
    }

    pub fn op_move<I>(&mut self, block: LabelN, source: I, dest: SSAVariable) where I: Into<Source> {
        self.basic_op(block, OpKind::Move, vec![source.into()], vec![dest]);
    }

    pub fn op_tombstone(&mut self, block: LabelN, ssa: SSAVariable) {
        self.basic_op(block, OpKind::TombstoneSSA(ssa), vec![], vec![]);
    }

    pub fn op_jump(&mut self, block: LabelN, target: LabelN) -> EdgeN {
        self.basic_op(block, OpKind::Jump, vec![], vec![]);
        self.add_jump(block, target)
    }

    pub fn op_return_ok(&mut self, block: LabelN, value: Source) {
        self.basic_op(block, OpKind::ReturnOk, vec![value], vec![]);
    }
    pub fn op_return_throw(&mut self, block: LabelN, value: Source) {
        self.basic_op(block, OpKind::ReturnThrow, vec![value], vec![]);
    }

    pub fn op_arguments(&mut self, block: LabelN, args: Vec<SSAVariable>) {
        self.basic_op(block, OpKind::Arguments, vec![], args);
    }

    pub fn op_case_guard_ok(&mut self, block: LabelN, case_structure: SSAVariable) {
        self.basic_op(block, OpKind::CaseGuardOk,
                      vec![Source::Variable(case_structure)], vec![]);
    }
    pub fn op_case_values(&mut self, block: LabelN, case_structure: SSAVariable, values: Vec<SSAVariable>) {
        self.basic_op(
            block,
            OpKind::CaseValues,
            vec![case_structure.into()],
            values
        );
    }

    pub fn op_unreachable(&mut self, block: LabelN) {
        self.basic_op(block, OpKind::Unreachable, vec![], vec![]);
        self.finish(block);
    }

    pub fn op_primop(&mut self, block: LabelN, primop: Atom, args: Vec<Source>, results: Vec<SSAVariable>) {
        self.basic_op(
            block,
            OpKind::PrimOp(primop),
            args,
            results
        );
    }

    pub fn op_make_tuple(&mut self, block: LabelN, vars: Vec<Source>, res: SSAVariable) {
        self.basic_op(
            block,
            OpKind::MakeTuple,
            vars,
            vec![res]
        );
    }

    pub fn op_unpack_value_list(&mut self, block: LabelN, val_list: SSAVariable, values: Vec<SSAVariable>) {
        self.basic_op(
            block,
            OpKind::UnpackValueList,
            vec![Source::Variable(val_list)],
            values
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

    pub fn op_unpack_tuple(&mut self, block: LabelN, value: Source, values: Vec<SSAVariable>, ok_jump: LabelN, fail_jump: LabelN) {
        self.basic_op(
            block,
            OpKind::UnpackTuple,
            vec![value],
            values,
        );
        self.add_jump(block, ok_jump);
        self.add_jump(block, fail_jump);
        self.finish(block);
    }

    pub fn op_equal_atomic(&mut self, block: LabelN, value: Source, atomic: AtomicTerm, ok_jump: LabelN, fail_jump: LabelN) {
        self.basic_op(
            block,
            OpKind::EqualAtomic(atomic),
            vec![value],
            vec![]
        );
        self.add_jump(block, ok_jump);
        self.add_jump(block, fail_jump);
        self.finish(block);
    }

    pub fn op_unpack_list_cell(&mut self, block: LabelN, value: Source, head: SSAVariable, tail: SSAVariable, ok_jump: LabelN, fail_jump: LabelN) {
        self.basic_op(
            block,
            OpKind::UnpackListCell,
            vec![value],
            vec![head, tail]
        );
        self.add_jump(block, ok_jump);
        self.add_jump(block, fail_jump);
        self.finish(block);
    }

    pub fn op_is_map(&mut self, block: LabelN, value: Source, ok_jump: LabelN, fail_jump: LabelN) {
        self.basic_op(
            block,
            OpKind::IsMap,
            vec![value],
            vec![],
        );
        self.add_jump(block, ok_jump);
        self.add_jump(block, fail_jump);
        self.finish(block);
    }

    pub fn op_map_get(&mut self, block: LabelN, map: Source, key: Source,
                      result_ssa: SSAVariable, ok_jump: LabelN, fail_jump: LabelN) {
        self.basic_op(
            block,
            OpKind::MapGet,
            vec![map, key],
            vec![result_ssa],
        );
        self.add_jump(block, ok_jump);
        self.add_jump(block, fail_jump);
        self.finish(block);
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
