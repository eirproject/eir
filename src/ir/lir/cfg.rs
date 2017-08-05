use ::ir::SSAVariable;
use super::{ Label, BasicBlock, Phi, OpKind, Source, Op };

fn idx_of(lbl: Label) -> usize {
    (lbl.0 - 1) as usize
}

#[derive(Debug, Clone)]
pub struct FunctionCfg {
    args: Vec<SSAVariable>,
    entry: Label,
    current: Label,
    blocks: Vec<BasicBlock>,
}
impl FunctionCfg {
    pub fn new(args: Vec<SSAVariable>) -> Self {
        let mut builder = FunctionCfg {
            args: args,
            entry: Label(0),
            current: Label(0),
            blocks: vec![],
        };
        let entry = builder.add_block();
        builder.entry = entry;
        builder.current = entry;
        builder
    }

    pub fn entry(&self) -> Label {
        self.entry
    }

    pub fn add_block(&mut self) -> Label {
        let label = Label((self.blocks.len() + 1) as u32);
        self.blocks.push(BasicBlock {
            label: label,
            back_refs: vec![],
            phi_nodes: vec![],
            ops: vec![],
            jumps: vec![],
        });
        label
    }

    pub fn set_block(&mut self, label: Label) {
        self.current = label;
    }

    pub fn get_block(&self) -> Label {
        self.current
    }

    pub fn add_phi(&mut self,
                   pred: Label, pred_instr: SSAVariable,
                   node: Label, node_instr: SSAVariable) {
        let index = (node.0 - 1) as usize;
        let block = &mut self.blocks[index];

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

    pub fn basic_op(&mut self, op: OpKind,
                    reads: Vec<Source>, writes: Vec<SSAVariable>) {
        let index = (self.current.0 - 1) as usize;
        if let Some(last_op) = self.blocks[index].ops.last() {
            assert!(last_op.kind.num_jumps().is_none());
        }

        self.blocks[index].ops.push(Op {
            kind: op,
            reads: reads,
            writes: writes,
        });
    }

    pub fn blocks_iter<'a>(&'a self) -> ::std::slice::Iter<'a, BasicBlock> {
        self.blocks.iter()
    }
    pub fn blocks_iter_mut<'a>(&'a mut self) -> ::std::slice::IterMut<'a, BasicBlock> {
        self.blocks.iter_mut()
    }
    pub fn args_iter<'a>(&'a self) -> ::std::slice::Iter<'a, SSAVariable> {
        self.args.iter()
    }

    pub fn add_jump(&mut self, source: Label, dest: Label) {
        let source_index = idx_of(source);
        let dest_index = idx_of(dest);
        self.blocks[source_index].jumps.push(dest);
        self.blocks[dest_index].back_refs.push(source);
    }

}
