use super::SSAVariable;
use ::ir::hir::{ Pattern, LambdaEnvIdx };
use ::ir::FunctionIdent;
use ::Atom;

pub mod from_hir;
pub mod to_dot;
pub mod pass;

fn idx_of(lbl: Label) -> usize {
    (lbl.0 - 1) as usize
}

#[derive(Debug)]
pub struct LirBuilder {
    entry: Label,
    current: Label,
    blocks: Vec<BasicBlock>,
}
impl LirBuilder {
    pub fn new() -> Self {
        let mut builder = LirBuilder {
            entry: Label(0),
            current: Label(0),
            blocks: vec![],
        };
        let entry = builder.add_block();
        builder.entry = entry;
        builder.current = entry;
        builder
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

    pub fn add_phi(&mut self, phi: Phi) {
        let index = (self.current.0 - 1) as usize;
        assert!(self.blocks[index].ops.len() == 0);
        self.blocks[index].phi_nodes.push(phi);
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

    pub fn add_jump(&mut self, source: Label, dest: Label) {
        let source_index = idx_of(source);
        let dest_index = idx_of(dest);
        self.blocks[source_index].jumps.push(dest);
        self.blocks[dest_index].back_refs.push(source);
    }

    pub fn build(self) -> Function {
        for block in &self.blocks {
            let num_jumps = block.ops.last().unwrap().kind.num_jumps().unwrap();
            if num_jumps != block.jumps.len() {
                panic!("wrong number of jumps: {:#?}", block);
            }
        }
        Function {
            entry: self.entry,
            basic_blocks: self.blocks,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub entry: Label,
    pub basic_blocks: Vec<BasicBlock>,
}

#[derive(Debug, Clone)]
pub struct BasicBlock {
    label: Label,

    // Meta
    back_refs: Vec<Label>,

    // Main data
    phi_nodes: Vec<Phi>,
    ops: Vec<Op>,
    jumps: Vec<Label>,
}

#[derive(Debug, Clone, Copy)]
pub struct Label(u32);

#[derive(Debug, Clone)]
pub enum Source {
    Variable(SSAVariable),
    Constant(::parser::AtomicLiteral),
}

#[derive(Debug, Clone)]
pub struct Phi {
    entries: Vec<(Label, SSAVariable)>,
    ssa: SSAVariable,
}

#[derive(Debug, Clone)]
pub struct Op {
    kind: OpKind,
    reads: Vec<Source>,
    writes: Vec<SSAVariable>,
}

#[derive(Debug, Clone)]
pub enum OpKind {
    /// Move r[0] into w[0]
    Move,

    /// Calls r[0]:r[1] with args r[2..]
    Call,
    /// Calls r[0] with args r[1..]
    Apply,
    CaptureNamedFunction(::ir::FunctionIdent),

    MakeTuple,
    MakeList,

    MakeClosureEnv {
        env_idx: LambdaEnvIdx,
    },
    BindClosure {
        ident: FunctionIdent,
    },

    Case {
        vars: Vec<SSAVariable>,
        clauses: Vec<Clause>
    },

    Jump,

    PrimOp(Atom),

    ReturnOk,
    ReturnThrow,
}

#[derive(Debug, Clone)]
pub struct Clause {
    patterns: Vec<Pattern>,
}

impl OpKind {

    fn num_jumps(&self) -> Option<usize> {
        match *self {
            OpKind::Call => Some(2),
            OpKind::Apply => Some(2),
            OpKind::Jump => Some(1),
            OpKind::Case { ref clauses, .. } => Some(clauses.len()),
            OpKind::ReturnOk => Some(0),
            OpKind::ReturnThrow => Some(0),
            _ => None,
        }
    }

}
