use super::SSAVariable;
use ::ir::hir::{ Pattern, LambdaEnvIdx };
use ::ir::FunctionIdent;
use ::Atom;

pub mod from_hir;
pub mod to_dot;
pub mod pass;

pub mod cfg;
pub use self::cfg::{ FunctionCfg, FunctionCfgBuilder, LabelN, BasicBlock };

//#[derive(Debug, Clone)]
//pub struct BasicBlock {
//    label: Label,
//
//    // Meta
//    back_refs: Vec<Label>,
//
//    // Main data
//    phi_nodes: Vec<Phi>,
//    ops: Vec<Op>,
//    jumps: Vec<Label>,
//}

#[derive(Debug, Clone, Copy)]
pub struct Label(u32);

#[derive(Debug, Clone)]
pub enum Source {
    Variable(SSAVariable),
    Constant(::parser::AtomicLiteral),
}

#[derive(Debug, Clone)]
pub struct Phi {
    entries: Vec<(LabelN, SSAVariable)>,
    ssa: SSAVariable,
}

#[derive(Debug, Clone)]
pub struct Op {
    pub kind: OpKind,
    pub reads: Vec<Source>,
    pub writes: Vec<SSAVariable>,
}

#[derive(Debug, Clone)]
pub enum OpKind {
    /// Must be the first OP in a function.
    /// Write number must be equal to the function arity
    Arguments,

    /// Move r[0] into w[0]
    Move,

    /// Calls r[0]:r[1] with args r[2..]
    Call,
    /// Calls r[0] with args r[1..]
    Apply,
    CaptureNamedFunction(::ir::FunctionIdent),
    CaptureExternalNamedFunction(Atom, ::ir::FunctionIdent),

    MakeTuple,
    MakeList,
    MakeMap,
    MakeBinary,

    MakeClosureEnv {
        env_idx: LambdaEnvIdx,
    },
    BindClosure {
        ident: FunctionIdent,
    },

    Jump,

    PrimOp(Atom),

    ReturnOk,
    ReturnThrow,

    IfTruthy,

    // High level matching construct, lowered to explicit control flow
    // in a LIR compiler pass.
    // This OP indicates the start of a case structure.
    // The number of outgoing edges must be equal to the number of
    // clauses.
    // All outgoing edges must start with a CaseValues OP.
    // Once going through a CaseValues, control flow must either return
    // to the case through a GuardFail, or leave the structure through
    // a GuardOk.
    // Returns a pseudo-value which is used to fetch the matched values
    // in any subsequent blocks.
    Case {
        vars: Vec<SSAVariable>,
        clauses: Vec<Clause>,
        value_vars: Vec<SSAVariable>,
    },
    // Must have one incoming edge, which must end with a Case OP.
    // The number of writes are the same as the number of bindings
    // in the clause just matched in the case block.
    // Takes the pseudo-value from the preceeding case block.
    CaseValues,
    // After exiting control flow from a Case OP and going through a
    // CaseValues, control flow must always exit the structure through
    // a GuardOk or GuardFail. Returning while inside the structure is
    // a hard error!
    CaseGuardOk,
    CaseGuardFail { clause_num: usize },


    // Indicates the start of a receive structure, must jump to a block
    // containing a single ReceiveWait.
    // No further ReceiveStart or function termination is allowed
    // before control flow is passed through a ReceiveFinish or exited
    // the structure through the timeout edge.
    //
    //          [ReceiveStart]
    //                |
    //                v
    //    ----------[ReceiveWait]<--------
    //    v                   |          |
    // [Timeout           ]   |          |
    // [Other control flow]   |          |
    //                        v          |
    //              [ReceiveGetMessage]  |
    //         -----[Match logic      ]---
    //         v           |
    //  [ReceiveFinish]    ----->[ReceiveFinish]
    //  [Other        ]          [Other        ]
    //
    //
    // #start:
    //   ...
    //   %receive_context = ReceiveStart(%timeout, #receive_loop)
    // #receive_loop:
    //   ReceiveWait(%receive_context, #match_body, #timeout_body)
    // #match_body:
    //   %message = ReceiveGetMessage()
    //   // Jump to #receive_loop if message does not match
    //   // Jump to #message_1_match if a message matches
    //   // Jump to #message_2_match if another message matches
    // #timeout_body:
    //   ...
    // #message_1_match:
    //   ReceiveFinish(%receive_context)
    //   ...
    // #message_2_match:
    //   ReceiveFinish(%receive_context)
    //   ...
    //
    ReceiveStart,
    // Central node of match loop of a receive structure.
    // Must be the only op in its basic block.
    // Jumps to edge 0 when a message has been received.
    // Jumps to edge 1 when a timeout has occured.
    ReceiveWait,
    // This must be the first instruction on edge 0 from ReceiveWait.
    // Peeks at the message in the mailbox, not removed unless ReceiveFinish
    // is passed through on this iteration
    ReceiveGetMessage,
    // When jumped to from edge 0 from a ReceiveWait, control flow either
    // needs to (eventually) return to ReceiveWait or needs to pass
    // through ReceiveFinish on its way out. Returning while inside a receive
    // structure is a hard error!
    // This will actually consume the message from the mailbox.
    ReceiveFinish,

    // Not an actual operation per se, more like an annotation.
    // Assists with CFG validation.
    // Has a single read, indicates that the read variable should never be
    // used after this point in the CFG.
    TombstoneSSA(SSAVariable),
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
            // One for each clause + failure leaf
            OpKind::Case { ref clauses, .. } => Some(clauses.len() + 1),
            // TODO
            //OpKind::Match { ref types } => Some(types.len()),
            OpKind::ReturnOk => Some(0),
            OpKind::ReturnThrow => Some(0),
            OpKind::ReceiveStart { .. } => Some(1),
            OpKind::ReceiveGetMessage => Some(2),
            OpKind::IfTruthy => Some(2),
            OpKind::CaseGuardFail { .. } => Some(1),
            _ => None,
        }
    }

}
