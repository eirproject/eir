use crate::{ FunctionIdent, ClosureEnv, Atom, Clause };
use crate::{ Value, AtomicTerm };
use crate::{ Dialect };

#[derive(Debug, Clone)]
pub enum ComparisonOperation {
    /// ==
    Equal,
    /// /=
    NotEqual,
    /// =<
    LessEqual,
    /// <
    Less,
    /// >=
    GreaterEqual,
    /// >
    Greater,
    /// =:=
    ExactEqual,
    /// =/=
    ExactNotEqual,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum CallType {
    /// Normal call.
    Normal,
    /// Required tail call.
    Tail,
    /// Call to a continuation. Only allowed in CPS dialect.
    /// These are also terminators.
    Cont,
}

#[derive(Debug, Clone)]
pub enum OpKind {
    /// Must be the first OP in a function.
    /// Write number must be equal to the function arity
    Arguments,

    /// If the function is a lambda, this must be the second
    /// instruction in the function.
    UnpackEnv,

    /// Move r[0] into w[0]
    Move,

    // Raw exception handling.
    // This gets the stack trace from a raw trace
    ExcTrace,

    /// Calls r[0]:r[1] with args r[2..]
    /// Jumps to branch slot 0 on return, 1 on exception
    Call {
        call_type: CallType,
        /// Since the arity of the call may be affected
        /// by transformations, this indicates the original
        /// arity of the function.
        arity: usize,
    },
    /// Calls r[0] with args r[1..].
    /// Jumps to branch slot 0 on return, 1 on exception
    Apply {
        call_type: CallType,
    },
    /// Captures the function, and assigns it to w[0]
    CaptureNamedFunction(FunctionIdent),

    MakeTuple,
    MakeList,
    MakeBinary,

    MapEmpty,
    MapPut { update: bool },

    /// Value lists are not an actual type in the program.
    /// A value list of length 1 is semantically identical
    /// to the value itself.
    /// A value list may only exist as a SSA value directly,
    /// no other types may contain a value list. Value lists
    /// may not contain value lists.
    ///
    /// A value list of length 0 may be used as a empty set
    /// value. It may not be used in any reads, except a
    /// UnpackValueList with no writes.
    PackValueList,
    UnpackValueList,

    MakeClosureEnv {
        env_idx: ClosureEnv,
    },
    BindClosure {
        ident: FunctionIdent,
    },

    /// Jumps to branch slot 0
    Jump,

    PrimOp(Atom),

    /// One of the two ways to return from a function.
    /// This is a terminator operation, and its basic block
    /// should have no successors.
    /// Will return normally.
    ReturnOk,
    /// One of the two ways to return from a function.
    /// This is a terminator operation, and its basic block
    /// should have no successors.
    /// Will return an exception.
    ReturnThrow,

    IfTruthy,
    ComparisonOperation(ComparisonOperation),

    CaseStart {
        //vars: Value,
        clauses: Vec<Clause>,
        //value_vars: Vec<Value>,
    },
    /// High level matching construct, lowered to explicit control flow
    /// in a LIR compiler pass.
    /// This OP indicates the start of a case structure.
    /// The number of outgoing edges must be equal to the number of
    /// clauses.
    /// All outgoing edges must start with a CaseValues OP.
    /// Once going through a CaseValues, control flow must either return
    /// to the case through a GuardFail, or leave the structure through
    /// a GuardOk.
    /// Returns a pseudo-value which is used to fetch the matched values
    /// in any subsequent blocks.
    Case(usize),
    /// Must have one incoming edge, which must end with a Case OP.
    /// The number of writes are the same as the number of bindings
    /// in the clause just matched in the case block.
    /// Takes the pseudo-value from the preceeding case block.
    CaseValues,
    /// After exiting control flow from a Case OP and going through a
    /// CaseValues, control flow must always exit the structure through
    /// a GuardOk or GuardFail. Returning while inside the structure is
    /// a hard error!
    CaseGuardOk,
    CaseGuardFail { clause_num: usize },

    /// Lower level branches used in compiled patterns.
    UnpackTuple,
    UnpackListCell,
    IsMap,
    UnpackMapItem,
    EqualAtomic(AtomicTerm),
    MapGet,

    /// Not an actual operation per se, more like an annotation.
    /// Assists with CFG validation.
    /// Has a single read, indicates that the read variable should never be
    /// used after this point in the CFG.
    TombstoneSSA(Value),

    Unreachable,
}

impl OpKind {

    //pub fn num_jumps(&self) -> Option<usize> {
    //    match *self {
    //        OpKind::Call { tail_call: false, .. } => Some(2),
    //        OpKind::Call { tail_call: true, .. } => Some(0),
    //        OpKind::Apply { tail_call: false } => Some(2),
    //        OpKind::Apply { tail_call: true } => Some(0),
    //        OpKind::Jump => Some(1),
    //        OpKind::CaseStart { .. } => Some(1),
    //        OpKind::Case(num_clauses) => Some(num_clauses + 1),
    //        // One for each clause + failure leaf
    //        //OpKind::Case { ref clauses, .. } => Some(clauses.len() + 1),
    //        // TODO
    //        //OpKind::Match { ref types } => Some(types.len()),
    //        OpKind::ReturnOk => Some(0),
    //        OpKind::ReturnThrow => Some(0),
    //        //OpKind::ReceiveStart { .. } => Some(1),
    //        OpKind::ReceiveWait => Some(2),
    //        OpKind::IfTruthy => Some(2),
    //        //OpKind::CaseGuardFail { .. } => Some(1),
    //        OpKind::Unreachable => Some(0),

    //        // Value conditionals
    //        OpKind::UnpackTuple => Some(2),
    //        OpKind::UnpackListCell => Some(2),
    //        OpKind::IsMap => Some(2),
    //        OpKind::UnpackMapItem => Some(2),
    //        OpKind::EqualAtomic(_) => Some(2),
    //        OpKind::MapGet => Some(2),

    //        _ => None,
    //    }
    //}

    //pub fn is_logic(&self) -> bool {
    //    match *self {
    //        OpKind::TombstoneSSA(_) => false,
    //        OpKind::Jump => false,
    //        OpKind::ReturnOk => false,
    //        OpKind::ReturnThrow => false,
    //        _ => true,
    //    }
    //}

    pub fn is_block_terminator(&self) -> bool {
        match self {
            OpKind::Call { call_type: CallType::Tail, .. } => true,
            OpKind::Call { call_type: CallType::Cont, .. } => true,
            OpKind::Apply { call_type: CallType::Tail } => true,
            OpKind::Apply { call_type: CallType::Cont } => true,
            OpKind::Jump => true,
            OpKind::CaseStart { .. } => true,
            OpKind::Case(_) => true,
            OpKind::CaseGuardFail { .. } => true,
            OpKind::ReturnOk => true,
            OpKind::ReturnThrow => true,
            OpKind::Unreachable => true,
            _ => false,
        }
    }

    pub fn allowed_in_dialect(&self, dialect: Dialect) -> bool {
        match (self, dialect) {

            // Normal calls are not allowed in CPS dialect, but tail calls
            // and cont calls are.
            (OpKind::Call { call_type: CallType::Normal, .. }, Dialect::CPS) => false,
            (OpKind::Call { .. }, Dialect::CPS) => true,
            (OpKind::Apply { call_type: CallType::Normal }, Dialect::CPS) => false,
            (OpKind::Apply { .. }, Dialect::CPS) => true,

            // Cont calls are only allowed in CPS dialect
            (OpKind::Call { call_type: CallType::Cont, .. }, _) => false,
            (OpKind::Apply { call_type: CallType::Cont }, _) => false,

            // Pattern matching construct is only allowed in High dialect
            (OpKind::CaseStart { .. }, Dialect::High) => true,
            (OpKind::CaseStart { .. }, _) => false,
            (OpKind::Case(_), Dialect::High) => true,
            (OpKind::Case(_), _) => false,
            (OpKind::CaseValues, Dialect::High) => true,
            (OpKind::CaseValues, _) => false,
            (OpKind::CaseGuardOk, Dialect::High) => true,
            (OpKind::CaseGuardOk, _) => false,
            (OpKind::CaseGuardFail { .. }, Dialect::High) => true,
            (OpKind::CaseGuardFail { .. }, _) => false,

            _ => true,
        }
    }

}
