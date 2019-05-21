use crate::{ FunctionIdent, ClosureEnv, Atom, Clause };
use crate::{ Value, AtomicTerm };
use crate::{ Dialect };

#[derive(Debug, Clone)]
pub enum TestOperation {
    EqualAtomic(AtomicTerm),
    /// Tests if a value is truthy according to Erlang
    /// truthiness rules
    IsTruthy,
}

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

#[derive(Debug, Clone)]
pub enum OpKind {

    // Control flow/functions

    /// Calls r[0] with args r[1..]
    /// This is the calling primitive,
    /// doing everything from returns to local calls,
    /// to external calls.
    /// Always a block terminator.
    Call,

    /// Captures r[0]:r[1]/r[2], writing it to w[0]
    CaptureFunction,

    /// Move r[0] into w[0]
    Move,

    // Raw exception handling.
    // This gets the stack trace from a raw trace
    ExcTrace,

    MakeTuple,
    MakeList,
    MakeBinary,

    /// (cont: fn())
    /// Creates a new empty map term.
    MapEmpty,
    /// (cont: fn(), map: map, key, value)
    /// Puts a new value in the map, replacing the old key
    /// if it exists.
    /// The map argument is expected to be a map. Codegen
    /// should not need to do a term tag check on this.
    MapPut,
    /// (ok: fn(new_map), err: fn(), map: map, key, value)
    /// Puts a new value in a map, failing if the key is
    /// not already set.
    /// The map argument is expected to be a map. Codegen
    /// should not need to do a term tag check on this.
    MapUpdate,

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
    ///
    /// Only high level Eir may contain value lists. Codegen
    /// should not be concerned with these operations.
    PackValueList,
    UnpackValueList,

    PrimOp(Atom),

    // Tests
    /// (ok: fn(), fail: fn(), lhs, rhs)
    /// Compares two terms according to the given binary
    /// operator.
    Compare(ComparisonOperation),
    /// (ok: fn(), fail: fn(), term)
    /// Performs a test on the given term.
    Test(TestOperation),

    // Case structure
    /// ```
    /// (
    ///     no_match: fn(),
    ///     clause1_guard: fn(ok: fn(), fail: fn(), pat_refs..),
    ///     clause1_body: fn(pat_refs..),
    ///     clauseN_guard: fn(ok: fn(), fail: fn(), pat_refs..),
    ///     clauseN_body: fn(pat_refs..),
    ///     match_val: valuelist,
    ///     match_values..
    /// )
    /// ```
    /// High level matching construct, lowered to explicit control flow
    /// in a Eir compiler pass. Only allowed in high level Eir dialect.
    /// This OP indicates the start of a case structure.
    /// A guard is strictly required to return through either the ok
    /// or fail continuation.
    Case {
        clauses: Vec<Clause>,
    },


    /// (ok: fn(tuple_elem..), fail: fn(), term)
    UnpackTuple(usize),
    /// (ok: fn(head, tail), fail: fn(), term)
    UnpackListCell,
    /// (ok: fn(value), fail: fn(), map: map, key)
    UnpackMapItem,

    /// ()
    /// Something that should not happen. The VM could be left in an
    /// invalid state, should raise an unrecoverable runtime error.
    Unreachable,
}

impl OpKind {

    pub fn is_block_terminator(&self) -> bool {
        match self {
            OpKind::Call => true,
            OpKind::MapUpdate => true,
            OpKind::PrimOp(_) => true,
            OpKind::Compare(_) => true,
            OpKind::Test(_) => true,
            OpKind::Case { .. } => true,
            OpKind::UnpackTuple(_) => true,
            OpKind::UnpackListCell => true,
            OpKind::UnpackMapItem => true,
            OpKind::Unreachable => true,
            _ => false,
        }
    }

}
