use crate::Const;
use crate::pattern::{ PatternClause };

use libeir_intern::Symbol;

use cranelift_entity::EntityList;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BinOp {
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

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum MapPutUpdate {
    /// Value is put into map, regardless of already existing
    Put,
    /// Value is updated, fails if not in map
    Update,
}

#[derive(Debug, Clone)]
pub enum OpKind {

    // Control flow/functions

    /// (call: fn(..), ..)
    /// This is the calling primitive,
    /// doing everything from returns to local calls,
    /// to external calls.
    Call,

    /// (cont: fn(fun), m, f, a)
    CaptureFunction,

    /// (true: fn(), false: fn(), else: fn(), value)
    /// Strict truth check, only 'true' is true, 'false' is false
    IfBool,

    // BinOp
    /// (true: fn(), false: fn(), lhs, rhs)
    /// Branching variant
    BinOp(BinOp),
    /// (cont: fn(bool), lhs, rhs)
    /// Boolean variant
    BinOpValue(BinOp),

    /// (..)
    Intrinsic(Symbol),

    // Raw exception handling.
    // This gets the stack trace from a raw trace
    // TODO: this needs refinement
    //ExcTrace,

    /// (cont: fn(tup), terms..)
    MakeTuple,
    /// (cont: fn(list), tail, heads..)
    MakeList,
    // TODO
    //MakeBinary,

    /// (cont: fn(new_map))
    /// Creates a new empty map term.
    MapEmpty,
    /// (ok: fn(new_map), err: fn(), map: map, key1, value1, keyN, valueN)
    /// Puts a new value in the map, replacing the old key
    /// if it exists.
    MapPut {
        // TODO: don't do allocation
        action: Vec<MapPutUpdate>,
    },

    /// (cont: fn(l: valuelist), terms..)
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
    /// Only high level Eur may contain value lists. Codegen
    /// should not be concerned with these operations.
    PackValueList,
    /// (cont: fn(terms..), l: valuelist)
    UnpackValueList(usize),

    // Case structure
    /// ```ignore
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
        clauses: EntityList<PatternClause>,
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
