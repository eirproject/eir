use crate::pattern::PatternClause;
use crate::binary::BinaryEntrySpecifier;

use libeir_intern::Symbol;

use cranelift_entity::EntityList;

use serde::{ Serialize, Deserialize };

#[derive(Debug, Copy, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum CallKind {
    /// Control flow includes flow within a function and calls to
    /// escape values.
    ControlFlow,
    /// Call to a function, should generate a stack frame.
    /// The first two arguments MUST be the return and throw
    /// continuations respectively.
    Function,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum BasicType {
    // Contains both ListCell and Nil
    List,
    ListCell,
    Nil,

    /// Arity is part of the type
    Tuple(usize),

    Map,

    /// Contains both Float and Integer
    Number,
    Float,
    /// Contains both SmallInt and BigInt
    Integer,
    SmallInteger,
    BigInteger,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum MatchKind {
    /// One read, the value to test it against
    /// No arguments.
    Value,
    /// No reads.
    /// No writes.
    Type(BasicType),
    /// One optional read, the size.
    /// Two arguments, the decoded value, and the tail.
    Binary(BinaryEntrySpecifier),
    /// No reads.
    /// N arguments, the unpacked values.
    Tuple(usize),
    /// No reads.
    /// Two arguments, the head and the tail.
    ListCell,
    /// One read, the key.
    /// One argument, the value.
    MapItem,
    /// No reads.
    /// No arguments.
    Wildcard,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Serialize, Deserialize)]
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
    Call(CallKind),

    /// (true: fn(), false: fn(), else: fn(), value)
    /// (true: fn(), false: fn(), value) implies else is unreachable
    /// Strict truth check, only 'true' is true, 'false' is false
    IfBool,

    // Stack traces
    /// This captures the current stack trace.
    /// Returns an implementation specific value that can only be
    /// used with `TraceConstruct`. Can not be exposed to the user
    /// or used with any other operation.
    TraceCaptureRaw,
    /// This gets the stack trace from a raw trace.
    TraceConstruct,

    /// (ok: fn(new_map), err: fn(), map: map, keys: (keys..), values: (value..))
    /// Puts a new value in the map, replacing the old key
    /// if it exists.
    MapPut {
        // TODO: don't do allocation
        action: Vec<MapPutUpdate>,
    },

    /// (cont: fn(terms..), l: valuelist)
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
    UnpackValueList(usize),

    // Case structure
    /// ```ignore
    /// (
    ///     no_match: fn(),
    ///     clause_guards: (fn(ok: fn(), fail: fn(), pat_refs..)..)
    ///     clause_bodies: (fn(pat_refs..)..)
    ///     match_val: (term..),
    ///     match_values: (term..),
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

    // Binary construction
    /// (ok: fn(bin), fail: fn(), value)
    /// (ok: fn(bin), fail: fn(), value, size)
    BinaryPush {
        specifier: BinaryEntrySpecifier,
    },

    /// Match on a single value.
    /// Branches are tested in order, first matched is branched to.
    /// ```ignore
    /// (
    ///     branches: (fn(..), ..),
    ///     value: term,
    ///     branch1_args: (..),
    ///     ..
    /// )
    /// ```
    Match {
        branches: Vec<MatchKind>,
    },

    /// ()
    /// Something that should not happen. The VM could be left in an
    /// invalid state, should raise an unrecoverable runtime error.
    Unreachable,

    Intrinsic(Symbol),
}

impl OpKind {

    pub fn is_call(&self) -> bool {
        match self {
            OpKind::Call(_) => true,
            _ => false,
        }
    }

}
