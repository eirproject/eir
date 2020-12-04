#![feature(specialization, raw, allocator_api)]
//#![deny(warnings)]

use std::cmp::Ordering;
use std::fmt::{Display, Formatter};

use libeir_intern::Ident;

mod function;

mod dialect;
pub use dialect::{ArcDialect, Dialect};

pub mod operation;

pub mod traits;

// Auxiliary utilities
mod algo;
pub use algo::equality::GraphEqOptions;
pub use algo::func_tree::{FunctionEntry, FunctionTree};
pub use algo::live::LiveValues;
pub use algo::mangle::{MangleFrom, MangleTarget, MangleTo, Mangler};
pub use algo::validate::ValidationError;

pub mod text;

pub mod graph;
pub use graph::LiveBlockGraph;

// Subcontainers
pub mod constant;
pub mod pattern;

pub use function::ValueKind;
pub use function::{AttributeKey, AttributeValue};
pub use function::{
    BasicType, BinOp, CallKind, LogicOp, MapPutUpdate, MatchKind, OpKind, PrimOpKind,
};
pub use function::{Block, Function, Location, PrimOp, Value};
pub use function::{ContainerDebug, ContainerDebugAdapter};

pub use function::builder::{DynValue, FunctionBuilder, IntoValue};

pub use constant::EmptyMap;
pub use constant::{AtomTerm, BigIntTerm, BinaryTerm, FloatTerm, IntTerm, NilTerm};
pub use constant::{AtomicTerm, Const, ConstKind, ConstantContainer};
pub use constant::{FromPrimitive, Integer, ToPrimitive};

pub use pattern::{PatternClause, PatternContainer, PatternNode, PatternValue};

pub use text::printer::{FormatConfig, StandardFormatConfig};
pub use text::{
    parse_function, parse_function_map, parse_function_map_unwrap, parse_function_unwrap,
    parse_module, parse_module_unwrap,
};

pub mod binary;
pub use binary::{BinaryEntrySpecifier, Endianness};

mod module;
pub use module::{FunctionDefinition, FunctionIndex, Module};

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq, PartialOrd)]
pub struct FunctionIdent {
    pub module: Ident,
    pub name: Ident,
    pub arity: usize,
}
impl Ord for FunctionIdent {
    fn cmp(&self, other: &FunctionIdent) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}
impl Display for FunctionIdent {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}:{}/{}", self.module, self.name, self.arity)
    }
}
