#![feature(specialization)]
#![deny(warnings)]

use std::fmt::{ Display, Formatter };
use std::cmp::Ordering;

use libeir_intern::Ident;

mod function;

// Auxiliary utilities
mod algo;
pub use algo::mangle::*;
pub use algo::validate::ValidationError;

pub mod text;

mod graph;
pub use graph::LiveBlockGraph;

// Subcontainers
pub mod constant;
pub mod pattern;

pub use function::{ Function, Block, Value, PrimOp, Location };
pub use function::{ OpKind, MatchKind, BasicType, MapPutUpdate, PrimOpKind, BinOp,
                    LogicOp, CallKind };
pub use function::{ ValueKind };
pub use function::{ Dialect };
pub use function::{ AttributeKey, AttributeValue };
pub use function::{ ContainerDebug, ContainerDebugAdapter };

pub use function::builder::{ FunctionBuilder, CaseBuilder, IntoValue };

pub use algo::live::LiveValues;
pub use algo::mangle::{Mangler, MangleTarget, MangleTo, MangleFrom};

pub use constant::{ ConstantContainer, Const, ConstKind, AtomicTerm };
pub use constant::{ AtomTerm, BigIntTerm, IntTerm, FloatTerm, BinaryTerm, NilTerm };
pub use constant::{ EmptyMap };

pub use pattern::{ PatternNode, PatternValue, PatternClause, PatternContainer };

pub use text::{
    parse_function, parse_function_unwrap,
    parse_function_map, parse_function_map_unwrap,
    parse_module, parse_module_unwrap
};
pub use text::printer::{ ToEirText, ToEirTextContext };

pub mod binary;
pub use binary::{ BinaryEntrySpecifier, Endianness };

mod module;
pub use module::{Module, FunctionDefinition, FunctionIndex};

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
