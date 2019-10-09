#![deny(warnings)]

use std::fmt::{ Display, Formatter };
use std::collections::BTreeMap;
use std::cmp::Ordering;

use libeir_intern::Ident;

mod function;

// Auxiliary utilities
mod algo;
pub use algo::mangle::*;

pub mod text;

mod graph;
pub use graph::LiveBlockGraph;

// Subcontainers
pub mod constant;
pub mod pattern;

pub use function::{ Function, Block, Value, PrimOp };
pub use function::{ OpKind, MatchKind, BasicType, MapPutUpdate, PrimOpKind, BinOp,
                    LogicOp };
pub use function::{ ValueKind };
pub use function::{ Dialect };
pub use function::{ AttributeKey, AttributeValue };

pub use function::builder::{ FunctionBuilder, CaseBuilder, IntoValue };

pub use algo::live::LiveValues;
pub use algo::mangle::Mangler;

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

#[derive(Debug)]
pub struct Module {
    pub name: Ident,
    pub functions: BTreeMap<FunctionIdent, Function>,
}
impl Module {

    pub fn new(name: Ident) -> Self {
        Module {
            name,
            functions: BTreeMap::new(),
        }
    }

    pub fn add_function(&mut self, name: Ident, arity: usize) -> &mut Function {
        let ident = FunctionIdent {
            module: self.name,
            name,
            arity,
        };
        assert!(!self.functions.contains_key(&ident));

        let fun = Function::new(ident);
        self.functions.insert(ident, fun);

        self.functions.get_mut(&ident).unwrap()
    }

    pub fn to_text(&self) -> String {
        let mut ctx = ToEirTextContext::new();

        let mut out = Vec::new();
        self.to_eir_text(&mut ctx, 0, &mut out).unwrap();
        String::from_utf8(out).unwrap()
    }

}

