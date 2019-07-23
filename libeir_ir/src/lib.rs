use std::fmt::{ Display, Formatter };
use std::collections::HashMap;

use libeir_intern::Ident;

mod function;

// Auxiliary utilities
mod algo;
mod graph;
pub mod text;

// Subcontainers
pub mod constant;
pub mod pattern;

pub use function::{ Function, Block, Value, PrimOp };
pub use function::{ OpKind, MapPutUpdate, PrimOpKind, BinOp };
pub use function::{ ValueKind };
pub use function::{ Dialect };
pub use function::{ AttributeKey, AttributeValue };

pub use function::builder::{ FunctionBuilder, CaseBuilder, IntoValue };

pub use algo::live::LiveValues;
//pub use algo::mangle::Mangler;

pub use constant::{ ConstantContainer, Const, ConstKind };
pub use constant::{ AtomTerm, BigIntTerm, IntTerm, FloatTerm, BinaryTerm, NilTerm };

pub use pattern::{ PatternNode, PatternValue, PatternClause, PatternContainer };

pub use text::printer::{ ToEirText, ToEirTextContext };

pub mod binary;

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq, PartialOrd)]
pub struct FunctionIdent {
    pub module: Ident,
    pub name: Ident,
    pub arity: usize,
}
impl Display for FunctionIdent {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}:{}/{}", self.module, self.name, self.arity)
    }
}

#[derive(Debug)]
pub struct Module {
    pub name: Ident,
    pub functions: HashMap<FunctionIdent, Function>,
}
impl Module {

    pub fn new(name: Ident) -> Self {
        Module {
            name,
            functions: HashMap::new(),
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

