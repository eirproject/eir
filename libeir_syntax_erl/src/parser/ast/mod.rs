mod attributes;
mod expr;
mod functions;
mod module;
mod types;

use libeir_diagnostics::SourceIndex;

pub use self::attributes::*;
pub use self::expr::*;
pub use self::functions::*;
pub use self::module::*;
pub use self::types::*;
pub use super::{ParseError, ParserError};
pub use crate::lexer::{Ident, Symbol};

use crate::lexer::Token;
use crate::preprocessor::PreprocessorError;

/// Used for AST functions which need to raise an error to the parser directly
pub type TryParseResult<T> =
    Result<T, lalrpop_util::ParseError<SourceIndex, Token, PreprocessorError>>;

/// Represents either a concrete name (an atom) or a variable name (an identifier).
/// This is used in constructs where either are permitted.
#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Name {
    Atom(Ident),
    Var(Ident),
}
impl Name {
    pub fn symbol(&self) -> Symbol {
        match self {
            Name::Atom(Ident { ref name, .. }) => name.clone(),
            Name::Var(Ident { ref name, .. }) => name.clone(),
        }
    }
}
impl PartialOrd for Name {
    fn partial_cmp(&self, other: &Name) -> Option<std::cmp::Ordering> {
        self.symbol().partial_cmp(&other.symbol())
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd)]
pub enum Arity {
    Int(usize),
    Var(Ident),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, PartialOrd)]
pub struct NodeId(pub usize);

#[derive(Debug, Clone)]
pub struct NodeIdGenerator(usize);
impl NodeIdGenerator {
    pub fn new() -> Self {
        NodeIdGenerator(0)
    }

    pub fn next(&mut self) -> NodeId {
        self.0 += 1;
        NodeId(self.0)
    }
}

/// The set of all binary operators which may be used in expressions
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum BinaryOp {
    // 100 !, right associative
    Send,
    // 150 orelse
    OrElse,
    // 160 andalso
    AndAlso,
    // 200 <all comparison operators>
    Equal, // right associative
    NotEqual,
    Lte,
    Lt,
    Gte,
    Gt,
    StrictEqual,
    StrictNotEqual,
    // 300 <all list operators>, right associative
    Append,
    Remove,
    // 400 <all add operators>, left associative
    Add,
    Sub,
    Bor,
    Bxor,
    Bsl,
    Bsr,
    Or,
    Xor,
    // 500 <all mul operators>, left associative
    Divide,
    Multiply,
    Div,
    Rem,
    Band,
    And,
}

/// The set of all unary (prefix) operators which may be used in expressions
#[derive(Debug, Copy, Clone, PartialEq)]
pub enum UnaryOp {
    // 600 <all prefix operators>
    Plus,
    Minus,
    Bnot,
    Not,
}
