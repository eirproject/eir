use std::fmt::{ Display, Formatter };

pub mod intern;
pub use intern::Atom;

pub mod op;

pub mod ssa;
pub use ssa::{ SSAVariable, SSAVariableGenerator };

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub struct LambdaEnvIdx(usize);

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct FunctionIdent {
    module: Atom,
    name: Atom,
    arity: usize,
    lambda: Option<(LambdaEnvIdx, usize)>,
}

impl Display for FunctionIdent {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        if let Some(lambda_num) = self.lambda {
            write!(f, "{}@{}.{}/{}", self.name,
                   (lambda_num.0).0, lambda_num.1, self.arity)
        } else {
            write!(f, "{}/{}", self.name, self.arity)
        }
    }
}

#[derive(Debug, Clone)]
pub struct Clause {}

#[derive(Debug, Clone)]
pub enum AtomicTerm {
    Integer,
    Float,
    Atom(Atom),
    Char(char),
    String(String),
    Nil,
}

#[derive(Debug, Clone)]
pub enum ConstantTerm {
    Atomic(AtomicTerm),
}
