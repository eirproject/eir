use std::fmt::{ Display, Formatter };
use std::collections::HashMap;

pub mod intern;
pub use intern::Atom;

pub mod op;

pub mod ssa;
pub use ssa::{ SSAVariable, SSAVariableGenerator };

pub mod cfg;

pub mod text;

pub mod pattern;
pub use pattern::{ Clause, Pattern };

pub struct Module {
    pub name: Atom,
    pub lambda_envs: HashMap<LambdaEnvIdx, LambdaEnv>,
    pub functions: HashMap<FunctionIdent, Function>,
}

#[derive(Debug, Clone)]
pub struct Function {
    pub ident: FunctionIdent,
    pub lir: cfg::FunctionCfg,
}

#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
pub struct LambdaEnvIdx(usize);
impl LambdaEnvIdx {
    pub fn parse_from_str(string: &str) -> Self {
        LambdaEnvIdx(string.parse().unwrap())
    }
}
impl Display for LambdaEnvIdx {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Debug)]
pub struct LambdaEnvIdxGenerator(LambdaEnvIdx);
impl LambdaEnvIdxGenerator {
    pub fn new() -> Self {
        LambdaEnvIdxGenerator(LambdaEnvIdx(0))
    }
    pub fn next(&mut self) -> LambdaEnvIdx {
        (self.0).0 += 1;
        self.0
    }
}

#[derive(Debug, Clone)]
pub struct LambdaEnv {
    pub num_captures: usize,
    pub meta_binds: Vec<FunctionIdent>,
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct FunctionIdent {
    pub module: Atom,
    pub name: Atom,
    pub arity: usize,
    pub lambda: Option<(LambdaEnvIdx, usize)>,
}

impl Display for FunctionIdent {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        if let Some(lambda_num) = self.lambda {
            write!(f, "{}:{}@{}.{}/{}", self.module, self.name,
                   (lambda_num.0).0, lambda_num.1, self.arity)
        } else {
            write!(f, "{}:{}/{}", self.module, self.name, self.arity)
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AtomicTerm {
    Integer(num_bigint::BigInt),
    Float,
    Atom(Atom),
    Char(char),
    String(String),
    Nil,
}
impl From<Atom> for AtomicTerm {
    fn from(val: Atom) -> Self {
        AtomicTerm::Atom(val)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConstantTerm {
    Atomic(AtomicTerm),
    List(Vec<ConstantTerm>, Box<ConstantTerm>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Source {
    Variable(SSAVariable),
    Constant(ConstantTerm),
}
impl From<SSAVariable> for Source {
    fn from(val: SSAVariable) -> Self {
        Source::Variable(val)
    }
}
impl From<ConstantTerm> for Source {
    fn from(val: ConstantTerm) -> Self {
        Source::Constant(val)
    }
}
impl From<AtomicTerm> for Source {
    fn from(val: AtomicTerm) -> Self {
        Source::Constant(ConstantTerm::Atomic(val))
    }
}
impl From<Atom> for Source {
    fn from(val: Atom) -> Self {
        Source::Constant(ConstantTerm::Atomic(AtomicTerm::Atom(val)))
    }
}
