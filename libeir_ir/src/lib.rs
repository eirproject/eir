use std::fmt::{ Display, Formatter };
use std::collections::HashMap;

use libeir_intern::Ident;

pub mod op;
pub use op::{ TestOperation, ComparisonOperation };

//pub mod text;

pub mod pattern;
pub use pattern::{ PatternNode, PatternValue, PatternContainer };

pub mod fun;
pub use fun::{ Function, Block, Value };
pub use fun::{ FunctionBuilder, CaseBuilder };
pub use fun::{ ValueType };
pub use fun::{ Dialect };
pub use fun::{ AttributeKey, AttributeValue };

pub mod env;
pub use env::{ ModuleEnvs, ClosureEnv };

pub mod constant;
pub use constant::{ AtomicTerm, ConstantTerm };
pub use constant::{ AtomTerm, IntegerTerm, FloatTerm, BinaryTerm, NilTerm };

#[derive(Debug)]
pub struct Module {
    pub name: Ident,
    pub envs: ModuleEnvs,
    pub functions: HashMap<FunctionIdent, Function>,
}

#[derive(Debug, Clone, Hash, Eq, PartialEq)]
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
