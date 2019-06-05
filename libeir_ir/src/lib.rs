use std::fmt::{ Display, Formatter };
use std::collections::HashMap;

pub mod intern;
pub use intern::{ Atom, Variable };

pub mod op;
pub use op::{ TestOperation, ComparisonOperation };

//pub mod text;

pub mod pattern;
pub use pattern::{ PatternNode, PatternValue, PatternContainer };

pub mod fun;
pub use fun::{ Function, FunctionBuilder, Block, Value };
pub use fun::{ ValueType };
pub use fun::{ Dialect };
pub use fun::{ AttributeKey, AttributeValue };

pub mod env;
pub use env::{ ModuleEnvs, ClosureEnv };

pub mod constant;
pub use constant::{ AtomicTerm, ConstantTerm };

#[derive(Debug)]
pub struct Module {
    pub name: Atom,
    pub envs: ModuleEnvs,
    pub functions: HashMap<FunctionIdent, Function>,
}

#[derive(Debug, Clone, Hash, Eq, PartialEq, PartialOrd, Ord)]
pub struct FunctionIdent {
    pub module: Atom,
    pub name: Atom,
    pub arity: usize,
}

impl Display for FunctionIdent {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "{}:{}/{}", self.module, self.name, self.arity)
    }
}
