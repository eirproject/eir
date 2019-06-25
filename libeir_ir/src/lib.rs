use std::fmt::{ Display, Formatter };
use std::collections::HashMap;

use libeir_intern::Ident;

pub mod op;
pub use op::{ OpKind, BinOp };

pub mod text;

pub mod pattern;
pub use pattern::{ PatternNode, PatternValue, PatternContainer };

pub mod fun;
pub use fun::{ Function, Block, Value };
pub use fun::{ FunctionBuilder, CaseBuilder, IntoValue };
pub use fun::{ ValueType };
pub use fun::{ Dialect };
pub use fun::{ AttributeKey, AttributeValue };

//pub mod env;
//pub use env::{ ModuleEnvs, ClosureEnv };

pub mod constant;
pub use constant::{ ConstantContainer, Const, ConstValue };
pub use constant::{ AtomTerm, BigIntTerm, IntTerm, FloatTerm, BinaryTerm, NilTerm };

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

    pub fn add_function<'a>(&'a mut self, name: Ident, arity: usize) -> &'a mut Function {
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

}

