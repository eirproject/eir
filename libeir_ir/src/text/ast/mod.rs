use libeir_intern::Ident;

use crate::constant::Integer;

mod lower;
pub use lower::{LowerError, LowerMap};

mod raise;

#[derive(Debug, PartialEq, Eq)]
pub struct Module {
    pub name: Ident,
    pub items: Vec<ModuleItem>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ModuleItem {
    Function(Function),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Function {
    pub name: Ident,
    pub arity: Integer,
    pub items: Vec<FunctionItem>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum FunctionItem {
    Label(Label),
    Assignment(Assignment),
    Op(Op),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Label {
    pub name: Value,
    // Only Value::Value is supported here
    pub args: Vec<Value>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Assignment {
    pub lhs: Value,
    pub rhs: Value,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Op {
    UnpackValueList(UnpackValueListOp),
    Call(CallOp),
    IfBool(IfBoolOp),
    Unreachable,
}

#[derive(Debug, PartialEq, Eq)]
pub struct UnpackValueListOp {
    pub arity: usize,
    pub value: Value,
    pub block: Value,
}

#[derive(Debug, PartialEq, Eq)]
pub struct CallOp {
    pub target: Value,
    pub args: Vec<Value>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct IfBoolOp {
    pub value: Value,
    pub tru: Value,
    pub fal: Value,
    pub or: Option<Value>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Value {
    // Atomics
    Value(Ident),
    Block(Ident),
    Atom(Ident),
    Integer(Integer),
    Nil,

    // Composites
    ValueList(Vec<Value>),
    Tuple(Vec<Value>),
    CaptureFunction(Box<Value>, Box<Value>, Box<Value>),
}
impl Value {
    pub fn value(&self) -> Option<Ident> {
        match self {
            Value::Value(sym) => Some(*sym),
            _ => None,
        }
    }
    pub fn block(&self) -> Option<Ident> {
        match self {
            Value::Block(sym) => Some(*sym),
            _ => None,
        }
    }
}
