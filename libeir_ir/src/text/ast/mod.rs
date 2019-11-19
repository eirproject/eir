use libeir_intern::Ident;

use crate::{BasicType, BinaryEntrySpecifier, BinOp};
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
    CallControlFlow(CallControlFlowOp),
    CallFunction(CallFunctionOp),
    IfBool(IfBoolOp),
    TraceCaptureRaw(TraceCaptureRawOp),
    Match(MatchOp),
    Case(CaseOp),
    Unreachable,
}

#[derive(Debug, PartialEq, Eq)]
pub struct CaseOp {
    pub value: Value,
    pub entries: Vec<CaseEntry>,
    pub no_match: Option<Value>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct CaseEntry {
    pub patterns: Vec<CasePattern>,
    pub args: Vec<Ident>,
    pub guard: Value,
    pub target: Value,
}

#[derive(Debug, PartialEq, Eq)]
pub enum CasePattern {
    Value(Value),
    Binding {
        name: Ident,
        pattern: Box<CasePattern>,
    },
    ListCell {
        head: Box<CasePattern>,
        tail: Box<CasePattern>,
    },
    Tuple {
        elements: Vec<CasePattern>,
    },
    Wildcard,
}

#[derive(Debug, PartialEq, Eq)]
pub struct MatchOp {
    pub value: Value,
    pub entries: Vec<MatchEntry>,
}
#[derive(Debug, PartialEq, Eq)]
pub struct MatchEntry {
    pub target: Value,
    pub kind: MatchKind,
}
#[derive(Debug, PartialEq, Eq)]
pub enum MatchKind {
    Value(Value),
    Type(BasicType),
    Binary(BinaryEntrySpecifier, Option<Value>),
    Tuple(usize),
    ListCell,
    MapItem(Value),
    Wildcard,
}

#[derive(Debug, PartialEq, Eq)]
pub struct UnpackValueListOp {
    pub arity: usize,
    pub value: Value,
    pub block: Value,
}

#[derive(Debug, PartialEq, Eq)]
pub struct CallControlFlowOp {
    pub target: Value,
    pub args: Vec<Value>,
}
#[derive(Debug, PartialEq, Eq)]
pub struct CallFunctionOp {
    pub target: Value,
    pub ret: Value,
    pub thr: Value,
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
pub struct TraceCaptureRawOp {
    pub then: Value,
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
    List(Vec<Value>, Option<Box<Value>>),
    CaptureFunction(Box<Value>, Box<Value>, Box<Value>),
    BinOp(Box<Value>, BinOp, Box<Value>),
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
