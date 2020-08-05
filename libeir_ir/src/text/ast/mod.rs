use libeir_diagnostics::SourceSpan;
use libeir_intern::Ident;

use crate::constant::Integer;
use crate::{BasicType, BinOp, BinaryEntrySpecifier};

#[derive(Debug, PartialEq, Eq)]
pub enum DynToken {
    Parens(Vec<DynToken>, SourceSpan),
    Braces(Vec<DynToken>, SourceSpan),
    MapBraces(Vec<DynToken>, SourceSpan),
    SquareBrackets(Vec<DynToken>, SourceSpan),
    AngleBrackets(Vec<DynToken>, SourceSpan),

    Ident(Ident),
    Variable(Ident),

    Atom(Ident),
    Integer(Integer, SourceSpan),
    Float(Ident),
    String(Ident),

    Percent(SourceSpan),
    Colon(SourceSpan),
    SemiColon(SourceSpan),
    Comma(SourceSpan),
    Question(SourceSpan),
    ForwardSlash(SourceSpan),
    Equals(SourceSpan),
    EqualsEquals(SourceSpan),
    FatArrow(SourceSpan),
    Underscore(SourceSpan),
    Pipe(SourceSpan),
    At(SourceSpan),
    Bang(SourceSpan),

    Unpack(SourceSpan),
    Unreachable(SourceSpan),
    Arity(SourceSpan),
    IfBool(SourceSpan),
    TraceCaptureRaw(SourceSpan),
    Value(SourceSpan),
    Match(SourceSpan),
    Type(SourceSpan),
    Case(SourceSpan),
    Guard(SourceSpan),
    Except(SourceSpan),
}

impl DynToken {
    pub fn span(&self) -> SourceSpan {
        use DynToken::*;
        match self {
            Parens(_, span) => *span,
            Braces(_, span) => *span,
            MapBraces(_, span) => *span,
            SquareBrackets(_, span) => *span,
            AngleBrackets(_, span) => *span,

            Ident(ident) => ident.span,
            Variable(ident) => ident.span,

            Atom(ident) => ident.span,
            Integer(_, span) => *span,
            Float(ident) => ident.span,
            String(ident) => ident.span,

            Percent(span) => *span,
            Colon(span) => *span,
            SemiColon(span) => *span,
            Comma(span) => *span,
            Question(span) => *span,
            ForwardSlash(span) => *span,
            Equals(span) => *span,
            EqualsEquals(span) => *span,
            FatArrow(span) => *span,
            Underscore(span) => *span,
            Pipe(span) => *span,
            At(span) => *span,
            Bang(span) => *span,

            Unpack(span) => *span,
            Unreachable(span) => *span,
            Arity(span) => *span,
            IfBool(span) => *span,
            TraceCaptureRaw(span) => *span,
            Value(span) => *span,
            Match(span) => *span,
            Type(span) => *span,
            Case(span) => *span,
            Guard(span) => *span,
            Except(span) => *span,
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Module {
    pub span: SourceSpan,
    pub name: Ident,
    pub items: Vec<ModuleItem>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ModuleItem {
    Function(Function),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Function {
    pub span: SourceSpan,
    pub name: Ident,
    pub arity: Integer,
    pub items: Vec<FunctionItem>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum FunctionItem {
    Label(Label),
    Meta(Meta),
    Assignment(Assignment),
    Op(Op),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Meta {
    pub span: SourceSpan,
    pub name: Ident,
    pub tokens: Vec<DynToken>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Label {
    pub span: SourceSpan,
    pub name: Value,
    // Only Value::Value is supported here
    pub args: Vec<Value>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Assignment {
    pub span: SourceSpan,
    pub lhs: Value,
    pub rhs: Value,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Op {
    Dyn(Ident, Vec<DynToken>),
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
    pub span: SourceSpan,
    pub value: Value,
    pub entries: Vec<CaseEntry>,
    pub no_match: Option<Value>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct CaseEntry {
    pub span: SourceSpan,
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
    pub span: SourceSpan,
    pub value: Value,
    pub entries: Vec<MatchEntry>,
}
#[derive(Debug, PartialEq, Eq)]
pub struct MatchEntry {
    pub span: SourceSpan,
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
    pub span: SourceSpan,
    pub arity: usize,
    pub value: Value,
    pub block: Value,
}

#[derive(Debug, PartialEq, Eq)]
pub struct CallControlFlowOp {
    pub span: SourceSpan,
    pub target: Value,
    pub args: Vec<Value>,
}
#[derive(Debug, PartialEq, Eq)]
pub struct CallFunctionOp {
    pub span: SourceSpan,
    pub target: Value,
    pub ret: Value,
    pub thr: Value,
    pub args: Vec<Value>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct IfBoolOp {
    pub span: SourceSpan,
    pub value: Value,
    pub tru: Value,
    pub fal: Value,
    pub or: Option<Value>,
}

#[derive(Debug, PartialEq, Eq)]
pub struct TraceCaptureRawOp {
    pub span: SourceSpan,
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
