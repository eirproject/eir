use std::cmp::Ordering;

use libeir_diagnostics::SourceSpan;
use libeir_util_number::{Float, Integer, Number};

use super::NodeId;
use super::{BinaryOp, Ident, UnaryOp};
use super::{Function, FunctionName, Guard, Name, Type};

use crate::lexer::DelayedSubstitution;

/// The set of all possible expressions
#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    // An identifier/variable/function reference
    Var(Var),
    Literal(Literal),
    FunctionName(FunctionName),
    // Delayed substitution of macro
    DelayedSubstitution(SourceSpan, NodeId, DelayedSubstitution),
    // The various list forms
    Nil(Nil),
    Cons(Cons),
    // Other data structures
    Tuple(Tuple),
    Map(Map),
    MapUpdate(MapUpdate),
    MapProjection(MapProjection),
    Binary(Binary),
    Record(Record),
    RecordAccess(RecordAccess),
    RecordIndex(RecordIndex),
    RecordUpdate(RecordUpdate),
    // Comprehensions
    ListComprehension(ListComprehension),
    BinaryComprehension(BinaryComprehension),
    Generator(Generator),
    BinaryGenerator(BinaryGenerator),
    // Complex expressions
    Begin(Begin),
    Apply(Apply),
    Remote(Remote),
    BinaryExpr(BinaryExpr),
    UnaryExpr(UnaryExpr),
    Match(Match),
    If(If),
    Catch(Catch),
    Case(Case),
    Receive(Receive),
    Try(Try),
    Fun(Function),
}
impl Expr {
    pub fn span(&self) -> SourceSpan {
        match self {
            &Expr::Var(Var(_, Ident { ref span, .. })) => span.clone(),
            &Expr::Literal(ref lit) => lit.span(),
            &Expr::FunctionName(ref name) => name.span(),
            &Expr::DelayedSubstitution(ref span, _, _) => *span,
            &Expr::Nil(Nil(ref span, _)) => span.clone(),
            &Expr::Cons(Cons { ref span, .. }) => span.clone(),
            &Expr::Tuple(Tuple { ref span, .. }) => span.clone(),
            &Expr::Map(Map { ref span, .. }) => span.clone(),
            &Expr::MapUpdate(MapUpdate { ref span, .. }) => span.clone(),
            &Expr::MapProjection(MapProjection { ref span, .. }) => span.clone(),
            &Expr::Binary(Binary { ref span, .. }) => span.clone(),
            &Expr::Record(Record { ref span, .. }) => span.clone(),
            &Expr::RecordAccess(RecordAccess { ref span, .. }) => span.clone(),
            &Expr::RecordIndex(RecordIndex { ref span, .. }) => span.clone(),
            &Expr::RecordUpdate(RecordUpdate { ref span, .. }) => span.clone(),
            &Expr::ListComprehension(ListComprehension { ref span, .. }) => span.clone(),
            &Expr::BinaryComprehension(BinaryComprehension { ref span, .. }) => span.clone(),
            &Expr::Generator(Generator { ref span, .. }) => span.clone(),
            &Expr::BinaryGenerator(BinaryGenerator { ref span, .. }) => span.clone(),
            &Expr::Begin(Begin { ref span, .. }) => span.clone(),
            &Expr::Apply(Apply { ref span, .. }) => span.clone(),
            &Expr::Remote(Remote { ref span, .. }) => span.clone(),
            &Expr::BinaryExpr(BinaryExpr { ref span, .. }) => span.clone(),
            &Expr::UnaryExpr(UnaryExpr { ref span, .. }) => span.clone(),
            &Expr::Match(Match { ref span, .. }) => span.clone(),
            &Expr::If(If { ref span, .. }) => span.clone(),
            &Expr::Catch(Catch { ref span, .. }) => span.clone(),
            &Expr::Case(Case { ref span, .. }) => span.clone(),
            &Expr::Receive(Receive { ref span, .. }) => span.clone(),
            &Expr::Try(Try { ref span, .. }) => span.clone(),
            &Expr::Fun(ref fun) => fun.span(),
        }
    }
    pub fn id(&self) -> NodeId {
        match self {
            Expr::Var(Var(id, _)) => *id,
            Expr::Literal(lit) => lit.id(),
            Expr::FunctionName(name) => name.id(),
            Expr::DelayedSubstitution(_, id, _) => *id,
            Expr::Nil(Nil(_, id)) => *id,
            Expr::Cons(cons) => cons.id,
            Expr::Tuple(tuple) => tuple.id,
            Expr::Map(map) => map.id,
            Expr::MapUpdate(map) => map.id,
            Expr::MapProjection(map) => map.id,
            Expr::Binary(bin) => bin.id,
            Expr::Record(rec) => rec.id,
            Expr::RecordAccess(rec) => rec.id,
            Expr::RecordIndex(rec) => rec.id,
            Expr::RecordUpdate(rec) => rec.id,
            Expr::ListComprehension(compr) => compr.id,
            Expr::BinaryComprehension(compr) => compr.id,
            Expr::Generator(gen) => gen.id,
            Expr::BinaryGenerator(gen) => gen.id,
            Expr::Begin(begin) => begin.id,
            Expr::Apply(apply) => apply.id,
            Expr::Remote(rem) => rem.id,
            Expr::BinaryExpr(bin) => bin.id,
            Expr::UnaryExpr(un) => un.id,
            Expr::Match(mat) => mat.id,
            Expr::If(expr) => expr.id,
            Expr::Catch(catch) => catch.id,
            Expr::Case(case) => case.id,
            Expr::Receive(rec) => rec.id,
            Expr::Try(tr) => tr.id,
            Expr::Fun(fun) => fun.id(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Var(pub NodeId, pub Ident);
impl PartialEq for Var {
    fn eq(&self, other: &Self) -> bool {
        self.1 == other.1
    }
}
impl Eq for Var {}

#[derive(Debug, Clone)]
pub struct Nil(pub SourceSpan, pub NodeId);
impl PartialEq for Nil {
    fn eq(&self, _: &Self) -> bool {
        return true;
    }
}
impl Eq for Nil {}

#[derive(Debug, Clone)]
pub struct Cons {
    pub span: SourceSpan,
    pub id: NodeId,
    pub head: Box<Expr>,
    pub tail: Box<Expr>,
}
impl PartialEq for Cons {
    fn eq(&self, other: &Self) -> bool {
        self.head == other.head && self.tail == other.tail
    }
}
//impl PartialOrd for Cons {
//    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
//        match self.head.partial_cmp(&other.head) {
//            None => self.tail.partial_cmp(&other.tail),
//            Some(order) => Some(order),
//        }
//    }
//}

#[derive(Debug, Clone)]
pub struct Tuple {
    pub span: SourceSpan,
    pub id: NodeId,
    pub elements: Vec<Expr>,
}
impl PartialEq for Tuple {
    fn eq(&self, other: &Self) -> bool {
        self.elements == other.elements
    }
}
//impl PartialOrd for Tuple {
//    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
//        self.elements.partial_cmp(&other.elements)
//    }
//}

#[derive(Debug, Clone)]
pub struct Map {
    pub span: SourceSpan,
    pub id: NodeId,
    pub fields: Vec<MapField>,
}
impl PartialEq for Map {
    fn eq(&self, other: &Self) -> bool {
        self.fields == other.fields
    }
}
//impl PartialOrd for Map {
//    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
//        self.fields.partial_cmp(&other.fields)
//    }
//}

// Updating fields on an existing map, e.g. `Map#{field1 = value1}.`
#[derive(Debug, Clone)]
pub struct MapUpdate {
    pub span: SourceSpan,
    pub id: NodeId,
    pub map: Box<Expr>,
    pub updates: Vec<MapField>,
}
impl PartialEq for MapUpdate {
    fn eq(&self, other: &Self) -> bool {
        self.map == other.map && self.updates == other.updates
    }
}

// Pattern matching a map expression
#[derive(Debug, Clone)]
pub struct MapProjection {
    pub span: SourceSpan,
    pub id: NodeId,
    pub map: Box<Expr>,
    pub fields: Vec<MapField>,
}
impl PartialEq for MapProjection {
    fn eq(&self, other: &Self) -> bool {
        self.map == other.map && self.fields == other.fields
    }
}

/// The set of literal values
///
/// This does not include tuples, lists, and maps,
/// even though those can be constructed at compile-time,
/// as some places that allow literals do not permit those
/// types
#[derive(Debug, Clone)]
pub enum Literal {
    Atom(NodeId, Ident),
    String(NodeId, Ident),
    Char(SourceSpan, NodeId, char),
    Integer(SourceSpan, NodeId, Integer),
    Float(SourceSpan, NodeId, Float),
}
impl Literal {
    pub fn span(&self) -> SourceSpan {
        match self {
            &Literal::Atom(_, Ident { ref span, .. }) => span.clone(),
            &Literal::String(_, Ident { ref span, .. }) => span.clone(),
            &Literal::Char(span, _, _) => span.clone(),
            &Literal::Integer(span, _, _) => span.clone(),
            &Literal::Float(span, _, _) => span.clone(),
        }
    }
    pub fn id(&self) -> NodeId {
        match self {
            Literal::Atom(id, _) => *id,
            Literal::String(id, _) => *id,
            Literal::Char(_, id, _) => *id,
            Literal::Integer(_, id, _) => *id,
            Literal::Float(_, id, _) => *id,
        }
    }
}
impl PartialEq for Literal {
    fn eq(&self, other: &Literal) -> bool {
        match (self, other) {
            (&Literal::Atom(_, ref lhs), &Literal::Atom(_, ref rhs)) => lhs == rhs,
            (&Literal::Atom(_, _), _) => false,
            (_, &Literal::Atom(_, _)) => false,
            (&Literal::String(_, ref lhs), &Literal::String(_, ref rhs)) => lhs == rhs,
            (&Literal::String(_, _), _) => false,
            (_, &Literal::String(_, _)) => false,
            (x, y) => x.partial_cmp(y) == Some(Ordering::Equal),
        }
    }
}
impl PartialOrd for Literal {
    // number < atom < reference < fun < port < pid < tuple < map < nil < list < bit string
    fn partial_cmp(&self, other: &Literal) -> Option<Ordering> {
        match (self, other) {
            (Literal::String(_, ref lhs), Literal::String(_, ref rhs)) => lhs.partial_cmp(rhs),
            (Literal::String(_, _), _) => Some(Ordering::Greater),
            (_, Literal::String(_, _)) => Some(Ordering::Less),
            (Literal::Atom(_, ref lhs), Literal::Atom(_, ref rhs)) => lhs.partial_cmp(rhs),
            (Literal::Atom(_, _), _) => Some(Ordering::Greater),
            (_, Literal::Atom(_, _)) => Some(Ordering::Less),

            (
                l @ (Literal::Integer(_, _, _) | Literal::Float(_, _, _) | Literal::Char(_, _, _)),
                r @ (Literal::Integer(_, _, _) | Literal::Float(_, _, _) | Literal::Char(_, _, _)),
            ) => {
                let to_num = |lit: &Literal| match lit {
                    Literal::Integer(_, _, x) => x.clone().into(),
                    Literal::Float(_, _, x) => x.clone().into(),
                    Literal::Char(_, _, x) => {
                        let int: Integer = (*x).into();
                        int.into()
                    }
                    _ => unreachable!(),
                };

                let ln: Number = to_num(l);
                let rn: Number = to_num(r);

                ln.partial_cmp(&rn)
            }

            _ => unimplemented!(),
        }
    }
}

/// Maps can have two different types of field assignment:
///
/// * assoc - inserts or updates the given key with the given value
/// * exact - updates the given key with the given value, or produces an error
#[derive(Debug, Clone)]
pub enum MapField {
    Assoc {
        span: SourceSpan,
        id: NodeId,
        key: Expr,
        value: Expr,
    },
    Exact {
        span: SourceSpan,
        id: NodeId,
        key: Expr,
        value: Expr,
    },
}
impl MapField {
    pub fn key(&self) -> Expr {
        match self {
            &MapField::Assoc { ref key, .. } => key.clone(),
            &MapField::Exact { ref key, .. } => key.clone(),
        }
    }

    pub fn value(&self) -> Expr {
        match self {
            &MapField::Assoc { ref value, .. } => value.clone(),
            &MapField::Exact { ref value, .. } => value.clone(),
        }
    }

    pub fn span(&self) -> SourceSpan {
        match self {
            MapField::Assoc { span, .. } => *span,
            MapField::Exact { span, .. } => *span,
        }
    }
}
impl PartialEq for MapField {
    fn eq(&self, other: &Self) -> bool {
        (self.key() == other.key()) && (self.value() == other.value())
    }
}
//impl PartialOrd for MapField {
//    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
//        match self.key().partial_cmp(&other.key()) {
//            None => None,
//            Some(Ordering::Equal) => self.value().partial_cmp(&other.value()),
//            Some(order) => Some(order),
//        }
//    }
//}

#[derive(Debug, Clone)]
pub struct Record {
    pub span: SourceSpan,
    pub id: NodeId,
    pub name: Ident,
    pub fields: Vec<RecordField>,
}
impl PartialEq for Record {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.fields == other.fields
    }
}

// Accessing a record field value, e.g. Expr#myrec.field1
#[derive(Debug, Clone)]
pub struct RecordAccess {
    pub span: SourceSpan,
    pub id: NodeId,
    pub record: Box<Expr>,
    pub name: Ident,
    pub field: Ident,
}
impl PartialEq for RecordAccess {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.field == other.field && self.record == other.record
    }
}

// Referencing a record fields index, e.g. #myrec.field1
#[derive(Debug, Clone)]
pub struct RecordIndex {
    pub span: SourceSpan,
    pub id: NodeId,
    pub name: Ident,
    pub field: Ident,
}
impl PartialEq for RecordIndex {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.field == other.field
    }
}

// Update a record field value, e.g. Expr#myrec.field1
#[derive(Debug, Clone)]
pub struct RecordUpdate {
    pub span: SourceSpan,
    pub id: NodeId,
    pub record: Box<Expr>,
    pub name: Ident,
    pub updates: Vec<RecordField>,
}
impl PartialEq for RecordUpdate {
    fn eq(&self, other: &Self) -> bool {
        self.name == other.name && self.record == other.record && self.updates == other.updates
    }
}

/// Record fields always have a name, but both default value and type
/// are optional in a record definition. When instantiating a record,
/// if no value is given for a field, and no default is given,
/// then `undefined` is the default.
#[derive(Debug, Clone)]
pub struct RecordField {
    pub span: SourceSpan,
    pub id: NodeId,
    pub name: Ident,
    pub value: Option<Expr>,
    pub ty: Option<Type>,
}
impl PartialEq for RecordField {
    fn eq(&self, other: &Self) -> bool {
        (self.name == other.name) && (self.value == other.value) && (self.ty == other.ty)
    }
}

#[derive(Debug, Clone)]
pub struct Binary {
    pub span: SourceSpan,
    pub id: NodeId,
    pub elements: Vec<BinaryElement>,
}
impl PartialEq for Binary {
    fn eq(&self, other: &Self) -> bool {
        self.elements == other.elements
    }
}

/// Used to represent a specific segment in a binary constructor, to
/// produce a binary, all segments must be evaluated, and then assembled
#[derive(Debug, Clone)]
pub struct BinaryElement {
    pub span: SourceSpan,
    pub id: NodeId,
    pub bit_expr: Expr,
    pub bit_size: Option<Expr>,
    pub bit_type: Option<Vec<BitType>>,
}
impl PartialEq for BinaryElement {
    fn eq(&self, other: &Self) -> bool {
        (self.bit_expr == other.bit_expr)
            && (self.bit_size == other.bit_size)
            && (self.bit_type == other.bit_type)
    }
}

/// A bit type can come in the form `Type` or `Type:Size`
#[derive(Debug, Clone)]
pub enum BitType {
    Name(SourceSpan, NodeId, Ident),
    Sized(SourceSpan, NodeId, Ident, i64),
}
impl BitType {
    pub fn span(&self) -> SourceSpan {
        match self {
            BitType::Name(span, _, _) => *span,
            BitType::Sized(span, _, _, _) => *span,
        }
    }
}
impl PartialEq for BitType {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (&BitType::Name(_, _, ref x1), &BitType::Name(_, _, ref y1)) => x1 == y1,
            (&BitType::Sized(_, _, ref x1, ref x2), &BitType::Sized(_, _, ref y1, ref y2)) => {
                (x1 == y1) && (x2 == y2)
            }
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ListComprehension {
    pub span: SourceSpan,
    pub id: NodeId,
    pub body: Box<Expr>,
    pub qualifiers: Vec<Expr>,
}
impl PartialEq for ListComprehension {
    fn eq(&self, other: &Self) -> bool {
        self.body == other.body && self.qualifiers == other.qualifiers
    }
}

#[derive(Debug, Clone)]
pub struct BinaryComprehension {
    pub span: SourceSpan,
    pub id: NodeId,
    pub body: Box<Expr>,
    pub qualifiers: Vec<Expr>,
}
impl PartialEq for BinaryComprehension {
    fn eq(&self, other: &Self) -> bool {
        self.body == other.body && self.qualifiers == other.qualifiers
    }
}

// A generator of the form `LHS <- RHS`
#[derive(Debug, Clone)]
pub struct Generator {
    pub span: SourceSpan,
    pub id: NodeId,
    pub pattern: Box<Expr>,
    pub expr: Box<Expr>,
}
impl PartialEq for Generator {
    fn eq(&self, other: &Self) -> bool {
        self.pattern == other.pattern && self.expr == other.expr
    }
}

// A generator of the form `LHS <= RHS`
#[derive(Debug, Clone)]
pub struct BinaryGenerator {
    pub span: SourceSpan,
    pub id: NodeId,
    pub pattern: Box<Expr>,
    pub expr: Box<Expr>,
}
impl PartialEq for BinaryGenerator {
    fn eq(&self, other: &Self) -> bool {
        self.pattern == other.pattern && self.expr == other.expr
    }
}

// A sequence of expressions, e.g. begin expr1, .., exprN end
#[derive(Debug, Clone)]
pub struct Begin {
    pub span: SourceSpan,
    pub id: NodeId,
    pub body: Vec<Expr>,
}
impl PartialEq for Begin {
    fn eq(&self, other: &Self) -> bool {
        self.body == other.body
    }
}

// Function application, e.g. foo(expr1, .., exprN)
#[derive(Debug, Clone)]
pub struct Apply {
    pub span: SourceSpan,
    pub id: NodeId,
    pub callee: Box<Expr>,
    pub args: Vec<Expr>,
}
impl PartialEq for Apply {
    fn eq(&self, other: &Self) -> bool {
        self.callee == other.callee && self.args == other.args
    }
}

// Remote, e.g. Foo:Bar
#[derive(Debug, Clone)]
pub struct Remote {
    pub span: SourceSpan,
    pub id: NodeId,
    pub module: Box<Expr>,
    pub function: Box<Expr>,
}
impl PartialEq for Remote {
    fn eq(&self, other: &Self) -> bool {
        self.module == other.module && self.function == other.function
    }
}

#[derive(Debug, Clone)]
pub struct BinaryExpr {
    pub span: SourceSpan,
    pub id: NodeId,
    pub lhs: Box<Expr>,
    pub op: BinaryOp,
    pub rhs: Box<Expr>,
}
impl PartialEq for BinaryExpr {
    fn eq(&self, other: &Self) -> bool {
        self.op == other.op && self.lhs == other.lhs && self.rhs == other.rhs
    }
}

#[derive(Debug, Clone)]
pub struct UnaryExpr {
    pub span: SourceSpan,
    pub id: NodeId,
    pub op: UnaryOp,
    pub operand: Box<Expr>,
}
impl PartialEq for UnaryExpr {
    fn eq(&self, other: &Self) -> bool {
        self.op == other.op && self.operand == other.operand
    }
}

#[derive(Debug, Clone)]
pub struct Match {
    pub span: SourceSpan,
    pub id: NodeId,
    pub pattern: Box<Expr>,
    pub expr: Box<Expr>,
}
impl PartialEq for Match {
    fn eq(&self, other: &Self) -> bool {
        self.pattern == other.pattern && self.expr == other.expr
    }
}

#[derive(Debug, Clone)]
pub struct If {
    pub span: SourceSpan,
    pub id: NodeId,
    pub clauses: Vec<IfClause>,
}
impl PartialEq for If {
    fn eq(&self, other: &Self) -> bool {
        self.clauses == other.clauses
    }
}

/// Represents a single clause in an `if` expression
#[derive(Debug, Clone)]
pub struct IfClause {
    pub span: SourceSpan,
    pub id: NodeId,
    pub guards: Vec<Guard>,
    pub body: Vec<Expr>,
}
impl PartialEq for IfClause {
    fn eq(&self, other: &Self) -> bool {
        self.guards == other.guards && self.body == other.body
    }
}

#[derive(Debug, Clone)]
pub struct Catch {
    pub span: SourceSpan,
    pub id: NodeId,
    pub expr: Box<Expr>,
}
impl PartialEq for Catch {
    fn eq(&self, other: &Self) -> bool {
        self.expr == other.expr
    }
}

#[derive(Debug, Clone)]
pub struct Case {
    pub span: SourceSpan,
    pub id: NodeId,
    pub expr: Box<Expr>,
    pub clauses: Vec<Clause>,
}
impl PartialEq for Case {
    fn eq(&self, other: &Self) -> bool {
        self.expr == other.expr && self.clauses == other.clauses
    }
}

#[derive(Debug, Clone)]
pub struct Receive {
    pub span: SourceSpan,
    pub id: NodeId,
    pub clauses: Option<Vec<Clause>>,
    pub after: Option<After>,
}
impl PartialEq for Receive {
    fn eq(&self, other: &Self) -> bool {
        self.clauses == other.clauses && self.after == other.after
    }
}

#[derive(Debug, Clone)]
pub struct Try {
    pub span: SourceSpan,
    pub id: NodeId,
    pub exprs: Vec<Expr>,
    pub clauses: Option<Vec<Clause>>,
    pub catch_clauses: Option<Vec<TryClause>>,
    pub after: Option<Vec<Expr>>,
}
impl PartialEq for Try {
    fn eq(&self, other: &Self) -> bool {
        self.exprs == other.exprs
            && self.clauses == other.clauses
            && self.catch_clauses == other.catch_clauses
            && self.after == other.after
    }
}

/// Represents a single `catch` clause in a `try` expression
#[derive(Debug, Clone)]
pub struct TryClause {
    pub span: SourceSpan,
    pub id: NodeId,
    pub kind: Name,
    pub error: Expr,
    pub guard: Option<Vec<Guard>>,
    pub trace: Ident,
    pub body: Vec<Expr>,
}
impl PartialEq for TryClause {
    fn eq(&self, other: &Self) -> bool {
        self.kind == other.kind
            && self.error == other.error
            && self.guard == other.guard
            && self.trace == other.trace
            && self.body == other.body
    }
}

/// Represents the `after` clause of a `receive` expression
#[derive(Debug, Clone)]
pub struct After {
    pub span: SourceSpan,
    pub id: NodeId,
    pub timeout: Box<Expr>,
    pub body: Vec<Expr>,
}
impl PartialEq for After {
    fn eq(&self, other: &Self) -> bool {
        self.timeout == other.timeout && self.body == other.body
    }
}

/// Represents a single match clause in a `case`, `try`, or `receive` expression
#[derive(Debug, Clone)]
pub struct Clause {
    pub span: SourceSpan,
    pub id: NodeId,
    pub pattern: Expr,
    pub guard: Option<Vec<Guard>>,
    pub body: Vec<Expr>,
}
impl PartialEq for Clause {
    fn eq(&self, other: &Self) -> bool {
        self.pattern == other.pattern && self.guard == other.guard && self.body == other.body
    }
}
