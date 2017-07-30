pub use ::{ Variable, Atom };

#[derive(Debug, Clone)]
pub struct Annotated<I>(pub I, pub Vec<()>);
impl<I> Annotated<I> {
    fn empty(inner: I) -> Self {
        Annotated(inner, Vec::new())
    }
}

#[derive(Debug, Clone)]
pub struct Module {
    pub name: Atom,
    pub declarations: Vec<FunctionName>,
    pub attributes: Vec<(Atom, Constant)>,
    pub definitions: Vec<FunctionDefinition>,
}

#[derive(Debug, Clone, Hash, PartialEq, Eq)]
pub struct FunctionName {
    pub name: Atom,
    pub arity: u32,
}

#[derive(Debug, Clone)]
pub struct Integer {
    sign: bool,
    digits: String,
}
impl Integer {
    fn as_u32(&self) -> u32 {
        assert!(self.sign);
        self.digits.parse().unwrap()
    }
}

#[derive(Debug, Clone)]
pub enum AtomicLiteral {
    Integer(Integer),
    Float,
    Atom(Atom),
    Nil,
    Char(char),
    String(String),
}

#[derive(Debug, Clone)]
pub enum Constant {
    Atomic(AtomicLiteral),
    Tuple(Vec<Constant>),
    List(Vec<Constant>, Box<Constant>),
}

#[derive(Debug, Clone)]
pub struct FunctionDefinition {
    pub name: Annotated<FunctionName>,
    pub fun: Annotated<Function>,
}

#[derive(Debug, Clone)]
pub enum SingleExpression {
    FunctionName(FunctionName),
    AtomicLiteral(AtomicLiteral),
    Variable(Variable),
    Tuple(Vec<Expression>),
    List { head: Vec<Expression>, tail: Box<Expression> },
    Map(Vec<(Expression, Expression)>),
    Let { vars: Vec<Annotated<Variable>>, val: Box<Expression>, body: Box<Expression> },
    InterModuleCall { module: Box<Expression>, name: Box<Expression>, args: Vec<Expression> },
    Catch(Box<Expression>),
    Case { val: Box<Expression>, clauses: Vec<Annotated<CaseClause>> },
    PrimOpCall(PrimOpCall),
    Do(Box<Expression>, Box<Expression>),
    ApplyCall { fun: Box<Expression>, args: Vec<Expression> },
    Try { body: Box<Expression>, then_vars: Vec<Annotated<Variable>>, then: Box<Expression>,
          catch_vars: Vec<Annotated<Variable>>, catch: Box<Expression> },
    Receive { clauses: Vec<Annotated<CaseClause>>, timeout_time: Box<Expression>,
              timeout_body: Box<Expression> },
    Fun(Box<Function>),
    LetRec { funs: Vec<(FunctionName, Function)>, body: Box<Expression> },
}

#[derive(Debug, Clone)]
pub struct CaseClause {
    pub patterns: Vec<Annotated<Pattern>>,
    pub guard: Expression,
    pub body: Expression,
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Variable(Variable),
    Bind(Variable, Box<Annotated<Pattern>>),
    Atomic(AtomicLiteral),
    Tuple(Vec<Annotated<Pattern>>),
    List(Vec<Annotated<Pattern>>, Box<Annotated<Pattern>>),
}
impl Pattern {
    fn nil() -> Annotated<Pattern> {
        Annotated::empty(Pattern::Atomic(AtomicLiteral::Nil))
    }
}

pub type Expression = Annotated<Vec<Annotated<SingleExpression>>>;
impl Expression {
    fn nil() -> Self {
        Annotated::empty(vec![Annotated::empty(SingleExpression::AtomicLiteral(AtomicLiteral::Nil))])
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub vars: Vec<Annotated<Variable>>,
    pub body: Expression,
}

#[derive(Debug, Clone)]
pub struct PrimOpCall {
    pub name: Atom,
    pub args: Vec<Expression>,
}

pub use self::core_parser::annotatedModule as annotated_module;
mod core_parser {
    include!(concat!(env!("OUT_DIR"), "/grammar.rs"));
}
