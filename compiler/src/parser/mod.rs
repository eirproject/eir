pub use ::{ Variable, Atom };
use ::ir::AVariable;
use ::eir::FunctionIdent;

mod grammar;
mod lex;

pub fn parse<'input>(text: &'input str) -> Result<Annotated<Module>,
        ::lalrpop_util::ParseError<usize, lex::Tok<'input>, ()>> {

    let tokenizer = lex::Tokenizer::new(text);
    let parser = grammar::AnnotatedModuleParser::new();
    parser.parse(text, tokenizer)
}

#[test]
fn parse_otp_compiler_compile() {
    use ::std::io::Read;

    let mut f = ::std::fs::File::open("../test_data/compile.core").unwrap();
    let mut s = String::new();
    f.read_to_string(&mut s);

    parse(&s).unwrap();
}

#[derive(Debug, Copy, Clone)]
pub enum MapExactAssoc {
    Exact,
    Assoc,
}

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
    pub arity: usize,
}
impl FunctionName {
    pub fn to_eir(&self, module: Atom) -> FunctionIdent {
        FunctionIdent {
            module: module,
            name: self.name.clone(),
            arity: self.arity,
            lambda: None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Integer {
    pub sign: bool,
    pub digits: String,
}
impl Integer {
    fn as_u32(&self) -> u32 {
        assert!(self.sign);
        self.digits.parse().unwrap()
    }
    fn as_usize(&self) -> usize {
        assert!(self.sign);
        self.digits.parse().unwrap()
    }
}

use eir::AtomicTerm;
type AtomicLiteral = AtomicTerm;

//#[derive(Debug, Clone, PartialEq, Eq, Hash)]
//pub enum AtomicLiteral {
//    Integer(Integer),
//    Float,
//    Atom(Atom),
//    Nil,
//    Char(char),
//    String(String),
//}
//impl Display for AtomicLiteral {
//    fn fmt(&self, f: &mut Formatter) -> Result<(), ::std::fmt::Error> {
//        match self {
//            &AtomicLiteral::Integer(ref int) => write!(f, "{}{}", int.sign, int.digits),
//            _ => write!(f, "unimpl"),
//        }
//    }
//}

#[derive(Debug, Clone)]
pub enum Constant {
    Atomic(AtomicLiteral),
    Tuple(Vec<Constant>),
    List(Vec<Constant>, Box<Constant>),
}
impl Constant {
    pub fn to_eir(&self) -> ::eir::ConstantTerm {
        match self {
            Constant::Atomic(atomic) => ::eir::ConstantTerm::Atomic(atomic.clone()),
            Constant::List(head, tail) =>
                ::eir::ConstantTerm::List(
                    head.iter().map(|c| c.to_eir()).collect(),
                    Box::new(tail.to_eir()),
                ),
            _ => unimplemented!(),
        }
    }
}
//impl Display for Constant {
//    fn fmt(&self, f: &mut Formatter) -> Result<(), ::std::fmt::Error> {
//        match self {
//            &Constant::Atomic(ref lit) => write!(f, "{}", lit),
//            _ => write!(f, "unimpl"),
//        }
//    }
//}

#[derive(Debug, Clone)]
pub struct FunctionDefinition {
    pub name: Annotated<FunctionName>,
    pub fun: Annotated<Function>,
}

#[derive(Debug, Clone)]
pub enum SingleExpression {
    // Env reading
    FunctionName(FunctionName),
    ExternalFunctionName { module: Atom, name: FunctionName },
    Variable(Variable),

    // Control flow
    Let { vars: Vec<Annotated<Variable>>, val: Box<Expression>, body: Box<Expression> },
    Catch(Box<Expression>),
    Case { val: Box<Expression>, clauses: Vec<Annotated<CaseClause>> },
    Do(Box<Expression>, Box<Expression>),
    Try { body: Box<Expression>, then_vars: Vec<Annotated<Variable>>, then: Box<Expression>,
          catch_vars: Vec<Annotated<Variable>>, catch: Box<Expression> },
    Receive { clauses: Vec<Annotated<CaseClause>>, timeout_time: Box<Expression>,
              timeout_body: Box<Expression> },

    // Calling
    PrimOpCall(PrimOpCall),
    ApplyCall { fun: Box<Expression>, args: Vec<Expression> },
    InterModuleCall { module: Box<Expression>, name: Box<Expression>, args: Vec<Expression> },

    // Lambda creation
    Fun(Box<Function>),
    LetRec { funs: Vec<(FunctionName, Function)>, body: Box<Expression> },

    // Term constructors
    AtomicLiteral(AtomicLiteral),
    Tuple(Vec<Expression>),
    List { head: Vec<Expression>, tail: Box<Expression> },
    Map(Vec<Annotated<(Expression, MapExactAssoc, Expression)>>, Option<Expression>),
    Binary(Vec<(Expression, Vec<Expression>)>),
}

#[derive(Debug, Clone)]
pub struct CaseClause {
    pub patterns: Vec<Annotated<Pattern>>,
    pub guard: Expression,
    pub body: Expression,
}

#[derive(Debug, Clone)]
pub enum Pattern {
    Wildcard,
    BindVar(Annotated<Variable>, Box<Annotated<Pattern>>),
    Atomic(AtomicLiteral),
    Binary(Vec<(Annotated<Pattern>, Vec<Annotated<SingleExpression>>)>),
    Tuple(Vec<Annotated<Pattern>>),
    List(Vec<Annotated<Pattern>>, Box<Annotated<Pattern>>),
    Map(Vec<Annotated<(Annotated<SingleExpression>, Annotated<Pattern>)>>),
}
impl Pattern {
    fn nil() -> Annotated<Pattern> {
        Annotated::empty(Pattern::Atomic(AtomicTerm::Nil))
    }
}

pub type Expression = Annotated<Vec<Annotated<SingleExpression>>>;
impl Expression {
    fn nil() -> Self {
        Annotated::empty(vec![Annotated::empty(SingleExpression::AtomicLiteral(
            AtomicTerm::Nil))])
    }
}

#[derive(Debug, Clone)]
pub struct Function {
    pub vars: Vec<Annotated<Variable>>,
    pub body: Expression,
}

#[derive(Debug, Clone)]
pub struct PrimOpCall {
    pub name: Annotated<Atom>,
    pub args: Vec<Expression>,
}

