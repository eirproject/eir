use ::std::cell::RefCell;
use ::std::rc::Rc;

use core_erlang_compiler::intern::Atom;
use core_erlang_compiler::ir::hir::scope_tracker::LambdaEnvIdx;
use ::pattern::CaseContext;

use ::num_bigint::BigInt;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum TermType {
    Nil,
    Integer,
    Float,
    Atom,
    Tuple,
    BoundLambda,
    CapturedFunction,

    // Internal
    LambdaEnv,
    CaseContext,
}

#[derive(Debug, Clone)]
pub struct BoundLambdaEnv {
    /// This field is not runtime information, debug only.
    pub env: LambdaEnvIdx,
    pub vars: Vec<Term>,
}

#[derive(Debug, Clone)]
pub enum Term {
    Nil,
    Integer(BigInt),
    Float(f64),
    Atom(Atom),
    Tuple(Vec<Term>),
    BoundLambda {
        module: Atom,
        fun_name: Atom,
        arity: u32,
        lambda: u32,
        bound_env: BoundLambdaEnv,
    },
    CapturedFunction {
        module: Atom,
        fun_name: Atom,
        arity: u32,
    },

    // Internal
    LambdaEnv(BoundLambdaEnv),
    CaseContext(Rc<RefCell<CaseContext>>),
}
impl Term {

    pub fn new_i64(num: i64) -> Self {
        Term::Integer(num.into())
    }

    pub fn new_atom(string: &str) -> Self {
        Term::Atom(Atom::from_str(string))
    }

    pub fn atom_str<'a>(&'a self) -> &'a str {
        if let Term::Atom(ref atom) = *self {
            atom.as_str()
        } else {
            panic!();
        }
    }

    pub fn get_type(&self) -> TermType {
        match self {
            Term::Nil => TermType::Nil,
            Term::Integer(_) => TermType::Integer,
            Term::Float(_) => TermType::Float,
            Term::Atom(_) => TermType::Atom,
            Term::Tuple(_) => TermType::Tuple,
            Term::LambdaEnv { .. } => TermType::LambdaEnv,
            Term::BoundLambda { .. } => TermType::BoundLambda,
            Term::CapturedFunction { .. } => TermType::CapturedFunction,
            Term::CaseContext {.. } => TermType::CaseContext,
        }
    }

}


pub trait ErlEq<Rhs = Self> {
    fn erl_eq(&self, other: &Rhs) -> bool;
}

pub trait ErlExactEq<Rhs = Self> {
    fn erl_exact_eq(&self, other: &Rhs) -> bool;
}

pub trait ErlOrd<Rhs = Self> {
    fn erl_ord(&self, other: &Rhs) -> ::std::cmp::Ordering;
}

impl ErlEq for f64 {
    fn erl_eq(&self, other: &f64) -> bool {
        (*self) == (*other)
    }
}

impl ErlEq for Term {
    fn erl_eq(&self, other: &Term) -> bool {
        match (self, other) {
            (Term::Nil, Term::Nil) => true,
            (Term::Integer(ref i1), Term::Integer(ref i2)) => i1 == i2,
            (Term::Float(ref f1), Term::Float(ref f2)) => f1 == f2,
            (Term::Integer(_), Term::Float(_)) => unimplemented!(),
            (Term::Float(_), Term::Integer(_)) => unimplemented!(),
            (Term::Atom(ref a1), Term::Atom(ref a2)) => a1 == a2,
            (Term::Tuple(ref v1), Term::Tuple(ref v2)) =>
                v1.iter().zip(v2).all(|(e1, e2)| e1.erl_eq(e2)),
            (Term::CapturedFunction {
                module: ref mod1, fun_name: ref fun_name1,
                arity: ref arity1 },
             Term::CapturedFunction {
                 module: ref mod2, fun_name: ref fun_name2,
                 arity: ref arity2 }) =>
                mod1 == mod2 && fun_name1 == fun_name2 && arity1 == arity2,
            (Term::LambdaEnv { .. }, _) => unreachable!(), // There should never happen
            (_, Term::LambdaEnv { .. }) => unreachable!(),
            (Term::BoundLambda { .. }, _) => unreachable!(),
            (_, Term::BoundLambda { .. }) => unreachable!(),
            _ => false,
        }
    }
}
