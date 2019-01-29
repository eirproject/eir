use ::std::cell::RefCell;
use ::std::rc::Rc;

use core_erlang_compiler::intern::Atom;
use core_erlang_compiler::ir::hir::scope_tracker::LambdaEnvIdx;
use ::pattern::CaseContext;
use ::receive::ReceiveContext;

use ::num_bigint::BigInt;
use ::num_traits::cast::ToPrimitive;

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct Pid(pub usize);

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
pub struct Reference(pub usize);

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum TermType {
    Nil,
    Integer,
    Float,
    Atom,
    Tuple,
    List,
    Pid,
    Reference,
    Binary,

    BoundLambda,
    CapturedFunction,

    // Internal
    LambdaEnv,
    CaseContext,
    ReceiveContext,
    ValueList,
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
    List(Vec<Term>, Box<Term>),
    Pid(Pid),
    Reference(Reference),
    Binary(Vec<u8>),
    BoundLambda {
        module: Atom,
        fun_name: Atom,
        arity: u32,
        lambda: (LambdaEnvIdx, usize),
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
    ReceiveContext(Rc<RefCell<ReceiveContext>>),
    ValueList(Vec<Term>),
}
impl Term {

    pub fn new_i64(num: i64) -> Self {
        Term::Integer(num.into())
    }

    pub fn new_atom(string: &str) -> Self {
        Term::Atom(Atom::from_str(string))
    }

    pub fn new_bool(val: bool) -> Self {
        if val {
            Term::new_atom("true")
        } else {
            Term::new_atom("false")
        }
    }

    fn extend_erl_string(&self, buf: &mut String) -> bool {
        match self {
            Term::Integer(val) => {
                buf.push(val.to_u8().unwrap() as char);
                true
            }
            Term::List(head, tail) => {
                for item in head {
                    if !item.extend_erl_string(buf) {
                        return false;
                    }
                }
                return tail.extend_erl_string(buf);
            }
            Term::Nil => true,
            _ => false,
        }
    }

    pub fn get_erl_string(&self) -> Option<String> {
        let mut buf = String::new();
        if self.extend_erl_string(&mut buf) {
            Some(buf)
        } else {
            None
        }
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
            Term::Binary(_) => TermType::Binary,
            Term::Nil => TermType::Nil,
            Term::Pid(_) => TermType::Pid,
            Term::Reference(_) => TermType::Reference,
            Term::Integer(_) => TermType::Integer,
            Term::Float(_) => TermType::Float,
            Term::Atom(_) => TermType::Atom,
            Term::Tuple(_) => TermType::Tuple,
            Term::List(_, _) => TermType::List,
            Term::LambdaEnv { .. } => TermType::LambdaEnv,
            Term::BoundLambda { .. } => TermType::BoundLambda,
            Term::CapturedFunction { .. } => TermType::CapturedFunction,
            Term::CaseContext {.. } => TermType::CaseContext,
            Term::ReceiveContext(_) => TermType::ReceiveContext,
            Term::ValueList(_) => TermType::ValueList,
        }
    }

    pub fn as_boolean(&self) -> Option<bool> {
        if let Term::Atom(ref val) = self {
            let is_truthy = *val == Atom::from("true");
            let is_falsey = *val == Atom::from("false");
            if is_truthy ^ is_falsey {
                Some(is_truthy)
            } else {
                None
            }
        } else {
            None
        }
    }

    pub fn as_i64(&self) -> Option<i64> {
        if let Term::Integer(ref bigint) = self {
            Some(bigint.to_i64().unwrap())
        } else {
            None
        }
    }

    pub fn as_atom(&self) -> Option<Atom> {
        if let Term::Atom(ref atom) = self {
            Some(atom.clone())
        } else {
            None
        }
    }

    fn join_list(&self, list: &mut Vec<Term>) -> Term {
        match self {
            Term::List(head, tail) => {
                list.extend(head.iter().cloned());
                tail.join_list(list)
            }
            _ => self.clone()
        }
    }

    pub fn as_inproper_list(&self) -> (Vec<Term>, Term) {
        let mut list = Vec::new();
        let tail = self.join_list(&mut list);
        (list, tail)
    }

    pub fn as_list(&self) -> Option<Vec<Term>> {
        let (list, tail) = self.as_inproper_list();
        if let Term::Nil = tail {
            Some(list)
        } else {
            None
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

            (Term::LambdaEnv { .. }, _) => unreachable!(), // These should never happen
            (_, Term::LambdaEnv { .. }) => unreachable!(),
            (Term::BoundLambda { .. }, _) => unreachable!(),
            (_, Term::BoundLambda { .. }) => unreachable!(),
            (Term::ValueList(_), _) => unimplemented!(),
            (_, Term::ValueList(_)) => unimplemented!(),

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
            _ => {
                println!("WARNING: ErlEq might be unimplemented");
                println!("  {:?} == {:?}", self, other);
                false
            }
        }
    }
}

impl ErlExactEq for Term {
    fn erl_exact_eq(&self, other: &Term) -> bool {
        match (self, other) {
            (Term::LambdaEnv { .. }, _) => unreachable!(), // These should never happen
            (_, Term::LambdaEnv { .. }) => unreachable!(),
            (Term::BoundLambda { .. }, _) => unreachable!(),
            (_, Term::BoundLambda { .. }) => unreachable!(),
            (Term::ValueList(_), _) => unimplemented!(),
            (_, Term::ValueList(_)) => unimplemented!(),

            (Term::Atom(ref a1), Term::Atom(ref a2)) => a1 == a2,

            _ => {
                println!("WARNING: ErlExactEq might be unimplemented");
                println!("  {:?} =:= {:?}", self, other);
                false
            }
        }
    }
}
