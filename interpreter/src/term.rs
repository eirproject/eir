use ::std::cell::RefCell;
use ::std::rc::Rc;
use std::cmp::Ord;

use eir::Atom;
use eir::LambdaEnvIdx;
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
    Map,
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
    Map(Vec<(Term, Term)>),
    Pid(Pid),
    Reference(Reference),
    Binary(Vec<u8>),
    BoundLambda {
        module: Atom,
        fun_name: Atom,
        arity: usize,
        lambda: (LambdaEnvIdx, usize),
        bound_env: BoundLambdaEnv,
    },
    CapturedFunction {
        module: Atom,
        fun_name: Atom,
        arity: usize,
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
            Term::Map(_) => TermType::Map,
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

    pub fn as_usize(&self) -> Option<usize> {
        if let Term::Integer(ref bigint) = self {
            Some(bigint.to_usize().unwrap())
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

    pub fn to_doc(&self) -> ::pretty::Doc<::pretty::BoxDoc> {
        use ::pretty::Doc;
        match self {
            Term::Nil => Doc::text("[]"),
            Term::Integer(int) => Doc::text(int.to_string()),
            Term::Float(num) => Doc::text(num.to_string()),
            Term::Atom(atom) => Doc::text(atom.to_string()),
            Term::Tuple(items) => {
                let docs: Vec<_> = items.iter().map(|i| i.to_doc()).collect();
                Doc::text("{")
                    .append(Doc::intersperse(docs, Doc::text(",")))
                    .append(Doc::text("}"))
            },
            Term::List(head, tail) if head.len() == 0 => tail.to_doc(),
            Term::List(head, tail) => {
                let head_docs: Vec<_> = head.iter().map(|i| i.to_doc()).collect();
                let tail_doc = tail.to_doc();
                Doc::text("[")
                    .append(Doc::intersperse(head_docs, Doc::text(",")))
                    .append(Doc::text("|"))
                    .append(tail_doc)
                    .append(Doc::text("]"))
            },
            Term::Map(entries) => {
                let entries_doc: Vec<_> = entries.iter()
                    .map(|(k, v)| {
                        Doc::group(
                            k.to_doc()
                                .append(Doc::text("=>"))
                                .append(v.to_doc())
                        )
                    }).collect();
                Doc::text("%{")
                    .append(Doc::intersperse(entries_doc, Doc::text(",")))
                    .append(Doc::text("}"))
            },
            Term::Pid(pid) => Doc::text(format!("Pid<{}>", pid.0)),
            Term::Reference(refe) => Doc::text(format!("Reference<{}>", refe.0)),
            Term::Binary(bin) => {
                if let Ok(utf) = std::str::from_utf8(&bin) {
                    Doc::text("\"")
                        .append(Doc::text(utf))
                        .append(Doc::text("\""))
                } else {
                    let items: Vec<_> = bin.iter().map(|v| Doc::text(v.to_string()))
                        .collect();
                    Doc::text("<")
                        .append(Doc::intersperse(items, Doc::text(",")))
                        .append(Doc::text(">"))
                }
            },
            Term::BoundLambda { module, fun_name, arity, lambda, .. } => {
                Doc::text(format!("Bound<{}:{}@{}.{}/{}>", module, fun_name,
                                  lambda.0, lambda.1, arity))
            },
            Term::CapturedFunction { module, fun_name, arity } => {
                Doc::text(format!("Captured<{}:{}/{}>", module, fun_name, arity))
            },
            Term::LambdaEnv(_env) =>
                Doc::text("BoundLambdaEnv"),
            Term::CaseContext(_case) =>
                Doc::text("CaseContext"),
            Term::ReceiveContext(_recv) =>
                Doc::text("ReceiveContext"),
            Term::ValueList(items) => {
                let docs: Vec<_> = items.iter().map(|i| i.to_doc()).collect();
                Doc::text("<")
                    .append(Doc::intersperse(docs, Doc::text(",")))
                    .append(Doc::text(">"))
            }
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
            (Term::List(h1, t1), Term::Nil) if h1.len() == 0 =>
                if let Term::Nil = **t1 { true } else { false },
            (Term::Nil, Term::List(h2, t2)) if h2.len() == 0 =>
                if let Term::Nil = **t2 { true } else { false },
            (Term::List(h1, t1), Term::List(h2, t2)) => {
                let head_ok = h1.iter().zip(h2.iter())
                    .all(|(v1, v2)| v1.erl_eq(v2));
                if !head_ok { return false; }
                if h1.len() == h2.len() {
                    t1.erl_eq(t2)
                } else if h1.len() > h2.len() {
                    let h1_new_head: Vec<_> = h1.iter().skip(h2.len())
                        .cloned().collect();
                    let h1_new = Term::List(h1_new_head, t1.clone());
                    h1_new.erl_eq(t2)
                } else {
                    let h2_new_head: Vec<_> = h2.iter().skip(h1.len())
                        .cloned().collect();
                    let h2_new = Term::List(h2_new_head, t2.clone());
                    t1.erl_eq(&h2_new)
                }
            }
            (Term::Nil, _) => false,
            (_, Term::Nil) => false,

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
                ::trace::warning_args(
                    "WARNING: ErlEq might be unimplemented".to_string(),
                    || {
                        let mut event_args = std::collections::HashMap::new();
                        event_args.insert(
                            "lhs".to_string(),
                            ::serde_json::Value::String(format!("{:?}", self))
                        );
                        event_args.insert(
                            "rhs".to_string(),
                            ::serde_json::Value::String(format!("{:?}", other))
                        );
                        event_args
                    }
                );
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
            (Term::Atom(_), _) => false,
            (_, Term::Atom(_)) => false,

            (Term::Integer(lhs), Term::Integer(rhs)) => lhs == rhs,
            (Term::Integer(_), _) => false,
            (_, Term::Integer(_)) => false,

            (Term::Nil, Term::Nil) => true,
            (Term::List(h1, t1), Term::Nil) if h1.len() == 0 =>
                if let Term::Nil = **t1 { true } else { false },
            (Term::Nil, Term::List(h2, t2)) if h2.len() == 0 =>
                if let Term::Nil = **t2 { true } else { false },
            (Term::List(h1, t1), Term::List(h2, t2)) => {
                let head_ok = h1.iter().zip(h2.iter())
                    .all(|(v1, v2)| v1.erl_exact_eq(v2));
                if !head_ok { return false; }
                if h1.len() == h2.len() {
                    t1.erl_exact_eq(t2)
                } else if h1.len() > h2.len() {
                    let h1_new_head: Vec<_> = h1.iter().skip(h2.len())
                        .cloned().collect();
                    let h1_new = Term::List(h1_new_head, t1.clone());
                    h1_new.erl_exact_eq(t2)
                } else {
                    let h2_new_head: Vec<_> = h2.iter().skip(h1.len())
                        .cloned().collect();
                    let h2_new = Term::List(h2_new_head, t2.clone());
                    t1.erl_exact_eq(&h2_new)
                }
            }
            (Term::Nil, _) => false,
            (_, Term::Nil) => false,

            _ => {
                ::trace::warning_args(
                    "WARNING: ErlExactEq might be unimplemented".to_string(),
                    || {
                        let mut event_args = std::collections::HashMap::new();
                        event_args.insert(
                            "lhs".to_string(),
                            ::serde_json::Value::String(format!("{:?}", self))
                        );
                        event_args.insert(
                            "rhs".to_string(),
                            ::serde_json::Value::String(format!("{:?}", other))
                        );
                        event_args
                    }
                );
                println!("WARNING: ErlExactEq might be unimplemented");
                println!("  {:?} =:= {:?}", self, other);
                false
            }
        }
    }
}

impl ErlOrd for Term {
    fn erl_ord(&self, other: &Term) -> ::std::cmp::Ordering {
        match (self, other) {
            (Term::Integer(val1), Term::Integer(val2)) =>
                val1.cmp(val2),
            (_, _) => unimplemented!(),
        }
    }
}
