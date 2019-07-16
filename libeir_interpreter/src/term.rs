use ::std::cell::RefCell;
use ::std::rc::Rc;
use std::cmp::Ord;

use libeir_intern::{ Symbol, LocalInternedString };

use libeir_ir::{ FunctionIdent, Block };

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
    ListCell,
    Map,
    Pid,
    Reference,
    Binary,

    BoundLambda,
    CapturedFunction,

    // Internal
    ValueList,
    ReturnOk,
    ReturnThrow,
}

#[derive(Debug, Clone)]
pub enum Term {
    Nil,
    Integer(BigInt),
    Float(f64),
    Atom(Symbol),
    Tuple(Vec<Rc<Term>>),
    ListCell(Rc<Term>, Rc<Term>),
    Map(Vec<(Rc<Term>, Rc<Term>)>),
    Pid(Pid),
    Reference(Reference),
    Binary(Vec<u8>),
    BoundLambda {
        ident: FunctionIdent,
        block: Block,
        environment: Vec<Rc<Term>>,
    },
    CapturedFunction {
        ident: FunctionIdent,
    },

    // Internal
    ValueList(Vec<Rc<Term>>),
    ReturnOk,
    ReturnThrow,
}

impl From<i64> for Term {
    fn from(num: i64) -> Self {
        Term::Integer(num.into())
    }
}

pub enum ListIteratorItem {
    Elem(Rc<Term>),
    Tail(Rc<Term>),
}

pub struct ListTermIterator {
    term: Option<Rc<Term>>,
}
impl Iterator for ListTermIterator {
    type Item = ListIteratorItem;
    fn next(&mut self) -> Option<ListIteratorItem> {
        if let Some(term) = self.term.take() {
            match &*term {
                Term::ListCell(head, tail) => {
                    self.term = Some(tail.clone());
                    Some(ListIteratorItem::Elem(head.clone()))
                }
                _ => {
                    self.term = None;
                    Some(ListIteratorItem::Tail(term.clone()))
                }
            }
        } else {
            None
        }
    }
}

impl Term {

    pub fn new_i64(num: i64) -> Self {
        Term::Integer(num.into())
    }

    pub fn new_atom(string: &str) -> Self {
        Term::Atom(Symbol::intern(string))
    }

    pub fn new_bool(val: bool) -> Self {
        if val {
            Term::new_atom("true")
        } else {
            Term::new_atom("false")
        }
    }

    pub fn slice_to_list(head: &[Rc<Term>], tail: Rc<Term>) -> Rc<Term> {
        let mut acc = tail;
        for term in head.iter().rev() {
            acc = Term::ListCell(term.clone(), acc).into();
        }
        acc
    }

    fn extend_erl_string(&self, buf: &mut String) -> bool {
        match self {
            Term::Integer(val) => {
                buf.push(val.to_u8().unwrap() as char);
                true
            }
            Term::ListCell(head, tail) => {
                unimplemented!()
                //for item in head {
                //    if !item.extend_erl_string(buf) {
                //        return false;
                //    }
                //}
                //return tail.extend_erl_string(buf);
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

    pub fn atom_str<'a>(&'a self) -> LocalInternedString {
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
            Term::ListCell(_, _) => TermType::ListCell,
            Term::Map(_) => TermType::Map,
            Term::BoundLambda { .. } => TermType::BoundLambda,
            Term::CapturedFunction { .. } => TermType::CapturedFunction,
            Term::ValueList(_) => TermType::ValueList,
            Term::ReturnOk => TermType::ReturnOk,
            Term::ReturnThrow => TermType::ReturnThrow,
        }
    }

    pub fn as_boolean(&self) -> Option<bool> {
        if let Term::Atom(ref val) = self {
            let is_truthy = *val == Symbol::intern("true");
            let is_falsey = *val == Symbol::intern("false");
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

    pub fn as_atom(&self) -> Option<Symbol> {
        if let Term::Atom(ref atom) = self {
            Some(atom.clone())
        } else {
            None
        }
    }

    pub fn list_iter(term: &Rc<Term>) -> ListTermIterator {
        ListTermIterator {
            term: Some(term.clone()),
        }
    }

    //fn join_list(&self, list: &mut Vec<Rc<Term>>) -> Term {
    //    match self {
    //        Term::ListCell(head, tail) => {
    //            list.extend(head.iter().cloned());
    //            tail.join_list(list)
    //        }
    //        _ => self.clone()
    //    }
    //}

    pub fn as_inproper_list(term: &Rc<Term>) -> (Vec<Rc<Term>>, Rc<Term>) {
        let mut list = Vec::new();
        for item in Term::list_iter(term) {
            match item {
                ListIteratorItem::Elem(elem) => list.push(elem),
                ListIteratorItem::Tail(tail) => return (list, tail),
            }
        }
        unreachable!()
    }

    pub fn as_list(term: &Rc<Term>) -> Option<Vec<Rc<Term>>> {
        let (list, tail) = Term::as_inproper_list(term);
        if let Term::Nil = &*tail {
            Some(list)
        } else {
            None
        }
    }

    pub fn to_doc(term: Rc<Term>) -> ::pretty::Doc<'static, ::pretty::BoxDoc<'static>> {
        use ::pretty::{ Doc };
        match &*term {
            Term::Nil => Doc::text("[]"),
            Term::Integer(int) => Doc::text(int.to_string()),
            Term::Float(num) => Doc::text(num.to_string()),
            Term::Atom(atom) => Doc::text(atom.to_string()),
            Term::Tuple(items) => {
                let docs: Vec<_> = items.iter().map(|i| Term::to_doc(i.clone())).collect();
                Doc::text("{")
                    .append(Doc::intersperse(docs, Doc::text(",")))
                    .append(Doc::text("}"))
            },
            Term::ListCell(_, _) => {
                let (head, tail) = Term::as_inproper_list(&term);
                let head_docs: Vec<_> = head.iter().map(|i| Term::to_doc(i.clone())).collect();
                let tail_doc = Term::to_doc(tail);
                Doc::text("[")
                    .append(Doc::intersperse(head_docs, Doc::text(",")))
                    .append(Doc::text("|"))
                    .append(tail_doc)
                    .append(Doc::text("]"))
            },
            //Term::ListCell(head, tail) if head.len() == 0 => tail.to_doc(),
            //Term::ListCell(head, tail) => {
            //    let head_docs: Vec<_> = head.iter().map(|i| i.to_doc()).collect();
            //    let tail_doc = tail.to_doc();
            //    Doc::text("[")
            //        .append(Doc::intersperse(head_docs, Doc::text(",")))
            //        .append(Doc::text("|"))
            //        .append(tail_doc)
            //        .append(Doc::text("]"))
            //},
            Term::Map(entries) => {
                let entries_doc: Vec<_> = entries.iter()
                    .map(|(k, v)| {
                        Doc::group(
                            Term::to_doc(k.clone())
                                .append(Doc::text("=>"))
                                .append(Term::to_doc(v.clone()))
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
                        .append(Doc::text(utf.to_string()))
                        .append(Doc::text("\""))
                } else {
                    let items: Vec<_> = bin.iter().map(|v| Doc::text(v.to_string()))
                        .collect();
                    Doc::text("<")
                        .append(Doc::intersperse(items, Doc::text(",")))
                        .append(Doc::text(">"))
                }
            },
            Term::BoundLambda { ident, block, .. } => {
                Doc::text(format!("Bound<{}:{}/{}@{}>", ident.module.name, ident.name.name,
                                  ident.arity, block))
            },
            Term::CapturedFunction { ident } => {
                Doc::text(format!("Captured<{}:{}/{}>", ident.module.name, ident.name.name, ident.arity))
            },
            Term::ValueList(items) => {
                let docs: Vec<_> = items.iter().map(|i| Term::to_doc(i.clone())).collect();
                Doc::text("<")
                    .append(Doc::intersperse(docs, Doc::text(",")))
                    .append(Doc::text(">"))
            }
            Term::ReturnOk => {
                Doc::text(format!("ReturnOk"))
            }
            Term::ReturnThrow => {
                Doc::text(format!("ReturnThrow"))
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

            (Term::BoundLambda { .. }, _) => unreachable!(),
            (_, Term::BoundLambda { .. }) => unreachable!(),
            (Term::ValueList(_), _) => unimplemented!(),
            (_, Term::ValueList(_)) => unimplemented!(),

            (Term::Nil, Term::Nil) => true,
            (Term::ListCell(h1, t1), Term::ListCell(h2, t2)) => {
                h1.erl_eq(h2) && t1.erl_eq(t2)
            }
            //(Term::List(h1, t1), Term::Nil) if h1.len() == 0 =>
            //    if let Term::Nil = **t1 { true } else { false },
            //(Term::Nil, Term::List(h2, t2)) if h2.len() == 0 =>
            //    if let Term::Nil = **t2 { true } else { false },
            //(Term::List(h1, t1), Term::List(h2, t2)) => {
            //    let head_ok = h1.iter().zip(h2.iter())
            //        .all(|(v1, v2)| v1.erl_eq(v2));
            //    if !head_ok { return false; }
            //    if h1.len() == h2.len() {
            //        t1.erl_eq(t2)
            //    } else if h1.len() > h2.len() {
            //        let h1_new_head: Vec<_> = h1.iter().skip(h2.len())
            //            .cloned().collect();
            //        let h1_new = Term::List(h1_new_head, t1.clone());
            //        h1_new.erl_eq(t2)
            //    } else {
            //        let h2_new_head: Vec<_> = h2.iter().skip(h1.len())
            //            .cloned().collect();
            //        let h2_new = Term::List(h2_new_head, t2.clone());
            //        t1.erl_eq(&h2_new)
            //    }
            //}
            (Term::Nil, _) => false,
            (_, Term::Nil) => false,

            (Term::Integer(ref i1), Term::Integer(ref i2)) => i1 == i2,
            (Term::Float(ref f1), Term::Float(ref f2)) => f1 == f2,
            (Term::Integer(_), Term::Float(_)) => unimplemented!(),
            (Term::Float(_), Term::Integer(_)) => unimplemented!(),
            (Term::Atom(ref a1), Term::Atom(ref a2)) => a1 == a2,
            (Term::Tuple(ref v1), Term::Tuple(ref v2)) =>
                v1.iter().zip(v2).all(|(e1, e2)| e1.erl_eq(e2)),
            (Term::CapturedFunction { ident: ref ident1 },
             Term::CapturedFunction { ident: ref ident2 }) =>
                ident1 == ident2,
            _ => {
                crate::trace::warning_args(
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
            (Term::ListCell(h1, t1), Term::ListCell(h2, t2)) => {
                h1.erl_exact_eq(h2) && t1.erl_exact_eq(t2)
            }
            //(Term::List(h1, t1), Term::Nil) if h1.len() == 0 =>
            //    if let Term::Nil = **t1 { true } else { false },
            //(Term::Nil, Term::List(h2, t2)) if h2.len() == 0 =>
            //    if let Term::Nil = **t2 { true } else { false },
            //(Term::List(h1, t1), Term::List(h2, t2)) => {
            //    let head_ok = h1.iter().zip(h2.iter())
            //        .all(|(v1, v2)| v1.erl_exact_eq(v2));
            //    if !head_ok { return false; }
            //    if h1.len() == h2.len() {
            //        t1.erl_exact_eq(t2)
            //    } else if h1.len() > h2.len() {
            //        let h1_new_head: Vec<_> = h1.iter().skip(h2.len())
            //            .cloned().collect();
            //        let h1_new = Term::List(h1_new_head, t1.clone());
            //        h1_new.erl_exact_eq(t2)
            //    } else {
            //        let h2_new_head: Vec<_> = h2.iter().skip(h1.len())
            //            .cloned().collect();
            //        let h2_new = Term::List(h2_new_head, t2.clone());
            //        t1.erl_exact_eq(&h2_new)
            //    }
            //}
            (Term::Nil, _) => false,
            (_, Term::Nil) => false,

            _ => {
                crate::trace::warning_args(
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
            //(Term::Float(val1), Term::Float(val2)) =>
            //    val1.cmp(val2),
            (_, _) => unimplemented!(),
        }
    }
}
