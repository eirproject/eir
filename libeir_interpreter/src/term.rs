use ::std::cell::RefCell;
use ::std::rc::Rc;
use std::hash::{ Hash, Hasher };
use std::cmp::{ Ord, Ordering };
use std::collections::HashMap;

use libeir_intern::{ Symbol, LocalInternedString };

use libeir_ir::{ FunctionIdent, Block };

use libeir_util::binary::{ BitRead };
use libeir_util::binary::{ BitVec, BitSlice, copy as bit_copy };

//use ::num_bigint::BigInt;
use ::rug::Integer;
use ::num_traits::cast::ToPrimitive;

#[derive(Debug, Copy, Clone, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct Pid(pub usize);

#[derive(Debug, Copy, Clone, Eq, PartialEq, PartialOrd, Ord, Hash)]
pub struct Reference(pub usize);

#[derive(Debug, Copy, Clone, Eq, PartialEq, Hash)]
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

#[derive(Debug, Copy, Clone, PartialOrd)]
pub struct FloatTerm(pub f64);
impl PartialEq for FloatTerm {
    fn eq(&self, other: &FloatTerm) -> bool {
        self.0.to_bits() == other.0.to_bits()
    }
}
impl Ord for FloatTerm {
    fn cmp(&self, other: &FloatTerm) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}
impl Eq for FloatTerm {}
impl Hash for FloatTerm {
    fn hash<T: Hasher>(&self, state: &mut T) {
        self.0.to_bits().hash(state)
    }
}
impl From<f64> for FloatTerm {
    fn from(src: f64) -> Self {
        FloatTerm(src)
    }
}

#[derive(Debug, Clone)]
pub struct MapTerm {
    map: HashMap<Rc<Term>, Rc<Term>>,
    sorted: Vec<(Rc<Term>, Rc<Term>)>,
}
impl MapTerm {

    pub fn new() -> MapTerm {
        MapTerm {
            map: HashMap::new(),
            sorted: Vec::new(),
        }
    }

    pub fn insert(&mut self, key: Rc<Term>, val: Rc<Term>) -> bool {
        self.map.insert(key.clone(), val.clone());
        match self.sorted.binary_search_by(|(k, _)| k.cmp(&key)) {
            Ok(idx) => {
                self.sorted[idx] = (key, val);
                true
            },
            Err(idx) => {
                self.sorted.insert(idx, (key, val));
                false
            },
        }
    }

    pub fn get(&self, key: &Rc<Term>) -> Option<Rc<Term>> {
        self.map.get(key).cloned()
    }

    pub fn len(&self) -> usize {
        self.map.len()
    }

}
impl PartialEq for MapTerm {
    fn eq(&self, other: &MapTerm) -> bool {
        self.sorted == other.sorted
    }
}
impl Eq for MapTerm {}
impl PartialOrd for MapTerm {
    fn partial_cmp(&self, other: &MapTerm) -> Option<Ordering> {
        self.sorted.partial_cmp(&other.sorted)
    }
}
impl Ord for MapTerm {
    fn cmp(&self, other: &MapTerm) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}
impl Hash for MapTerm {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.sorted.hash(state)
    }
}

#[derive(Debug, Clone)]
pub enum Term {
    Nil,
    Integer(Integer),
    Float(FloatTerm),
    Atom(Symbol),
    Tuple(Vec<Rc<Term>>),
    ListCell(Rc<Term>, Rc<Term>),
    Map(MapTerm),
    Pid(Pid),
    Reference(Reference),
    Binary(Rc<BitVec>),
    BinarySlice {
        buf: Rc<BitVec>,
        bit_offset: usize,
        bit_length: usize,
    },
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

impl Term {
    fn order_idx(&self) -> usize {
        // number < atom < reference < fun < port < pid
        // < tuple < map < nil < list < bit string
        match self {
            Term::Integer(_) => 1,
            Term::Float(_) => 1,
            Term::Atom(_) => 2,
            Term::Reference(_) => 3,
            Term::BoundLambda { .. } => 4,
            Term::CapturedFunction { .. } => 4,
            //Term::Port(_) => 5,
            Term::Pid(_) => 6,
            Term::Tuple(_) => 7,
            Term::Map(_) => 8,
            Term::Nil => 9,
            Term::ListCell(_, _) => 10,
            Term::Binary(_) => 11,
            Term::BinarySlice { .. } => 11,
            _ => panic!(),
        }
    }
}

impl Eq for Term {}
impl PartialEq for Term {
    fn eq(&self, other: &Self) -> bool {
        use Term::*;
        match (self, other) {
            (Nil, Nil) => true,
            (Integer(l), Integer(r)) => l == r,
            (Float(l), Float(r)) => l == r,
            (Atom(l), Atom(r)) => l == r,
            (Tuple(l), Tuple(r)) => l == r,
            (ListCell(lh, lt), ListCell(rh, rt)) => lh == rh && lt == rt,
            (Map(l), Map(r)) => l == r,
            (Pid(l), Pid(r)) => l == r,
            (Reference(l), Reference(r)) => l == r,
            (Binary(l), Binary(r)) => l == r,
            (Binary(l), BinarySlice { buf, bit_offset, bit_length }) => {
                let rs = BitSlice::with_offset_length(
                    &**buf, *bit_offset, *bit_length);
                &**l == &rs
            },
            (BinarySlice { buf, bit_offset, bit_length }, Binary(r)) => {
                let ls = BitSlice::with_offset_length(
                    &**buf, *bit_offset, *bit_length);
                &ls == &**r
            },
            (BinarySlice { buf: lb, bit_offset: lo, bit_length: ll },
             BinarySlice { buf: rb, bit_offset: ro, bit_length: rl }) => {
                let ls = BitSlice::with_offset_length(
                    &**lb, *lo, *ll);
                let rs = BitSlice::with_offset_length(
                    &**rb, *ro, *rl);
                &ls == &rs
            },
            (BoundLambda { ident: li, block: lb, environment: le },
             BoundLambda { ident: ri, block: rb, environment: re }) =>
                li == ri && lb == rb && le == re,
            (CapturedFunction { ident: li },
             CapturedFunction { ident: ri }) =>
                li == ri,
            (ValueList(l), ValueList(r)) => l == r,
            (ReturnOk, ReturnOk) => true,
            (ReturnThrow, ReturnThrow) => true,
            _ => false,
        }
    }
}

impl Ord for Term {
    fn cmp(&self, other: &Self) -> Ordering {
        self.partial_cmp(other).unwrap()
    }
}
impl PartialOrd for Term {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        use Term::*;
        match (self, other) {
            (Nil, Nil) => Some(Ordering::Equal),
            (Integer(l), Integer(r)) => l.partial_cmp(r),
            (Float(l), Float(r)) => l.partial_cmp(r),
            (Atom(l), Atom(r)) => l.partial_cmp(r),
            (Tuple(l), Tuple(r)) => l.partial_cmp(r),
            (ListCell(lh, lt), ListCell(rh, rt)) => match lh.partial_cmp(rh) {
                Some(Ordering::Equal) | None => lt.partial_cmp(rt),
                non_eq => non_eq,
            },
            (Map(l), Map(r)) => l.partial_cmp(r),
            (Pid(l), Pid(r)) => l.partial_cmp(r),
            (Reference(l), Reference(r)) => l.partial_cmp(r),
            (Binary(l), Binary(r)) => l.partial_cmp(r),
            (Binary(l), BinarySlice { buf, bit_offset, bit_length }) => {
                let rs = BitSlice::with_offset_length(
                    &**buf, *bit_offset, *bit_length);
                (&**l).partial_cmp(&rs)
            },
            (BinarySlice { buf, bit_offset, bit_length }, Binary(r)) => {
                let ls = BitSlice::with_offset_length(
                    &**buf, *bit_offset, *bit_length);

                ls.partial_cmp(&**r)
            },
            (BinarySlice { buf: lb, bit_offset: lo, bit_length: ll },
             BinarySlice { buf: rb, bit_offset: ro, bit_length: rl }) => {
                let ls = BitSlice::with_offset_length(
                    &**lb, *lo, *ll);
                let rs = BitSlice::with_offset_length(
                    &**rb, *ro, *rl);
                ls.partial_cmp(&rs)
            },
            (BoundLambda { ident: li, block: lb, environment: le },
             BoundLambda { ident: ri, block: rb, environment: re }) =>
                match li.partial_cmp(ri) {
                    Some(Ordering::Equal) | None => match lb.partial_cmp(rb) {
                        Some(Ordering::Equal) | None => le.partial_cmp(re),
                        non_eq => non_eq,
                    }
                    non_eq => non_eq,
                }
            (CapturedFunction { ident: li },
             CapturedFunction { ident: ri }) =>
                li.partial_cmp(ri),
            (ValueList(l), ValueList(r)) => l.partial_cmp(r),
            (ReturnOk, ReturnOk) => Some(Ordering::Equal),
            (ReturnThrow, ReturnThrow) => Some(Ordering::Equal),
            (l, r) => l.order_idx().partial_cmp(&r.order_idx()),
        }
    }
}

impl Hash for Term {
    fn hash<H: Hasher>(&self, state: &mut H) {
        use Term::*;

        self.get_type().hash(state);
        match self {
            Nil => (),
            Integer(i) => i.hash(state),
            Float(f) => f.hash(state),
            Atom(a) => a.hash(state),
            Tuple(t) => t.hash(state),
            ListCell(h, t) => {
                h.hash(state);
                t.hash(state);
            },
            Map(t) => t.hash(state),
            Pid(t) => t.hash(state),
            Reference(t) => t.hash(state),
            Binary(t) => t.hash(state),
            BinarySlice { buf, bit_offset, bit_length } => {
                let ls = BitSlice::with_offset_length(
                    &**buf, *bit_offset, *bit_length);
                ls.hash(state);
            },
            BoundLambda { ident, block, environment } => {
                ident.hash(state);
                block.hash(state);
                environment.hash(state);
            },
            CapturedFunction { ident } => ident.hash(state),
            ValueList(i) => i.hash(state),
            ReturnOk => (),
            ReturnThrow => (),
        }
    }
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
    pub fn new_usize(num: usize) -> Self {
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
            Term::BinarySlice { .. } => TermType::Binary,
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

    pub fn as_integer(&self) -> Option<&Integer> {
        if let Term::Integer(ref bigint) = self {
            Some(bigint)
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

    pub fn as_tuple(&self) -> Option<&[Rc<Term>]> {
        if let Term::Tuple(tup) = self {
            Some(tup)
        } else {
            None
        }
    }

    pub fn as_binary(&self) -> Option<&BitVec> {
        if let Term::Binary(bin) = self {
            Some(bin)
        } else {
            None
        }
    }

    pub fn as_map(&self) -> Option<&MapTerm> {
        if let Term::Map(bin) = self {
            Some(bin)
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

    pub fn as_value_list(term: &Rc<Term>) -> Vec<Rc<Term>> {
        match &**term {
            Term::ValueList(val) => val.clone(),
            _ => panic!("is not value list {:?}", term),
        }
    }

    pub fn to_doc(term: Rc<Term>) -> ::pretty::Doc<'static, ::pretty::BoxDoc<'static>> {
        use ::pretty::{ Doc };
        match &*term {
            Term::Nil => Doc::text("[]"),
            Term::Integer(int) => Doc::text(int.to_string()),
            Term::Float(num) => Doc::text(num.0.to_string()),
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
            Term::Map(map) => {
                let entries_doc: Vec<_> = map.sorted.iter()
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
                if let Some(slice) = bin.try_as_byte_aligned_slice() {
                    if let Ok(utf) = std::str::from_utf8(slice) {
                        return Doc::text("\"")
                            .append(Doc::text(utf.to_string()))
                            .append(Doc::text("\""));
                    }
                }

                // TODO bit length
                let items: Vec<_> = bin.iter_bytes()
                    .map(|v| Doc::text(v.to_string()))
                    .collect();
                Doc::text("<")
                    .append(Doc::intersperse(items, Doc::text(",")))
                    .append(Doc::text(">"))
            },
            Term::BinarySlice { buf, bit_offset, bit_length } => {
                if bit_length % 8 == 0 {
                    let from = BitSlice::with_offset_length(
                        &**buf, *bit_offset, *bit_length);

                    let byte_len = bit_length / 8;
                    let mut bin = vec![0; byte_len];
                    bit_copy(&from, &mut bin as &mut [u8]);

                    if let Ok(utf) = std::str::from_utf8(&bin) {
                        return Doc::text("\"")
                            .append(Doc::text(utf.to_string()))
                            .append(Doc::text("\""));
                    } else {
                        let items: Vec<_> = bin.iter()
                            .map(|v| Doc::text(v.to_string()))
                            .collect();
                        Doc::text("<")
                            .append(Doc::intersperse(items, Doc::text(",")))
                            .append(Doc::text(">"))
                    }
                } else {
                    let items: Vec<_> = buf.iter_words()
                        .map(|v| Doc::text(v.to_string()))
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
        self == other
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
