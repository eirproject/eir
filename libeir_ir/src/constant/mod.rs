use std::hash::{ Hash, Hasher };

use libeir_intern::{ Ident };

use libeir_util_datastructures::aux_hash_map::{ AuxHashMap, AuxHash, AuxEq };

use cranelift_entity::{ PrimaryMap, ListPool, EntityList, entity_impl };

mod atomic;
pub use atomic::*;
mod float;
pub use libeir_util_number::{Integer, ToPrimitive, FromPrimitive};

/// These entities has the property that if they are equal, they
/// represent the same value.
#[derive(Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Const(u32);
entity_impl!(Const, "const_value");

#[derive(Debug, Clone)]
pub enum ConstKind {
    Atomic(AtomicTerm),
    ListCell {
        head: Const,
        tail: Const,
    },
    Tuple {
        entries: EntityList<Const>,
    },
    Map {
        /// Key/value pairs are ALWAYS ordered by key constant index
        keys: EntityList<Const>,
        values: EntityList<Const>,
    }
}
impl AuxHash<ListPool<Const>> for ConstKind {
    fn aux_hash<H: Hasher>(&self, state: &mut H, container: &ListPool<Const>) {
        match self {
            ConstKind::Atomic(atomic) => {
                0.hash(state);
                atomic.hash(state);
            }
            ConstKind::ListCell { head, tail } => {
                1.hash(state);
                head.hash(state);
                tail.hash(state);
            }
            ConstKind::Tuple { entries } => {
                2.hash(state);
                entries.as_slice(container).hash(state);
            }
            ConstKind::Map { keys, values } => {
                3.hash(state);
                keys.as_slice(container).hash(state);
                values.as_slice(container).hash(state);
            }
        }
    }
}
impl AuxEq<ListPool<Const>> for ConstKind {
    fn aux_eq(&self, rhs: &ConstKind, container: &ListPool<Const>) -> bool {
        match (self, rhs) {
            (ConstKind::Atomic(l), ConstKind::Atomic(r)) => l == r,
            (ConstKind::ListCell { head: lh, tail: lt },
             ConstKind::ListCell { head: rh, tail: rt }) => lh == rh && lt == rt,
            (ConstKind::Tuple { entries: l }, ConstKind::Tuple { entries: r }) =>
                l.as_slice(container) == r.as_slice(container),
            (ConstKind::Map { keys: lk, values: lv }, ConstKind::Map { keys: rk, values: rv }) =>
                lk.as_slice(container) == rk.as_slice(container)
                && lv.as_slice(container) == rv.as_slice(container),
            _ => false,
        }
    }
}

#[derive(Debug, Clone)]
pub struct ConstantContainer {
    const_values: PrimaryMap<Const, ConstKind>,
    value_map: AuxHashMap<ConstKind, Const, ListPool<Const>>,
    pub const_pool: ListPool<Const>,
}

impl Default for ConstantContainer {
    fn default() -> Self {
        ConstantContainer {
            const_values: PrimaryMap::new(),
            value_map: AuxHashMap::new(),
            const_pool: ListPool::new(),
        }
    }
}

impl ConstantContainer {

    pub fn new() -> Self {
        Self::default()
    }

    pub fn const_kind(&self, value: Const) -> &ConstKind {
        &self.const_values[value]
    }

    pub fn list_cell(&mut self, head: Const, tail: Const) -> Const {
        self.from(ConstKind::ListCell { head, tail })
    }

    pub fn nil(&mut self) -> Const {
        self.from(NilTerm)
    }

    pub fn from<T>(&mut self, val: T) -> Const where T: IntoConst {
        val.into_const(self)
    }

    pub fn get<T>(&self, val: T) -> Option<Const> where T: IntoConst {
        val.get_const(self)
    }

    //pub fn print<T>(&self, val: Const, fmt: &mut T)
    //where T: crate::text::TextFormatter
    //{
    //    match &self.const_values[val] {
    //        ConstKind::Atomic(atomic) => {
    //            fmt.write(&format!("{}", atomic));
    //        }
    //        _ => unimplemented!()
    //    }
    //}

    pub fn as_bool(&self, val: Const) -> Option<bool> {
        let kind = &self.const_values[val];
        match kind {
            ConstKind::Atomic(AtomicTerm::Atom(atom)) if atom == "true" => Some(true),
            ConstKind::Atomic(AtomicTerm::Atom(atom)) if atom == "false" => Some(false),
            _ => None,
        }
    }

    pub fn write(&self, val: Const, out: &mut dyn std::io::Write) {
        match &self.const_values[val] {
            ConstKind::Atomic(atomic) => {
                write!(out, "{}", atomic).unwrap();
            }
            ConstKind::ListCell { head, tail } => {
                write!(out, "[").unwrap();
                self.write(*head, out);
                write!(out, " | ").unwrap();
                self.write(*tail, out);
                write!(out, "]").unwrap();
            }
            ConstKind::Tuple { entries } => {
                write!(out, "{{").unwrap();
                for (n, entry) in entries.as_slice(&self.const_pool).iter().enumerate() {
                    if n != 0 {
                        write!(out, ", ").unwrap();
                    }
                    self.write(*entry, out);
                }
                write!(out, "}}").unwrap();
            }
            ConstKind::Map { keys, values } => {
                write!(out, "%{{").unwrap();
                for (n, (key, val)) in keys.as_slice(&self.const_pool).iter()
                    .zip(values.as_slice(&self.const_pool))
                    .enumerate()
                {
                    if n != 0 {
                        write!(out, ", ").unwrap();
                    }
                    self.write(*key, out);
                    write!(out, ": ").unwrap();
                    self.write(*val, out);
                }
                write!(out, "}}").unwrap();
            }
        }
    }

    pub fn tuple_builder(&self) -> TupleBuilder {
        TupleBuilder::new()
    }

    pub fn eq_other(&self, l: Const, r_cont: &ConstantContainer, r: Const) -> bool {
        match (&self.const_values[l], &r_cont.const_values[r]) {
            (ConstKind::Atomic(la), ConstKind::Atomic(ra)) if la == ra => true,
            (ConstKind::Atomic(_), ConstKind::Atomic(_)) => false,
            (ConstKind::Tuple { entries: t1 }, ConstKind::Tuple { entries: t2 }) => {
                let s1 = t1.as_slice(&self.const_pool);
                let s2 = t2.as_slice(&r_cont.const_pool);
                if s1.len() != s2.len() { return false; }
                for (e1, e2) in s1.iter().zip(s2.iter()) {
                    if !self.eq_other(*e1, r_cont, *e2) { return false; }
                }
                true
            },
            l => unimplemented!("{:?}", l),
        }
    }

}

pub trait IntoConst {
    fn into_const(self, c: &mut ConstantContainer) -> Const;
    fn get_const(self, fun: &ConstantContainer) -> Option<Const>;
}

pub struct EmptyMap;
impl IntoConst for EmptyMap {
    fn into_const(self, c: &mut ConstantContainer) -> Const {
        c.from(ConstKind::Map {
            keys: EntityList::new(),
            values: EntityList::new(),
        })
    }
    fn get_const(self, c: &ConstantContainer) -> Option<Const> {
        c.get(ConstKind::Map {
            keys: EntityList::new(),
            values: EntityList::new(),
        })
    }
}

impl<T> IntoConst for T where T: Into<AtomicTerm> {
    fn into_const(self, c: &mut ConstantContainer) -> Const {
        c.from(ConstKind::Atomic(self.into()))
    }
    fn get_const(self, c: &ConstantContainer) -> Option<Const> {
        c.get(ConstKind::Atomic(self.into()))
    }
}

impl IntoConst for ConstKind {
    fn into_const(self, c: &mut ConstantContainer) -> Const {
        if let Some(val) = c.value_map.get(&self, &c.const_pool) {
            *val
        } else {
            let val = c.const_values.push(self.clone());
            c.value_map.try_insert(self, val, &c.const_pool).unwrap();
            val
        }
    }
    fn get_const(self, c: &ConstantContainer) -> Option<Const> {
        c.value_map.get(&self, &c.const_pool).cloned()
    }
}

impl IntoConst for Const {
    fn into_const(self, _c: &mut ConstantContainer) -> Const {
        self
    }
    fn get_const(self, _c: &ConstantContainer) -> Option<Const> {
        Some(self)
    }
}

impl IntoConst for Ident {
    fn into_const(self, c: &mut ConstantContainer) -> Const {
        self.name.into_const(c)
    }
    fn get_const(self, c: &ConstantContainer) -> Option<Const> {
        self.name.get_const(c)
    }
}

pub struct TupleBuilder {
    elements: EntityList<Const>,
}
impl TupleBuilder {

    pub fn new() -> Self {
        TupleBuilder {
            elements: EntityList::new(),
        }
    }

    pub fn push(&mut self, elem: Const, c: &mut ConstantContainer) {
        self.elements.push(elem, &mut c.const_pool);
    }

    pub fn clear(mut self, c: &mut ConstantContainer) {
        self.elements.clear(&mut c.const_pool);
    }

    pub fn finish(self, c: &mut ConstantContainer) -> Const {
        c.from(ConstKind::Tuple {
            entries: self.elements,
        })
    }

}

//struct TupleTerm<T: IntoConst, I: IntoIterator<Item = T>>(I);
//impl<T: IntoConst, I: IntoIterator<Item = T>> IntoConst for TupleTerm<T, I> {
//    fn into_const(self, c: &mut ConstantContainer) -> Const {
//        for typ
//    }
//}
