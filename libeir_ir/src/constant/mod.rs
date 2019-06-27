use std::collections::HashMap;

use libeir_intern::{ Ident };
use libeir_diagnostics::{ ByteSpan, DUMMY_SPAN };

use cranelift_entity::{ PrimaryMap, ListPool, EntityList, entity_impl };

mod atomic;
pub use atomic::*;
mod float;

/// This is an unspanned constant.
/// These entities has the property that if they are equal, they
/// represent the same value.
#[derive(Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct ConstValue(u32);
entity_impl!(ConstValue, "const_value");

/// These contain both a source span and a ConstValue.
#[derive(Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Const(u32);
entity_impl!(Const, "const");

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
enum ConstValueKind {
    Atomic(AtomicTerm),
    ListCell {
        head: ConstValue,
        tail: ConstValue,
    },
    Tuple {
        head: ConstValue,
        /// Required to be of the tuple kind
        tail: Option<ConstValue>,
    }
}

/// In the case of constant composite terms, they don't necessarily
/// have a continuous source span. We want to enable these to be
/// represented by a separate span for each of their atomic values.
/// In the case that a composite term actually has a single span,
/// it is fully legal to specify that as an Atomic span.
#[derive(Debug)]
enum SpanKind {
    Atomic(ByteSpan),
}

#[derive(Debug)]
struct ConstData {
    value: ConstValue,
    span: SpanKind,
}

#[derive(Debug)]
pub struct ConstantContainer {
    const_values: PrimaryMap<ConstValue, ConstValueKind>,
    consts: PrimaryMap<Const, ConstData>,

    value_map: HashMap<ConstValueKind, ConstValue>,
}

impl ConstantContainer {

    pub fn new() -> Self {
        ConstantContainer {
            const_values: PrimaryMap::new(),
            consts: PrimaryMap::new(),

            value_map: HashMap::new(),
        }
    }

    pub fn const_value(&self, cons: Const) -> ConstValue {
        self.consts[cons].value
    }

    fn create_value(&mut self, kind: ConstValueKind) -> ConstValue {
        if let Some(val) = self.value_map.get(&kind) {
            *val
        } else {
            let val = self.const_values.push(kind.clone());
            self.value_map.insert(kind, val);
            val
        }
    }

    pub fn value_list_cell(&mut self, head: ConstValue, tail: ConstValue) -> ConstValue {
        self.create_value(ConstValueKind::ListCell { head, tail })
    }

    pub fn from<T>(&mut self, val: T) -> Const where T: IntoConst {
        val.into_const(self)
    }
    pub fn value_from<T>(&mut self, val: T) -> ConstValue where T: IntoConst {
        // TODO: Remove the useless Const creation
        let v = self.from(val);
        self.const_value(v)
    }

    pub fn print<T>(&self, val: ConstValue, fmt: &mut T)
    where T: crate::text::TextFormatter
    {
        match &self.const_values[val] {
            ConstValueKind::Atomic(atomic) => {
                fmt.write(&format!("{}", atomic));
            }
            _ => unimplemented!()
        }
    }

    pub fn write(&self, val: ConstValue, out: &mut dyn std::io::Write) {
        match &self.const_values[val] {
            ConstValueKind::Atomic(atomic) => {
                write!(out, "{}", atomic).unwrap();
            }
            // TODO
            kind => (), //unimplemented!("{:?}", kind)
        }
    }

}

pub trait IntoConst {
    fn into_const(self, c: &mut ConstantContainer) -> Const;
}

impl<T> IntoConst for (T, ByteSpan) where T: Into<AtomicTerm> {
    fn into_const(self, c: &mut ConstantContainer) -> Const {
        let atomic: AtomicTerm = self.0.into();
        let value = c.create_value(ConstValueKind::Atomic(atomic));
        c.consts.push(ConstData {
            value,
            span: SpanKind::Atomic(self.1),
        })
    }
}
impl<T> IntoConst for T where T: Into<AtomicTerm> {
    fn into_const(self, c: &mut ConstantContainer) -> Const {
        let atomic: AtomicTerm = self.into();
        let value = c.create_value(ConstValueKind::Atomic(atomic));
        c.consts.push(ConstData {
            value,
            span: SpanKind::Atomic(DUMMY_SPAN),
        })
    }
}

impl IntoConst for (ConstValue, ByteSpan) {
    fn into_const(self, c: &mut ConstantContainer) -> Const {
        c.consts.push(ConstData {
            value: self.0,
            span: SpanKind::Atomic(self.1),
        })
    }
}

impl IntoConst for Const {
    fn into_const(self, _c: &mut ConstantContainer) -> Const {
        self
    }
}

impl IntoConst for Ident {
    fn into_const(self, c: &mut ConstantContainer) -> Const {
        (self.name, self.span).into_const(c)
    }
}

//struct TupleTerm<T: IntoConst, I: IntoIterator<Item = T>>(I);
//impl<T: IntoConst, I: IntoIterator<Item = T>> IntoConst for TupleTerm<T, I> {
//    fn into_const(self, c: &mut ConstantContainer) -> Const {
//        for typ
//    }
//}
