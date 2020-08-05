use std::collections::HashMap;
use std::ops::{Index, IndexMut};

use super::{Block, Const, Location, PrimOp};
use cranelift_bforest::Set;
use cranelift_entity::packed_option::ReservedValue;
use cranelift_entity::{entity_impl, PrimaryMap};
use libeir_util_datastructures::aux_traits::AuxDebug;

#[derive(Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Value(u32);
entity_impl!(Value, "value");
impl Default for Value {
    fn default() -> Self {
        Value::reserved_value()
    }
}
impl<C> AuxDebug<C> for Value {
    fn aux_fmt(&self, f: &mut std::fmt::Formatter<'_>, _aux: &C) -> std::fmt::Result {
        std::fmt::Debug::fmt(self, f)
    }
}

use libeir_util_dot_graph::NodeId;
impl NodeId for Value {
    fn make_id(&self, out: &mut String) {
        use std::fmt::Write;
        write!(out, "{}", self).unwrap();
    }
}

#[derive(Clone)]
pub struct ValueData {
    pub(crate) kind: ValueKind,
    pub(crate) location: Option<Location>,
    pub(crate) usages: Set<Block>,
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub enum ValueKind {
    Argument(Block, usize),
    Block(Block),
    Const(Const),
    PrimOp(PrimOp),
}

impl ValueKind {
    pub fn is_arg(&self) -> bool {
        match self {
            ValueKind::Argument(_, _) => true,
            _ => false,
        }
    }
}

#[derive(Clone)]
pub struct ValueMap {
    primary: PrimaryMap<Value, ValueData>,
    back: HashMap<ValueKind, Value>,
}

impl ValueMap {
    pub fn new() -> Self {
        ValueMap {
            primary: PrimaryMap::new(),
            back: HashMap::new(),
        }
    }

    pub fn push(&mut self, kind: ValueKind) -> Value {
        self.push_with_location(kind, None)
    }

    pub fn push_with_location(&mut self, kind: ValueKind, location: Option<Location>) -> Value {
        if let Some(key) = self.back.get(&kind) {
            *key
        } else {
            let val = self.primary.push(ValueData {
                kind,
                location,
                usages: Set::new(),
            });
            self.back.insert(kind, val);
            val
        }
    }

    pub fn get(&self, kind: ValueKind) -> Option<Value> {
        self.back.get(&kind).cloned()
    }
}

impl Index<Value> for ValueMap {
    type Output = ValueData;
    fn index(&self, key: Value) -> &ValueData {
        &self.primary[key]
    }
}
impl IndexMut<Value> for ValueMap {
    fn index_mut(&mut self, key: Value) -> &mut ValueData {
        &mut self.primary[key]
    }
}
