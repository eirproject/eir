use std::collections::HashMap;
use std::ops::{ Index, IndexMut };

use cranelift_entity::{ PrimaryMap, entity_impl };
use cranelift_entity::packed_option::ReservedValue;
use libeir_util_datastructures::pooled_entity_set::EntitySet;
use super::{ Block, Const, PrimOp, Location };

#[derive(Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Value(u32);
entity_impl!(Value, "value");
impl Default for Value {
    fn default() -> Self {
        Value::reserved_value()
    }
}

#[derive(Debug, Clone)]
pub struct ValueData {
    pub(crate) kind: ValueKind,
    pub(crate) location: Option<Location>,
    pub(crate) usages: EntitySet<Block>,
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub enum ValueKind {
    Argument(Block, usize),
    Block(Block),
    Const(Const),
    PrimOp(PrimOp),
}

#[derive(Debug, Clone)]
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
        if let Some(key) = self.back.get(&kind) {
            *key
        } else {
            let val = self.primary.push(ValueData {
                kind,
                location: None,
                usages: EntitySet::new(),
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
