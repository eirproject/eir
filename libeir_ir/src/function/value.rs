use std::collections::HashMap;
use std::ops::{ Index, IndexMut };

use cranelift_entity::{ PrimaryMap, entity_impl };
use libeir_util_datastructures::pooled_entity_set::PooledEntitySet;
use super::{ Block, Const, PrimOp };

#[derive(Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Value(u32);
entity_impl!(Value, "value");

#[derive(Debug, Clone)]
pub struct ValueData {
    pub(crate) kind: ValueKind,
    pub(crate) usages: PooledEntitySet<Block>,
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
                usages: PooledEntitySet::new(),
            });
            self.back.insert(kind, val);
            val
        }
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
