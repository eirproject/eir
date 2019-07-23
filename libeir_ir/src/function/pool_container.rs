use cranelift_entity::ListPool;
use libeir_util::pooled_entity_set::EntitySetPool;

use super::{ Value, PatternClause };

#[derive(Debug, Clone)]
pub struct PoolContainer {
    pub value: ListPool<Value>,
    pub clause: ListPool<PatternClause>,
    pub block_set: EntitySetPool,
}
