use cranelift_entity::ListPool;
use libeir_util_datastructures::pooled_entity_set::EntitySetPool;
use cranelift_bforest::SetForest;

use super::{ Value, Block, PatternClause };

#[derive(Clone)]
pub struct PoolContainer {
    pub value: ListPool<Value>,
    pub clause: ListPool<PatternClause>,
    pub block_set: SetForest<Block>,
}
