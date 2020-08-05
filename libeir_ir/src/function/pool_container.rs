use cranelift_bforest::SetForest;
use cranelift_entity::ListPool;

use super::{Block, Value};

#[derive(Clone)]
pub struct PoolContainer {
    pub value: ListPool<Value>,
    pub block_set: SetForest<Block>,
}
