use super::{ Block, Value };
use super::{ BlockData };
use super::{ ValueType, WriteToken };
use super::{ Function };
use super::{ AttributeKey, AttributeValue };

use crate::Atom;
use crate::{ FunctionIdent, ConstantTerm, AtomicTerm, ClosureEnv };
use crate::Clause;
use crate::op::{ OpKind, ComparisonOperation, TestOperation };

use matches::assert_matches;
use ::cranelift_entity::{ EntityList };
use util::pooled_entity_set::{ EntitySetPool, PooledEntitySet };

pub struct FunctionBuilder<'a> {
    fun: &'a mut Function,

    block_buf: Option<Vec<Block>>,
}

impl<'a> FunctionBuilder<'a> {

    pub fn new(fun: &'a mut Function) -> FunctionBuilder<'a> {
        // TODO separate allocated data structures into separate
        // reusable struct
        FunctionBuilder {
            fun: fun,

            block_buf: Some(Vec::new()),
        }
    }

}

/// Block modifiers
impl<'a> FunctionBuilder<'a> {

    /// This will explicitly clear the operation contained in the
    /// block. This will remove all successors, and will cause
    /// this block to be removed from their predecessors.
    pub fn block_clear(&mut self, block: Block) {
        let mut buf = self.block_buf.take().unwrap();
        debug_assert!(buf.len() == 0);

        {
            let data = self.fun.blocks.get_mut(block).unwrap();
            data.op = None;
            data.reads = EntityList::new();

            for successor in data.successors.iter(&self.fun.block_set_pool) {
                buf.push(successor);
            }

            data.successors = PooledEntitySet::new();
        }

        for block in buf.iter() {
            let data = self.fun.blocks.get_mut(*block).unwrap();
            data.predecessors.remove(*block, &mut self.fun.block_set_pool);
        }

        buf.clear();
        self.block_buf = Some(buf);
    }

}

struct CallBuilder<'a, 'b> {
    builder: &'b mut FunctionBuilder<'a>,
    block: Block,
}

/// Operation constructors
impl<'a> FunctionBuilder<'a> {

    fn op_call<'b>(&'b mut self, block: Block, target: Value) -> CallBuilder<'a, 'b> {
        unimplemented!()
    }

}











