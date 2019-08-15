use super::{ Block, Value, PrimOp, Const };
use super::ValueKind;
use super::{ PrimOpData, PrimOpKind };
use super::{ Function };

use crate::{ BinOp, MapPutUpdate };
//use crate::algo::Mangler;
use crate::constant::{ ConstantContainer, IntoConst };
use crate::pattern::{ PatternContainer };

use libeir_diagnostics::ByteSpan;

mod op;
pub use op::CaseBuilder;

mod primop;

impl Function {

    pub fn builder(&mut self) -> FunctionBuilder<'_> {
        FunctionBuilder::new(self)
    }

}

pub trait IntoValue {
    fn into_value<'a>(self, b: &mut FunctionBuilder<'a>) -> Value;
}
impl IntoValue for Value {
    fn into_value<'a>(self, _b: &mut FunctionBuilder<'a>) -> Value {
        self
    }
}
impl IntoValue for Block {
    fn into_value<'a>(self, b: &mut FunctionBuilder<'a>) -> Value {
        b.fun.values.push(ValueKind::Block(self))
    }
}
impl IntoValue for PrimOp {
    fn into_value<'a>(self, b: &mut FunctionBuilder<'a>) -> Value {
        b.fun.values.push(ValueKind::PrimOp(self))
    }
}
impl<T> IntoValue for T where T: IntoConst {
    fn into_value<'a>(self, b: &mut FunctionBuilder<'a>) -> Value {
        let constant = b.fun.constant_container.from(self);
        b.fun.values.push(ValueKind::Const(constant))
    }
}

#[derive(Debug, Clone)]
enum BuilderState {
    MapPut {
        block: Block,
        action: Vec<MapPutUpdate>,
    },
}

pub struct FunctionBuilder<'a> {
    fun: &'a mut Function,

    state: Option<BuilderState>,

    block_buf: Option<Vec<Block>>,
    value_buf: Option<Vec<Value>>,
    const_pair_buf: Option<Vec<[Const; 2]>>,
    value_pair_buf: Option<Vec<[Value; 2]>>,

    //mangler: Mangler,
}

impl<'a> FunctionBuilder<'a> {

    pub fn new(fun: &'a mut Function) -> FunctionBuilder<'a> {
        // TODO separate allocated data structures into separate
        // reusable struct
        FunctionBuilder {
            fun,

            state: None,

            block_buf: Some(Vec::new()),
            value_buf: Some(Vec::new()),
            const_pair_buf: Some(Vec::new()),
            value_pair_buf: Some(Vec::new()),
        }
    }

    pub fn fun(&self) -> &Function {
        &self.fun
    }
    pub fn fun_mut(&mut self) -> &mut Function {
        &mut self.fun
    }

    pub fn pat(&self) -> &PatternContainer {
        &self.fun.pattern_container
    }
    pub fn pat_mut(&mut self) -> &mut PatternContainer {
        &mut self.fun.pattern_container
    }

    pub fn cons(&self) -> &ConstantContainer {
        &self.fun.constant_container
    }
    pub fn cons_mut(&mut self) -> &mut ConstantContainer {
        &mut self.fun.constant_container
    }

}

/// Values
impl<'a> FunctionBuilder<'a> {

    pub fn value<T>(&mut self, v: T) -> Value where T: IntoValue {
        v.into_value(self)
    }

}

/// Graph
impl<'a> FunctionBuilder<'a> {

    /// Updates the successors in the graph from the reads.
    /// Mainly used in the builder.
    fn graph_update_block(&mut self, block: Block) {
        let mut block_buf = self.block_buf.take().unwrap();
        let mut value_buf = self.value_buf.take().unwrap();
        debug_assert!(block_buf.is_empty());
        debug_assert!(value_buf.is_empty());

        // 1. Remove the block from all previous successors predecessor sets
        {
            let block_data = &self.fun.blocks[block];
            for successor in block_data.successors.iter(&self.fun.pool.block_set) {
                block_buf.push(successor);
            }
        }
        for successor in block_buf.iter() {
            let block_data = &mut self.fun.blocks[*successor];
            block_data.predecessors.remove(
                *successor, &mut self.fun.pool.block_set);
        }

        // 2. Add new successors to block
        block_buf.clear();
        {
            self.fun.block_walk_nested_values::<_, ()>(block, &mut |val| {
                value_buf.push(val);
                Ok(())
            }).unwrap();

            let block_data = &mut self.fun.blocks[block];
            block_data.successors.clear(&mut self.fun.pool.block_set);

            for value in value_buf.iter() {
                let value_data = &mut self.fun.values[*value];
                // Insert block as usage of value
                value_data.usages.insert(block, &mut self.fun.pool.block_set);

                // If the value is a block capture, insert into successors
                // for current block
                if let ValueKind::Block(dest_block) = &value_data.kind {
                    block_data.successors.insert(
                        *dest_block, &mut self.fun.pool.block_set);
                    block_buf.push(*dest_block);
                }
            }
        }


        // 3. Add block as predecessor to all successors
        for dest_block in block_buf.iter() {
            let block_data = &mut self.fun.blocks[*dest_block];
            block_data.predecessors.insert(block, &mut self.fun.pool.block_set);
        }

        block_buf.clear();
        value_buf.clear();
        self.block_buf = Some(block_buf);
        self.value_buf = Some(value_buf);
    }

}

/// Block modifiers
impl<'a> FunctionBuilder<'a> {

    pub fn block_insert(&mut self) -> Block {
        self.fun.block_insert()
    }

    /// Inserts a new block and get its value
    pub fn block_insert_get_val(&mut self) -> (Block, Value) {
        let block = self.block_insert();
        let val = self.value(block);
        (block, val)
    }

    pub fn block_arg_insert(&mut self, block: Block) -> Value {
        self.fun.block_arg_insert(block)
    }

    pub fn block_args(&self, block: Block) -> &[Value] {
        self.fun.block_args(block)
    }

    pub fn block_reads(&self, block: Block) -> &[Value] {
        self.fun.block_reads(block)
    }

    pub fn block_set_entry(&mut self, block: Block) {
        self.fun.entry_block = Some(block);
    }

    pub fn block_set_span(&mut self, block: Block, span: ByteSpan) {
        self.fun.blocks[block].span = span;
    }

    /// This will explicitly clear the operation contained in the
    /// block. This will remove all successors, and will cause
    /// this block to be removed from their predecessors.
    pub fn block_clear(&mut self, block: Block) {
        #[cfg(debug_assertions)]
        self.fun().graph_validate_block(block);

        let mut value_buf = self.value_buf.take().unwrap();
        debug_assert!(value_buf.is_empty());

        {
            let data = self.fun.blocks.get_mut(block).unwrap();

            data.op = None;
            for read in data.reads.as_slice(&self.fun.pool.value) {
                value_buf.push(*read);
            }
            data.successors.clear(&mut self.fun.pool.block_set);
            data.reads.clear(&mut self.fun.pool.value);
        }


        for value in value_buf.iter() {
            let data = &mut self.fun.values[*value];
            data.usages.remove(block, &mut self.fun.pool.block_set);

            if let ValueKind::Block(successor_block) = data.kind {
                let data = self.fun.blocks.get_mut(successor_block).unwrap();
                data.predecessors
                    .remove(block, &mut self.fun.pool.block_set);
            }
        }

        value_buf.clear();
        self.value_buf = Some(value_buf);
    }

    /// TODO temporary until we get a proper type system implemented.
    /// IR generators are required to annotate blocks that meet the
    /// following requirements with this:
    /// 1. The first two arguments of the block are return and throw
    ///    continuations respectively.
    /// 2. Control flow only escapes the blocks control flow scope
    ///    by one of these two continuations.
    pub fn block_annotate_is_fun(&mut self, block: Block) {
        assert!(self.fun.block_args(block).len() >= 2);
        self.fun.blocks[block].is_fun = true;
    }

}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::FunctionIdent;

    use libeir_intern::Ident;

    #[test]
    fn graph_impl() {
        let ident = FunctionIdent {
            module: Ident::from_str("test"),
            name: Ident::from_str("test"),
            arity: 1,
        };
        let mut fun = Function::new(ident);
        let mut b = fun.builder();

        {
            let ba = b.block_insert();
            let bb = b.block_insert();
            b.op_call(ba, bb, &[]);
            b.block_clear(ba);

            b.fun().graph_validate_global();

            let bc = b.block_insert();
            b.op_call(ba, bc, &[]);

            b.fun().graph_validate_global();
        }
    }

}

