use super::{ Block, Value, PrimOp, Const, Location };
use super::ValueKind;
use super::{ PrimOpData, PrimOpKind };
use super::{ Function };

use crate::BinOp;
use crate::constant::{ ConstantContainer, IntoConst };
use crate::pattern::{ PatternContainer };

use cranelift_entity::EntityList;

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
    fn get_value(self, fun: &Function) -> Option<Value>;
}
impl IntoValue for Value {
    fn into_value<'a>(self, _b: &mut FunctionBuilder<'a>) -> Value {
        self
    }
    fn get_value(self, _fun: &Function) -> Option<Value> {
        Some(self)
    }
}
impl IntoValue for Block {
    fn into_value<'a>(self, b: &mut FunctionBuilder<'a>) -> Value {
        b.fun.values.push(ValueKind::Block(self))
    }
    fn get_value(self, fun: &Function) -> Option<Value> {
        fun.values.get(ValueKind::Block(self))
    }
}
impl IntoValue for PrimOp {
    fn into_value<'a>(self, b: &mut FunctionBuilder<'a>) -> Value {
        b.fun.values.push(ValueKind::PrimOp(self))
    }
    fn get_value(self, fun: &Function) -> Option<Value> {
        fun.values.get(ValueKind::PrimOp(self))
    }
}
impl<T> IntoValue for T where T: IntoConst {
    fn into_value<'a>(self, b: &mut FunctionBuilder<'a>) -> Value {
        let constant = b.fun.constant_container.from(self);
        let value = b.fun.values.push(ValueKind::Const(constant));
        b.fun.constant_values.insert(value);
        value
    }
    fn get_value(self, fun: &Function) -> Option<Value> {
        let constant = fun.constant_container.get(self);
        constant.and_then(|v| fun.values.get(ValueKind::Const(v)))
    }
}

pub enum DynValue {
    Value(Value),
    Block(Block),
    PrimOp(PrimOp),
    Const(Const),
}
impl Into<DynValue> for Value {
    fn into(self) -> DynValue {
        DynValue::Value(self)
    }
}
impl Into<DynValue> for Block {
    fn into(self) -> DynValue {
        DynValue::Block(self)
    }
}
impl Into<DynValue> for PrimOp {
    fn into(self) -> DynValue {
        DynValue::PrimOp(self)
    }
}
impl Into<DynValue> for Const {
    fn into(self) -> DynValue {
        DynValue::Const(self)
    }
}
impl IntoValue for DynValue {
    fn into_value<'a>(self, b: &mut FunctionBuilder<'a>) -> Value {
        match self {
            DynValue::Value(val) => val,
            DynValue::Block(block) => b.value(block),
            DynValue::PrimOp(prim) => b.value(prim),
            DynValue::Const(cons) => b.value(cons),
        }
    }
    fn get_value(self, fun: &Function) -> Option<Value> {
        match self {
            DynValue::Value(val) => Some(val),
            DynValue::Block(block) => fun.value_get(block),
            DynValue::PrimOp(prim) => fun.value_get(prim),
            DynValue::Const(cons) => fun.value_get(cons),
        }
    }
}

pub struct FunctionBuilder<'a> {
    fun: &'a mut Function,

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

    pub fn value_map<F>(&mut self, mut value: Value, map: &mut F) -> Value where F: FnMut(Value) -> Option<Value> {
        if let Some(new) = map(value) {
            value = new;
        }

        match self.fun().value_kind(value) {
            ValueKind::PrimOp(prim) => {
                let mut values = self.fun().primop_reads(prim).to_owned();
                for val in values.iter_mut() {
                    debug_assert!(*val != value);
                    let new_val = self.value_map(*val, map);
                    *val = new_val;
                }

                let kind = self.fun().primop_kind(prim).clone();
                self.prim_from_kind(kind, &values)
            }
            _ => value,
        }
    }


}

/// Graph
impl<'a> FunctionBuilder<'a> {

    /// Updates the successors in the graph from the reads.
    /// Mainly used in the builder.
    pub(crate) fn graph_update_block(&mut self, block: Block) {
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

    pub fn block_set_location(&mut self, block: Block, loc: Location) {
        self.fun.blocks[block].location = loc;
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

    pub fn block_value_map<F>(&mut self, block: Block, mut map: F)
    where F: FnMut(Value) -> Value
    {
        let num_reads = self.fun.block_reads(block).len();

        let mut new_reads = EntityList::new();
        for val_num in 0..num_reads {
            let val = self.fun.block_reads(block)[val_num];
            let new_val = self.value_map(val, &mut |v| Some(map(v)));
            new_reads.push(new_val, &mut self.fun.pool.value);
        }

        self.fun.blocks[block].reads = new_reads;
    }

    pub fn block_copy_body_map<F>(&mut self, from: Block, to: Block, mut map: F) where F: FnMut(Value) -> Option<Value> {
        let op;
        let loc;
        {
            let from_data = &self.fun.blocks[from];
            op = from_data.op.clone();
            loc = from_data.location;
        }

        let mut reads = EntityList::new();
        {
            let len = self.fun.block_reads(from).len();
            for n in 0..len {
                let val = self.fun.block_reads(from)[n];
                let new = self.value_map(val, &mut map);
                reads.push(new, &mut self.fun.pool.value);
            }
        }

        let to_data = &mut self.fun.blocks[to];
        to_data.op = op;
        to_data.reads = reads;
        to_data.location = loc;

        self.graph_update_block(to);
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
            b.op_call_flow(ba, bb, &[]);
            b.block_clear(ba);

            b.fun().graph_validate_global();

            let bc = b.block_insert();
            b.op_call_flow(ba, bc, &[]);

            b.fun().graph_validate_global();
        }
    }

}

