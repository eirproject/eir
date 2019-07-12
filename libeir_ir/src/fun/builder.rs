use super::{ Block, Value };
use super::{ ValueData, ValueType };
use super::{ Function };

use crate::constant::{ ConstantContainer, Const, IntoConst };
use crate::op::{ OpKind, BinOp, MapPutUpdate };
use crate::pattern::{ PatternContainer, PatternClause };

use ::cranelift_entity::{ EntityList };

use libeir_diagnostics::{ ByteSpan, DUMMY_SPAN };
use libeir_intern::{ Symbol, Ident };
use libeir_util::pooled_entity_set::{ PooledEntitySet };

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
        b.fun.values.push(ValueData {
            kind: ValueType::Block(self),
            usages: PooledEntitySet::new(),

            span: DUMMY_SPAN,
        })
    }
}
impl<T> IntoValue for T where T: IntoConst {
    fn into_value<'a>(self, b: &mut FunctionBuilder<'a>) -> Value {
        b.fun.values.push(ValueData {
            kind: ValueType::Constant(b.fun.constant_container.from(self)),
            usages: PooledEntitySet::new(),

            span: DUMMY_SPAN,
        })
    }
}

#[derive(Debug, Clone)]
enum BuilderState {
    MapPut {
        block: Block,
        action: Vec<MapPutUpdate>,
    },
    Case {
        block: Block,
        clauses: EntityList<PatternClause>,
        val: Value,
        clauses_b: EntityList<Value>,
        values: EntityList<Value>,
    },
}

pub struct FunctionBuilder<'a> {
    fun: &'a mut Function,

    state: Option<BuilderState>,

    block_buf: Option<Vec<Block>>,
    value_buf: Option<Vec<Value>>,
}

impl<'a> FunctionBuilder<'a> {

    pub fn new(fun: &'a mut Function) -> FunctionBuilder<'a> {
        // TODO separate allocated data structures into separate
        // reusable struct
        FunctionBuilder {
            fun: fun,

            state: None,

            block_buf: Some(Vec::new()),
            value_buf: Some(Vec::new()),
        }
    }

    pub fn fun<'b>(&'b self) -> &'b Function {
        &self.fun
    }
    pub fn fun_mut<'b>(&'b mut self) -> &'b mut Function {
        &mut self.fun
    }

    pub fn pat<'b>(&'b self) -> &'b PatternContainer {
        &self.fun.pattern_container
    }
    pub fn pat_mut<'b>(&'b mut self) -> &'b mut PatternContainer {
        &mut self.fun.pattern_container
    }

    pub fn cons<'b>(&'b self) -> &'b ConstantContainer {
        &self.fun.constant_container
    }
    pub fn cons_mut<'b>(&'b mut self) -> &'b mut ConstantContainer {
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
        debug_assert!(block_buf.len() == 0);

        // 1. Remove the block from all previous successors predecessor sets
        {
            let block_data = &self.fun.blocks[block];
            for successor in block_data.successors.iter(&self.fun.block_set_pool) {
                block_buf.push(successor);
            }
        }
        for successor in block_buf.iter() {
            let block_data = &mut self.fun.blocks[*successor];
            assert!(block_data.predecessors.remove(*successor, &mut self.fun.block_set_pool));
        }

        // 2. Add new successors to block
        block_buf.clear();
        {
            let block_data = &mut self.fun.blocks[block];
            block_data.successors.clear(&mut self.fun.block_set_pool);

            for value in block_data.reads.as_slice(&self.fun.value_pool) {
                let value_data = &mut self.fun.values[*value];
                // Insert block as usage of value
                value_data.usages.insert(block, &mut self.fun.block_set_pool);

                // If the value is a block capture, insert into successors for current block
                if let ValueType::Block(dest_block) = &value_data.kind {
                    block_data.successors.insert(*dest_block, &mut self.fun.block_set_pool);
                    block_buf.push(*dest_block);
                }
            }
        }


        // 3. Add block as predecessor to all successors
        for dest_block in block_buf.iter() {
            let block_data = &mut self.fun.blocks[*dest_block];
            block_data.predecessors.insert(block, &mut self.fun.block_set_pool);
        }

        block_buf.clear();
        self.block_buf = Some(block_buf);
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

    pub fn block_args<'b>(&'b self, block: Block) -> &'b [Value] {
        self.fun.block_args(block)
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
        debug_assert!(value_buf.len() == 0);

        {
            let data = self.fun.blocks.get_mut(block).unwrap();

            data.op = None;
            for read in data.reads.as_slice(&self.fun.value_pool) {
                value_buf.push(*read);
            }
            data.successors.clear(&mut self.fun.block_set_pool);
            data.reads.clear(&mut self.fun.value_pool);
        }


        for value in value_buf.iter() {
            let data = &mut self.fun.values[*value];
            assert!(data.usages.remove(block, &mut self.fun.block_set_pool));

            match data.kind {
                ValueType::Block(successor_block) => {
                    let data = self.fun.blocks.get_mut(successor_block).unwrap();
                    assert!(data.predecessors
                            .remove(block, &mut self.fun.block_set_pool));
                }
                ValueType::Alias(_) => panic!(),
                _ => {},
            }
        }

        value_buf.clear();
        self.value_buf = Some(value_buf);
    }

}

//struct CallBuilder<'a, 'b> {
//    builder: &'b mut FunctionBuilder<'a>,
//    block: Block,
//}

/// Operation constructors
impl<'a> FunctionBuilder<'a> {

    pub fn op_call<'b, V>(&'b mut self, block: Block,
                       target: V, args: &[Value]) where V: IntoValue {
        assert!(self.state.is_none());

        let target_val = self.value(target);

        let data = self.fun.blocks.get_mut(block).unwrap();
        assert!(data.op.is_none());
        assert!(data.reads.is_empty());

        data.op = Some(OpKind::Call);
        data.reads.push(target_val, &mut self.fun.value_pool);
        data.reads.extend(args.iter().cloned(), &mut self.fun.value_pool);

        self.graph_update_block(block);
    }

    pub fn op_intrinsic<'b>(&'b mut self, block: Block, name: Symbol, args: &[Value]) {
        assert!(self.state.is_none());

        let data = self.fun.blocks.get_mut(block).unwrap();
        assert!(data.op.is_none());
        assert!(data.reads.is_empty());

        data.op = Some(OpKind::Intrinsic(name));
        data.reads.extend(args.iter().cloned(), &mut self.fun.value_pool);

        self.graph_update_block(block);
    }
    pub fn op_intrinsic_build(&mut self, name: Symbol) -> IntrinsicBuilder {
        IntrinsicBuilder::new(name, self)
    }

    pub fn op_capture_function<'b, M, F, A>(
        &'b mut self, block: Block,
        m: M, f: F, a: A) -> Block
    where
        M: IntoValue, F: IntoValue, A: IntoValue
    {
        assert!(self.state.is_none());

        let cont = self.fun.block_insert();
        let cont_val = self.value(cont);
        self.fun.block_arg_insert(cont);

        let m_val = self.value(m);
        let f_val = self.value(f);
        let a_val = self.value(a);

        let data = self.fun.blocks.get_mut(block).unwrap();
        assert!(data.op.is_none());
        assert!(data.reads.is_empty());

        data.op = Some(OpKind::CaptureFunction);
        data.reads.push(cont_val, &mut self.fun.value_pool);
        data.reads.push(m_val, &mut self.fun.value_pool);
        data.reads.push(f_val, &mut self.fun.value_pool);
        data.reads.push(a_val, &mut self.fun.value_pool);

        self.graph_update_block(block);

        cont
    }

    pub fn op_make_tuple(&mut self, block: Block, values: &[Value]) -> Block {
        assert!(self.state.is_none());

        let cont = self.fun.block_insert();
        let cont_val = self.value(cont);
        self.fun.block_arg_insert(cont);

        let data = self.fun.blocks.get_mut(block).unwrap();
        assert!(data.op.is_none());
        assert!(data.reads.is_empty());

        data.op = Some(OpKind::MakeTuple);
        data.reads.push(cont_val, &mut self.fun.value_pool);
        data.reads.extend(values.iter().cloned(), &mut self.fun.value_pool);

        self.graph_update_block(block);

        cont
    }
    pub fn op_make_tuple_build(&mut self) -> MakeTupleBuilder {
        MakeTupleBuilder::new(self)
    }

    pub fn op_make_list(&mut self, block: Block, head: &[Value], tail: Value) -> Block {
        assert!(self.state.is_none());

        let cont = self.fun.block_insert();
        let cont_val = self.value(cont);
        self.fun.block_arg_insert(cont);

        let data = self.fun.blocks.get_mut(block).unwrap();
        assert!(data.op.is_none());
        assert!(data.reads.is_empty());

        data.op = Some(OpKind::MakeList);
        data.reads.push(cont_val, &mut self.fun.value_pool);
        data.reads.push(tail, &mut self.fun.value_pool);
        data.reads.extend(head.iter().cloned(), &mut self.fun.value_pool);

        self.graph_update_block(block);

        cont
    }

    pub fn op_make_map_empty(&mut self, block: Block) -> Block {
        assert!(self.state.is_none());

        let cont = self.fun.block_insert();
        let cont_val = self.value(cont);
        self.fun.block_arg_insert(cont);

        let data = self.fun.blocks.get_mut(block).unwrap();
        assert!(data.op.is_none());
        assert!(data.reads.is_empty());

        data.op = Some(OpKind::MapEmpty);
        data.reads.push(cont_val, &mut self.fun.value_pool);

        self.graph_update_block(block);

        cont
    }

    pub fn op_start_map_put(&mut self, block: Block, map: Value) -> Block {
        assert!(self.state.is_none());
        self.state = Some(BuilderState::MapPut {
            block: block,
            action: Vec::new(),
        });

        let cont = self.fun.block_insert();
        let cont_val = self.value(cont);
        self.fun.block_arg_insert(cont);

        let data = self.fun.blocks.get_mut(block).unwrap();
        assert!(data.op.is_none());
        assert!(data.reads.is_empty());

        data.reads.push(cont_val, &mut self.fun.value_pool);
        data.reads.push(map, &mut self.fun.value_pool);

        cont
    }

    pub fn map_put_kv(&mut self, key: Value, val: Value, typ: MapPutUpdate) {
        if let Some(BuilderState::MapPut { block, ref mut action }) = self.state {
            let data = self.fun.blocks.get_mut(block).unwrap();
            data.reads.push(key, &mut self.fun.value_pool);
            data.reads.push(val, &mut self.fun.value_pool);
            action.push(typ);
        } else {
            panic!()
        }
    }

    pub fn map_put_finish(&mut self) {
        let state = self.state.take().unwrap();
        if let BuilderState::MapPut { block, action } = state {
            let data = self.fun.blocks.get_mut(block).unwrap();
            data.op = Some(OpKind::MapPut { action });
            self.graph_update_block(block);
        } else {
            panic!()
        }
    }

    pub fn op_pack_value_list(&mut self, block: Block, values: &[Value]) -> Block {
        assert!(self.state.is_none());

        let cont = self.fun.block_insert();
        let cont_val = self.value(cont);
        self.fun.block_arg_insert(cont);

        let data = self.fun.blocks.get_mut(block).unwrap();
        assert!(data.op.is_none());
        assert!(data.reads.is_empty());

        data.op = Some(OpKind::PackValueList);
        data.reads.push(cont_val, &mut self.fun.value_pool);
        data.reads.extend(values.iter().cloned(), &mut self.fun.value_pool);

        self.graph_update_block(block);

        cont
    }
    pub fn op_pack_value_list_build(&mut self) -> PackValueListBuilder {
        PackValueListBuilder::new(self)
    }

    pub fn op_case_build(&mut self) -> CaseBuilder {
        CaseBuilder::new()
    }

    pub fn op_unpack_value_list(&mut self, block: Block, list: Value, num: usize) -> Block {
        assert!(self.state.is_none());

        let cont = self.fun.block_insert();
        let cont_val = self.value(cont);
        for _ in 0..num {
            self.fun.block_arg_insert(cont);
        }

        let data = self.fun.blocks.get_mut(block).unwrap();
        assert!(data.op.is_none());
        assert!(data.reads.is_empty());

        data.op = Some(OpKind::UnpackValueList(num));
        data.reads.push(cont_val, &mut self.fun.value_pool);
        data.reads.push(list, &mut self.fun.value_pool);

        self.graph_update_block(block);

        cont
    }

    pub fn op_binop(&mut self, block: Block, op: BinOp, lhs: Value, rhs: Value) -> (Block, Block) {
        assert!(self.state.is_none());

        let ok_cont = self.fun.block_insert();
        let ok_cont_val = self.value(ok_cont);
        let err_cont = self.fun.block_insert();
        let err_cont_val = self.value(err_cont);

        let data = self.fun.blocks.get_mut(block).unwrap();
        assert!(data.op.is_none());
        assert!(data.reads.is_empty());

        data.op = Some(OpKind::BinOp(op));
        data.reads.push(ok_cont_val, &mut self.fun.value_pool);
        data.reads.push(err_cont_val, &mut self.fun.value_pool);
        data.reads.push(lhs, &mut self.fun.value_pool);
        data.reads.push(rhs, &mut self.fun.value_pool);

        self.graph_update_block(block);

        (ok_cont, err_cont)
    }

    pub fn op_binop_value(&mut self, block: Block, op: BinOp, lhs: Value, rhs: Value) -> Block {
        assert!(self.state.is_none());

        let cont = self.fun.block_insert();
        self.fun.block_arg_insert(cont);
        let cont_val = self.value(cont);

        let data = self.fun.blocks.get_mut(block).unwrap();
        assert!(data.op.is_none());
        assert!(data.reads.is_empty());

        data.op = Some(OpKind::BinOpValue(op));
        data.reads.push(cont_val, &mut self.fun.value_pool);
        data.reads.push(lhs, &mut self.fun.value_pool);
        data.reads.push(rhs, &mut self.fun.value_pool);

        self.graph_update_block(block);

        cont
    }

    pub fn op_if_bool(&mut self, block: Block, value: Value) -> (Block, Block, Block) {
        assert!(self.state.is_none());

        let true_cont = self.fun.block_insert();
        let true_cont_val = self.value(true_cont);
        let false_cont = self.fun.block_insert();
        let false_cont_val = self.value(false_cont);
        let non_cont = self.fun.block_insert();
        let non_cont_val = self.value(non_cont);

        let data = self.fun.blocks.get_mut(block).unwrap();
        assert!(data.op.is_none());
        assert!(data.reads.is_empty());

        data.op = Some(OpKind::IfBool);
        data.reads.push(true_cont_val, &mut self.fun.value_pool);
        data.reads.push(false_cont_val, &mut self.fun.value_pool);
        data.reads.push(non_cont_val, &mut self.fun.value_pool);
        data.reads.push(value, &mut self.fun.value_pool);

        self.graph_update_block(block);

        (true_cont, false_cont, non_cont)
    }

    pub fn op_unpack_tuple(&mut self, block: Block, val: Value, num: usize) -> (Block, Block) {
        assert!(self.state.is_none());

        let ok_cont = self.fun.block_insert();
        for _ in 0..num {
            self.fun.block_arg_insert(ok_cont);
        }
        let ok_cont_val = self.value(ok_cont);
        let err_cont = self.fun.block_insert();
        let err_cont_val = self.value(err_cont);

        let data = self.fun.blocks.get_mut(block).unwrap();
        assert!(data.op.is_none());
        assert!(data.reads.is_empty());

        data.op = Some(OpKind::UnpackTuple(num));
        data.reads.push(ok_cont_val, &mut self.fun.value_pool);
        data.reads.push(err_cont_val, &mut self.fun.value_pool);
        data.reads.push(val, &mut self.fun.value_pool);

        self.graph_update_block(block);

        (ok_cont, err_cont)
    }

    pub fn op_unpack_list_cell(&mut self, block: Block, val: Value) -> (Block, Block) {
        assert!(self.state.is_none());

        let ok_cont = self.fun.block_insert();
        self.fun.block_arg_insert(ok_cont);
        self.fun.block_arg_insert(ok_cont);
        let ok_cont_val = self.value(ok_cont);
        let err_cont = self.fun.block_insert();
        let err_cont_val = self.value(err_cont);

        let data = self.fun.blocks.get_mut(block).unwrap();
        assert!(data.op.is_none());
        assert!(data.reads.is_empty());

        data.op = Some(OpKind::UnpackListCell);
        data.reads.push(ok_cont_val, &mut self.fun.value_pool);
        data.reads.push(err_cont_val, &mut self.fun.value_pool);
        data.reads.push(val, &mut self.fun.value_pool);

        self.graph_update_block(block);

        (ok_cont, err_cont)
    }

    pub fn op_unpack_map_item(&mut self, block: Block, map: Value, key: Value) -> (Block, Block) {
        assert!(self.state.is_none());

        let ok_cont = self.fun.block_insert();
        self.fun.block_arg_insert(ok_cont);
        let ok_cont_val = self.value(ok_cont);
        let err_cont = self.fun.block_insert();
        let err_cont_val = self.value(err_cont);

        let data = self.fun.blocks.get_mut(block).unwrap();
        assert!(data.op.is_none());
        assert!(data.reads.is_empty());

        data.op = Some(OpKind::UnpackMapItem);
        data.reads.push(ok_cont_val, &mut self.fun.value_pool);
        data.reads.push(err_cont_val, &mut self.fun.value_pool);
        data.reads.push(map, &mut self.fun.value_pool);
        data.reads.push(key, &mut self.fun.value_pool);

        self.graph_update_block(block);

        (ok_cont, err_cont)
    }

    pub fn op_unreachable(&mut self, block: Block) {
        assert!(self.state.is_none());

        let data = self.fun.blocks.get_mut(block).unwrap();
        assert!(data.op.is_none());
        assert!(data.reads.is_empty());

        data.op = Some(OpKind::Unreachable);

        self.graph_update_block(block);
    }

}

macro_rules! impl_simple_builder {
    ($name:ident, $op_kind:ident) => {
        pub struct $name {
            pub block: Option<Block>,
            next: Block,
            values: EntityList<Value>,
        }
        impl $name {

            pub fn new<'a>(b: &mut FunctionBuilder<'a>) -> Self {
                let next = b.block_insert();
                b.block_arg_insert(next);
                let next_val = b.value(next);

                let mut values = EntityList::new();
                values.push(next_val, &mut b.fun.value_pool);

                $name {
                    block: None,
                    next,
                    values,
                }
            }

            pub fn push_value<'a>(&mut self, val: Value, b: &mut FunctionBuilder<'a>) {
                self.values.push(val, &mut b.fun.value_pool);
            }

            pub fn finish<'a>(self, b: &mut FunctionBuilder<'a>) -> Block {
                let block = self.block.unwrap();
                let data = b.fun.blocks.get_mut(block).unwrap();
                assert!(data.op.is_none());
                assert!(data.reads.is_empty());

                data.op = Some(OpKind::$op_kind);
                data.reads = self.values;

                b.graph_update_block(block);

                self.next
            }

        }
    }
}

impl_simple_builder!(PackValueListBuilder, PackValueList);
impl_simple_builder!(MakeTupleBuilder, MakeTuple);

pub struct IntrinsicBuilder {
    name: Symbol,
    pub block: Option<Block>,
    values: EntityList<Value>,
}
impl IntrinsicBuilder {

    pub fn new<'a>(name: Symbol, b: &mut FunctionBuilder<'a>) -> Self {
        IntrinsicBuilder {
            name,
            block: None,
            values: EntityList::new(),
        }
    }

    pub fn push_value<'a, V>(&mut self, val: V, b: &mut FunctionBuilder<'a>) where V: IntoValue {
        let val_n = b.value(val);
        self.values.push(val_n, &mut b.fun.value_pool);
    }

    pub fn finish<'a>(self, b: &mut FunctionBuilder<'a>) {
        if self.values.len(&b.fun().value_pool) == 0 {
            panic!();
        }

        let block = self.block.unwrap();

        let data = b.fun.blocks.get_mut(block).unwrap();
        assert!(data.op.is_none());
        assert!(data.reads.is_empty());

        data.op = Some(OpKind::Intrinsic(self.name));
        data.reads = self.values;

        b.graph_update_block(block);
    }

}

pub struct CaseBuilder {
    pub match_on: Option<Value>,
    pub no_match: Option<Value>,

    clauses: EntityList<PatternClause>,
    clauses_b: EntityList<Value>,
    values: EntityList<Value>,
}
impl CaseBuilder {

    pub fn new() -> Self {
        CaseBuilder {
            match_on: None,
            no_match: None,

            clauses: EntityList::new(),
            clauses_b: EntityList::new(),
            values: EntityList::new(),
        }
    }

    pub fn push_clause<'a>(&mut self, clause: PatternClause, guard: Value, body: Value, b: &mut FunctionBuilder<'a>) {
        self.clauses.push(clause, &mut b.fun.clause_pool);
        self.clauses_b.extend([guard, body].iter().cloned(), &mut b.fun.value_pool);
    }

    pub fn push_value<'a>(&mut self, value: Value, b: &mut FunctionBuilder<'a>) {
        self.values.push(value, &mut b.fun.value_pool);
    }

    pub fn finish<'a>(mut self, block: Block, b: &mut FunctionBuilder<'a>) {
        assert!(b.state.is_none());

        // Validate that the number of values matches between the
        // clauses and reads
        let mut num_values = 0;
        for clause in self.clauses.as_slice(&b.fun().clause_pool) {
            num_values += b.fun().pat().clause_values(*clause).len();
        }
        let num_value_reads = self.values.len(&b.fun().value_pool);
        assert!(num_values == num_value_reads);

        let data = b.fun.blocks.get_mut(block).unwrap();
        assert!(data.op.is_none());
        assert!(data.reads.is_empty());

        data.op = Some(OpKind::Case {
            clauses: self.clauses,
        });

        let mut buf = b.value_buf.take().unwrap();
        buf.clear();

        data.reads.push(self.no_match.unwrap(), &mut b.fun.value_pool);

        // Guard and body blocks for clauses
        for c in self.clauses_b.as_slice(&b.fun.value_pool) {
            buf.push(*c);
        }
        data.reads.extend(buf.iter().cloned(), &mut b.fun.value_pool);

        // Match value
        data.reads.push(self.match_on.unwrap(), &mut b.fun.value_pool);

        // Values
        buf.clear();
        for c in self.values.as_slice(&b.fun.value_pool) {
            buf.push(*c);
        }
        data.reads.extend(buf.iter().cloned(), &mut b.fun.value_pool);

        buf.clear();
        b.value_buf = Some(buf);

        self.clauses_b.clear(&mut b.fun.value_pool);
        self.values.clear(&mut b.fun.value_pool);

        b.graph_update_block(block);
    }

}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::FunctionIdent;

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

