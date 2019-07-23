use libeir_intern::Symbol;

use cranelift_entity::{ EntityList };

use crate::{ Block, Value, PatternClause };
use crate::{ IntoValue };
use crate::{ OpKind, MapPutUpdate };
use crate::binary::EntrySpecifier as BinaryEntrySpecifier;
use super::{ BuilderState };
use super::{ FunctionBuilder };

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
        data.reads.push(target_val, &mut self.fun.pool.value);
        data.reads.extend(args.iter().cloned(), &mut self.fun.pool.value);

        self.graph_update_block(block);
    }

    pub fn op_intrinsic<'b>(&'b mut self, block: Block, name: Symbol, args: &[Value]) {
        assert!(self.state.is_none());

        let data = self.fun.blocks.get_mut(block).unwrap();
        assert!(data.op.is_none());
        assert!(data.reads.is_empty());

        data.op = Some(OpKind::Intrinsic(name));
        data.reads.extend(args.iter().cloned(), &mut self.fun.pool.value);

        self.graph_update_block(block);
    }
    pub fn op_intrinsic_build(&mut self, name: Symbol) -> IntrinsicBuilder {
        IntrinsicBuilder::new(name, self)
    }

    pub fn op_capture_function<M, F, A>(
        &mut self, block: Block,
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
        data.reads.push(cont_val, &mut self.fun.pool.value);
        data.reads.push(m_val, &mut self.fun.pool.value);
        data.reads.push(f_val, &mut self.fun.pool.value);
        data.reads.push(a_val, &mut self.fun.pool.value);

        self.graph_update_block(block);

        cont
    }

    pub fn op_start_map_put(&mut self, block: Block, map: Value) -> Block {
        assert!(self.state.is_none());
        self.state = Some(BuilderState::MapPut {
            block,
            action: Vec::new(),
        });

        let cont = self.fun.block_insert();
        let cont_val = self.value(cont);
        self.fun.block_arg_insert(cont);

        let data = self.fun.blocks.get_mut(block).unwrap();
        assert!(data.op.is_none());
        assert!(data.reads.is_empty());

        data.reads.push(cont_val, &mut self.fun.pool.value);
        data.reads.push(map, &mut self.fun.pool.value);

        cont
    }

    pub fn map_put_kv(&mut self, key: Value, val: Value, typ: MapPutUpdate) {
        if let Some(BuilderState::MapPut { block, ref mut action }) = self.state {
            let data = self.fun.blocks.get_mut(block).unwrap();
            data.reads.push(key, &mut self.fun.pool.value);
            data.reads.push(val, &mut self.fun.pool.value);
            action.push(typ);
        } else {
            panic!()
        }
    }

    #[allow(clippy::single_match, irrefutable_let_patterns)]
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
        data.reads.push(cont_val, &mut self.fun.pool.value);
        data.reads.push(list, &mut self.fun.pool.value);

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
        data.reads.push(true_cont_val, &mut self.fun.pool.value);
        data.reads.push(false_cont_val, &mut self.fun.pool.value);
        data.reads.push(non_cont_val, &mut self.fun.pool.value);
        data.reads.push(value, &mut self.fun.pool.value);

        self.graph_update_block(block);

        (true_cont, false_cont, non_cont)
    }

    pub fn op_if_bool_strict(&mut self, block: Block, value: Value) -> (Block, Block) {
        assert!(self.state.is_none());

        let true_cont = self.fun.block_insert();
        let true_cont_val = self.value(true_cont);
        let false_cont = self.fun.block_insert();
        let false_cont_val = self.value(false_cont);

        let data = self.fun.blocks.get_mut(block).unwrap();
        assert!(data.op.is_none());
        assert!(data.reads.is_empty());

        data.op = Some(OpKind::IfBool);
        data.reads.push(true_cont_val, &mut self.fun.pool.value);
        data.reads.push(false_cont_val, &mut self.fun.pool.value);
        data.reads.push(value, &mut self.fun.pool.value);

        self.graph_update_block(block);

        (true_cont, false_cont)
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
        data.reads.push(ok_cont_val, &mut self.fun.pool.value);
        data.reads.push(err_cont_val, &mut self.fun.pool.value);
        data.reads.push(val, &mut self.fun.pool.value);

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
        data.reads.push(ok_cont_val, &mut self.fun.pool.value);
        data.reads.push(err_cont_val, &mut self.fun.pool.value);
        data.reads.push(val, &mut self.fun.pool.value);

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
        data.reads.push(ok_cont_val, &mut self.fun.pool.value);
        data.reads.push(err_cont_val, &mut self.fun.pool.value);
        data.reads.push(map, &mut self.fun.pool.value);
        data.reads.push(key, &mut self.fun.pool.value);

        self.graph_update_block(block);

        (ok_cont, err_cont)
    }

    pub fn op_binary_push(&mut self, block: Block, specifier: BinaryEntrySpecifier,
                          value: Value, size: Option<Value>) -> (Block, Block) {
        if size.is_some() {
            assert!(specifier.has_size());
        }
        assert!(self.state.is_none());

        let ok_cont = self.fun.block_insert();
        self.fun.block_arg_insert(ok_cont);
        let ok_cont_val = self.value(ok_cont);
        let err_cont = self.fun.block_insert();
        let err_cont_val = self.value(err_cont);

        let data = self.fun.blocks.get_mut(block).unwrap();
        assert!(data.op.is_none());
        assert!(data.reads.is_empty());

        data.op = Some(OpKind::BinaryPush {
            specifier,
        });
        data.reads.push(ok_cont_val, &mut self.fun.pool.value);
        data.reads.push(err_cont_val, &mut self.fun.pool.value);
        data.reads.push(value, &mut self.fun.pool.value);
        if let Some(size) = size {
            data.reads.push(size, &mut self.fun.pool.value);
        }

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

//macro_rules! impl_simple_builder {
//    ($name:ident, $op_kind:ident) => {
//        pub struct $name {
//            pub block: Option<Block>,
//            next: Block,
//            values: EntityList<Value>,
//        }
//        impl $name {
//
//            pub fn new<'a>(b: &mut FunctionBuilder<'a>) -> Self {
//                let next = b.block_insert();
//                b.block_arg_insert(next);
//                let next_val = b.value(next);
//
//                let mut values = EntityList::new();
//                values.push(next_val, &mut b.fun.pool.value);
//
//                $name {
//                    block: None,
//                    next,
//                    values,
//                }
//            }
//
//            pub fn push_value<'a>(&mut self, val: Value, b: &mut FunctionBuilder<'a>) {
//                self.values.push(val, &mut b.fun.pool.value);
//            }
//
//            pub fn finish<'a>(self, b: &mut FunctionBuilder<'a>) -> Block {
//                let block = self.block.unwrap();
//                let data = b.fun.blocks.get_mut(block).unwrap();
//                assert!(data.op.is_none());
//                assert!(data.reads.is_empty());
//
//                data.op = Some(OpKind::$op_kind);
//                data.reads = self.values;
//
//                b.graph_update_block(block);
//
//                self.next
//            }
//
//        }
//    }
//}

//impl_simple_builder!(PackValueListBuilder, PackValueList);
//impl_simple_builder!(MakeTupleBuilder, MakeTuple);

pub struct IntrinsicBuilder {
    name: Symbol,
    pub block: Option<Block>,
    values: EntityList<Value>,
}
impl IntrinsicBuilder {

    pub fn new<'a>(name: Symbol, _b: &mut FunctionBuilder<'a>) -> Self {
        IntrinsicBuilder {
            name,
            block: None,
            values: EntityList::new(),
        }
    }

    pub fn push_value<'a, V>(&mut self, val: V, b: &mut FunctionBuilder<'a>) where V: IntoValue {
        let val_n = b.value(val);
        self.values.push(val_n, &mut b.fun.pool.value);
    }

    pub fn finish<'a>(self, b: &mut FunctionBuilder<'a>) {
        if self.values.len(&b.fun().pool.value) == 0 {
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

impl Default for CaseBuilder {
    fn default() -> Self {
        CaseBuilder {
            match_on: None,
            no_match: None,

            clauses: EntityList::new(),
            clauses_b: EntityList::new(),
            values: EntityList::new(),
        }
    }
}

impl CaseBuilder {

    pub fn new() -> Self {
        Self::default()
    }

    pub fn push_clause<'a>(&mut self, clause: PatternClause, guard: Value, body: Value, b: &mut FunctionBuilder<'a>) {
        self.clauses.push(clause, &mut b.fun.pool.clause);
        self.clauses_b.extend([guard, body].iter().cloned(), &mut b.fun.pool.value);
    }

    pub fn push_value<'a>(&mut self, value: Value, b: &mut FunctionBuilder<'a>) {
        self.values.push(value, &mut b.fun.pool.value);
    }

    pub fn finish<'a>(mut self, block: Block, b: &mut FunctionBuilder<'a>) {
        assert!(b.state.is_none());

        // Validate that the number of values matches between the
        // clauses and reads
        let mut num_values = 0;
        for clause in self.clauses.as_slice(&b.fun().pool.clause) {
            num_values += b.fun().pat().clause_values(*clause).len();
        }
        let num_value_reads = self.values.len(&b.fun().pool.value);
        assert!(num_values == num_value_reads);

        let data = b.fun.blocks.get_mut(block).unwrap();
        assert!(data.op.is_none());
        assert!(data.reads.is_empty());

        data.op = Some(OpKind::Case {
            clauses: self.clauses,
        });

        let mut buf = b.value_buf.take().unwrap();
        buf.clear();

        data.reads.push(self.no_match.unwrap(), &mut b.fun.pool.value);

        // Guard and body blocks for clauses
        for c in self.clauses_b.as_slice(&b.fun.pool.value) {
            buf.push(*c);
        }
        data.reads.extend(buf.iter().cloned(), &mut b.fun.pool.value);

        // Match value
        data.reads.push(self.match_on.unwrap(), &mut b.fun.pool.value);

        // Values
        buf.clear();
        for c in self.values.as_slice(&b.fun.pool.value) {
            buf.push(*c);
        }
        data.reads.extend(buf.iter().cloned(), &mut b.fun.pool.value);

        buf.clear();
        b.value_buf = Some(buf);

        self.clauses_b.clear(&mut b.fun.pool.value);
        self.values.clear(&mut b.fun.pool.value);

        b.graph_update_block(block);
    }

}
