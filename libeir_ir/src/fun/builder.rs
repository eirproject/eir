use super::{ Block, Value };
use super::{ ValueData, ValueType };
use super::{ Function };

use crate::{ ConstantTerm, AtomicTerm };
use crate::op::{ OpKind, ComparisonOperation, TestOperation, MapPutUpdate };
use crate::pattern::{ PatternClause };

use ::cranelift_entity::{ EntityList };

use libeir_diagnostics::{ DUMMY_SPAN };
use libeir_intern::{ Symbol, Ident };
use libeir_util::pooled_entity_set::{ PooledEntitySet };

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

}

/// Values
impl<'a> FunctionBuilder<'a> {

    pub fn value_block(&mut self, block: Block) -> Value {
        self.fun.values.push(ValueData {
            kind: ValueType::Block(block),
            // TODO: Update usages in builder api
            usages: PooledEntitySet::new(),

            span: DUMMY_SPAN,
        })
    }

    pub fn value_const(&mut self, constant: ConstantTerm) -> Value {
        // TODO: Dedup atomic at creation time
        self.fun.values.push(ValueData {
            kind: ValueType::Constant(constant),
            usages: PooledEntitySet::new(),

            span: DUMMY_SPAN,
        })
    }

    pub fn value_atomic(&mut self, atomic: AtomicTerm) -> Value {
        self.value_const(ConstantTerm::Atomic(atomic))
    }

}

/// Block modifiers
impl<'a> FunctionBuilder<'a> {

    pub fn block_insert(&mut self) -> Block {
        self.fun.block_insert()
    }

    pub fn block_arg_insert(&mut self, block: Block) -> Value {
        self.fun.block_arg_insert(block)
    }

    pub fn block_args<'b>(&'b self, block: Block) -> &'b [Value] {
        self.fun.block_args(block)
    }

    /// This will explicitly clear the operation contained in the
    /// block. This will remove all successors, and will cause
    /// this block to be removed from their predecessors.
    pub fn block_clear(&mut self, block: Block) {
        let mut buf = self.value_buf.take().unwrap();
        debug_assert!(buf.len() == 0);

        {
            let data = self.fun.blocks.get_mut(block).unwrap();
            data.op = None;

            for read in data.reads.as_slice(&self.fun.value_pool) {
                buf.push(*read);
            }
            data.reads = EntityList::new();
        }

        for value in buf.iter() {
            match self.fun.values[*value].kind {
                ValueType::Block(block) => {
                    let data = self.fun.blocks.get_mut(block).unwrap();
                    assert!(data.predecessors
                            .remove(block, &mut self.fun.block_set_pool));
                }
                ValueType::Alias(_) => panic!(),
                _ => {},
            }
            assert!(self.fun.values[*value].usages
                    .remove(block, &mut self.fun.block_set_pool));
        }

        buf.clear();
        self.value_buf = Some(buf);
    }

}

//struct CallBuilder<'a, 'b> {
//    builder: &'b mut FunctionBuilder<'a>,
//    block: Block,
//}

/// Operation constructors
impl<'a> FunctionBuilder<'a> {

    pub fn op_call<'b>(&'b mut self, block: Block,
                             target: Value, args: &[Value]) {
        assert!(self.state.is_none());

        let data = self.fun.blocks.get_mut(block).unwrap();
        assert!(data.op.is_none());
        assert!(data.reads.is_empty());

        data.op = Some(OpKind::Call);
        data.reads.push(target, &mut self.fun.value_pool);
        data.reads.extend(args.iter().cloned(), &mut self.fun.value_pool);
    }

    pub fn op_intrinsic<'b>(&'b mut self, block: Block, name: Symbol, args: &[Value]) {
        assert!(self.state.is_none());

        let data = self.fun.blocks.get_mut(block).unwrap();
        assert!(data.op.is_none());
        assert!(data.reads.is_empty());

        data.op = Some(OpKind::Intrinsic(name));
        data.reads.extend(args.iter().cloned(), &mut self.fun.value_pool);
    }

    pub fn op_capture_function<'b>(&'b mut self, block: Block,
                                   m: Value, f: Value, a: Value) -> Block {
        assert!(self.state.is_none());

        let cont = self.fun.block_insert();
        let cont_val = self.value_block(cont);
        self.fun.block_arg_insert(cont);

        let data = self.fun.blocks.get_mut(block).unwrap();
        assert!(data.op.is_none());
        assert!(data.reads.is_empty());

        data.op = Some(OpKind::CaptureFunction);
        data.reads.push(cont_val, &mut self.fun.value_pool);
        data.reads.push(m, &mut self.fun.value_pool);
        data.reads.push(f, &mut self.fun.value_pool);
        data.reads.push(a, &mut self.fun.value_pool);

        cont
    }

    pub fn op_make_tuple(&mut self, block: Block, values: &[Value]) -> Block {
        assert!(self.state.is_none());

        let cont = self.fun.block_insert();
        let cont_val = self.value_block(cont);
        self.fun.block_arg_insert(cont);

        let data = self.fun.blocks.get_mut(block).unwrap();
        assert!(data.op.is_none());
        assert!(data.reads.is_empty());

        data.op = Some(OpKind::MakeTuple);
        data.reads.push(cont_val, &mut self.fun.value_pool);
        data.reads.extend(values.iter().cloned(), &mut self.fun.value_pool);

        cont
    }

    pub fn op_make_list(&mut self, block: Block, values: &[Value]) -> Block {
        assert!(self.state.is_none());

        let cont = self.fun.block_insert();
        let cont_val = self.value_block(cont);
        self.fun.block_arg_insert(cont);

        let data = self.fun.blocks.get_mut(block).unwrap();
        assert!(data.op.is_none());
        assert!(data.reads.is_empty());

        data.op = Some(OpKind::MakeList);
        data.reads.push(cont_val, &mut self.fun.value_pool);
        data.reads.extend(values.iter().cloned(), &mut self.fun.value_pool);

        cont
    }

    pub fn op_make_map_empty(&mut self, block: Block) -> Block {
        assert!(self.state.is_none());

        let cont = self.fun.block_insert();
        let cont_val = self.value_block(cont);
        self.fun.block_arg_insert(cont);

        let data = self.fun.blocks.get_mut(block).unwrap();
        assert!(data.op.is_none());
        assert!(data.reads.is_empty());

        data.op = Some(OpKind::MapEmpty);
        data.reads.push(cont_val, &mut self.fun.value_pool);

        cont
    }

    pub fn op_start_map_put(&mut self, block: Block, map: Value) -> Block {
        assert!(self.state.is_none());
        self.state = Some(BuilderState::MapPut {
            block: block,
            action: Vec::new(),
        });

        let cont = self.fun.block_insert();
        let cont_val = self.value_block(cont);
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
        } else {
            panic!()
        }
    }

    pub fn op_pack_value_list(&mut self, block: Block, values: &[Value]) -> Block {
        assert!(self.state.is_none());

        let cont = self.fun.block_insert();
        let cont_val = self.value_block(cont);
        self.fun.block_arg_insert(cont);

        let data = self.fun.blocks.get_mut(block).unwrap();
        assert!(data.op.is_none());
        assert!(data.reads.is_empty());

        data.op = Some(OpKind::PackValueList);
        data.reads.push(cont_val, &mut self.fun.value_pool);
        data.reads.extend(values.iter().cloned(), &mut self.fun.value_pool);

        cont
    }

    pub fn op_unpack_value_list(&mut self, block: Block, list: Value, num: usize) -> Block {
        assert!(self.state.is_none());

        let cont = self.fun.block_insert();
        let cont_val = self.value_block(cont);
        for _ in 0..num {
            self.fun.block_arg_insert(cont);
        }

        let data = self.fun.blocks.get_mut(block).unwrap();
        assert!(data.op.is_none());
        assert!(data.reads.is_empty());

        data.op = Some(OpKind::UnpackValueList);
        data.reads.push(cont_val, &mut self.fun.value_pool);
        data.reads.push(list, &mut self.fun.value_pool);

        cont
    }

    pub fn op_compare(&mut self, block: Block, op: ComparisonOperation, lhs: Value, rhs: Value) -> (Block, Block) {
        assert!(self.state.is_none());

        let ok_cont = self.fun.block_insert();
        let ok_cont_val = self.value_block(ok_cont);
        let err_cont = self.fun.block_insert();
        let err_cont_val = self.value_block(err_cont);

        let data = self.fun.blocks.get_mut(block).unwrap();
        assert!(data.op.is_none());
        assert!(data.reads.is_empty());

        data.op = Some(OpKind::Compare(op));
        data.reads.push(ok_cont_val, &mut self.fun.value_pool);
        data.reads.push(err_cont_val, &mut self.fun.value_pool);
        data.reads.push(lhs, &mut self.fun.value_pool);
        data.reads.push(rhs, &mut self.fun.value_pool);

        (ok_cont, err_cont)
    }

    pub fn op_test(&mut self, block: Block, op: TestOperation, val: Value) -> (Block, Block) {
        assert!(self.state.is_none());

        let ok_cont = self.fun.block_insert();
        let ok_cont_val = self.value_block(ok_cont);
        let err_cont = self.fun.block_insert();
        let err_cont_val = self.value_block(err_cont);

        let data = self.fun.blocks.get_mut(block).unwrap();
        assert!(data.op.is_none());
        assert!(data.reads.is_empty());

        data.op = Some(OpKind::Test(op));
        data.reads.push(ok_cont_val, &mut self.fun.value_pool);
        data.reads.push(err_cont_val, &mut self.fun.value_pool);
        data.reads.push(val, &mut self.fun.value_pool);

        (ok_cont, err_cont)
    }

    pub fn op_unpack_tuple(&mut self, block: Block, val: Value, num: usize) -> (Block, Block) {
        assert!(self.state.is_none());

        let ok_cont = self.fun.block_insert();
        for _ in 0..num {
            self.fun.block_arg_insert(ok_cont);
        }
        let ok_cont_val = self.value_block(ok_cont);
        let err_cont = self.fun.block_insert();
        let err_cont_val = self.value_block(err_cont);

        let data = self.fun.blocks.get_mut(block).unwrap();
        assert!(data.op.is_none());
        assert!(data.reads.is_empty());

        data.op = Some(OpKind::UnpackTuple(num));
        data.reads.push(ok_cont_val, &mut self.fun.value_pool);
        data.reads.push(err_cont_val, &mut self.fun.value_pool);
        data.reads.push(val, &mut self.fun.value_pool);

        (ok_cont, err_cont)
    }

    pub fn op_unpack_list_cell(&mut self, block: Block, val: Value) -> (Block, Block) {
        assert!(self.state.is_none());

        let ok_cont = self.fun.block_insert();
        self.fun.block_arg_insert(ok_cont);
        self.fun.block_arg_insert(ok_cont);
        let ok_cont_val = self.value_block(ok_cont);
        let err_cont = self.fun.block_insert();
        let err_cont_val = self.value_block(err_cont);

        let data = self.fun.blocks.get_mut(block).unwrap();
        assert!(data.op.is_none());
        assert!(data.reads.is_empty());

        data.op = Some(OpKind::UnpackListCell);
        data.reads.push(ok_cont_val, &mut self.fun.value_pool);
        data.reads.push(err_cont_val, &mut self.fun.value_pool);
        data.reads.push(val, &mut self.fun.value_pool);

        (ok_cont, err_cont)
    }

    pub fn op_unpack_map_item(&mut self, block: Block, map: Value, key: Value) -> (Block, Block) {
        assert!(self.state.is_none());

        let ok_cont = self.fun.block_insert();
        self.fun.block_arg_insert(ok_cont);
        let ok_cont_val = self.value_block(ok_cont);
        let err_cont = self.fun.block_insert();
        let err_cont_val = self.value_block(err_cont);

        let data = self.fun.blocks.get_mut(block).unwrap();
        assert!(data.op.is_none());
        assert!(data.reads.is_empty());

        data.op = Some(OpKind::UnpackMapItem);
        data.reads.push(ok_cont_val, &mut self.fun.value_pool);
        data.reads.push(err_cont_val, &mut self.fun.value_pool);
        data.reads.push(map, &mut self.fun.value_pool);
        data.reads.push(key, &mut self.fun.value_pool);

        (ok_cont, err_cont)
    }

    pub fn op_unreachable(&mut self, block: Block) {
        assert!(self.state.is_none());

        let data = self.fun.blocks.get_mut(block).unwrap();
        assert!(data.op.is_none());
        assert!(data.reads.is_empty());

        data.op = Some(OpKind::Unreachable);
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

        let data = b.fun.blocks.get_mut(block).unwrap();
        assert!(data.op.is_none());
        assert!(data.reads.is_empty());

        data.op = Some(OpKind::Case {
            clauses: self.clauses,
        });

        let mut buf = b.value_buf.take().unwrap();
        buf.clear();

        data.reads.push(self.no_match.unwrap(), &mut b.fun.value_pool);

        for c in self.clauses_b.as_slice(&b.fun.value_pool) {
            buf.push(*c);
        }
        data.reads.extend(buf.iter().cloned(), &mut b.fun.value_pool);

        data.reads.push(self.match_on.unwrap(), &mut b.fun.value_pool);

        buf.clear();
        for c in self.values.as_slice(&b.fun.value_pool) {
            buf.push(*c);
        }
        data.reads.extend(buf.iter().cloned(), &mut b.fun.value_pool);

        buf.clear();
        b.value_buf = Some(buf);

        self.clauses_b.clear(&mut b.fun.value_pool);
        self.values.clear(&mut b.fun.value_pool);
    }

}



