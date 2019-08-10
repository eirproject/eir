use libeir_intern::Symbol;

use cranelift_entity::{ EntityList };

use crate::{ Block, Value, PatternClause };
use crate::{ IntoValue };
use crate::{ OpKind, MatchKind, BasicType, MapPutUpdate };
use crate::binary::BinaryEntrySpecifier;
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

    pub fn op_trace_capture_raw(&mut self, block: Block) -> Block {
        assert!(self.state.is_none());

        let cont = self.fun.block_insert();
        let cont_val = self.value(cont);
        self.fun.block_arg_insert(cont);

        let data = self.fun.blocks.get_mut(block).unwrap();
        assert!(data.op.is_none());
        assert!(data.reads.is_empty());

        data.op = Some(OpKind::TraceCaptureRaw);
        data.reads.push(cont_val, &mut self.fun.pool.value);

        self.graph_update_block(block);
        cont
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

    pub fn op_map_put_build(&mut self, value: Value) -> MapPutBuilder {
        MapPutBuilder::new(value, self)
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

    pub fn op_binary_push(&mut self, block: Block, specifier: BinaryEntrySpecifier,
                          bin: Value, value: Value, size: Option<Value>) -> (Block, Block) {
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
        data.reads.push(bin, &mut self.fun.pool.value);
        data.reads.push(value, &mut self.fun.pool.value);
        if let Some(size) = size {
            data.reads.push(size, &mut self.fun.pool.value);
        }

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

    pub fn op_match_build(&mut self) -> MatchBuilder {
        MatchBuilder::new()
    }

}

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
        b.fun.graph_validate_block(block);
    }

}

pub struct MatchBuilder {
    branches: EntityList<Value>,
    branch_args: EntityList<Value>,

    kinds: Vec<MatchKind>,
}
impl Default for MatchBuilder {
    fn default() -> Self {
        MatchBuilder {
            branches: EntityList::new(),
            branch_args: EntityList::new(),

            kinds: Vec::new(),
        }
    }
}
impl MatchBuilder {

    pub fn new() -> Self {
        Self::default()
    }

    pub fn push_value(&mut self, val: Value, b: &mut FunctionBuilder) -> Block {
        let (block, block_val) = b.block_insert_get_val();

        self.kinds.push(MatchKind::Value);

        self.branches.push(block_val, &mut b.fun.pool.value);

        let args = b.prim_value_list(&[val]);
        self.branch_args.push(args, &mut b.fun.pool.value);

        block
    }

    pub fn push_type(&mut self, typ: BasicType, b: &mut FunctionBuilder) -> Block {
        let (block, block_val) = b.block_insert_get_val();

        self.kinds.push(MatchKind::Type(typ));

        self.branches.push(block_val, &mut b.fun.pool.value);

        let args = b.prim_value_list(&[]);
        self.branch_args.push(args, &mut b.fun.pool.value);

        block
    }

    pub fn push_tuple(&mut self, arity: usize, b: &mut FunctionBuilder) -> Block {
        let (block, block_val) = b.block_insert_get_val();
        for _ in 0..arity {
            b.block_arg_insert(block);
        }

        self.kinds.push(MatchKind::Tuple(arity));

        self.branches.push(block_val, &mut b.fun.pool.value);

        let args = b.prim_value_list(&[]);
        self.branch_args.push(args, &mut b.fun.pool.value);

        block
    }

    pub fn push_binary(&mut self, specifier: BinaryEntrySpecifier,
                       size: Option<Value>, b: &mut FunctionBuilder) -> Block {
        let (block, block_val) = b.block_insert_get_val();
        b.block_arg_insert(block);
        b.block_arg_insert(block);

        self.kinds.push(MatchKind::Binary(specifier));

        self.branches.push(block_val, &mut b.fun.pool.value);

        let args = if let Some(size) = size {
            b.prim_value_list(&[size])
        } else {
            b.prim_value_list(&[])
        };
        self.branch_args.push(args, &mut b.fun.pool.value);

        block
    }

    pub fn push_list_cell(&mut self, b: &mut FunctionBuilder) -> Block {
        let (block, block_val) = b.block_insert_get_val();
        b.block_arg_insert(block);
        b.block_arg_insert(block);

        self.kinds.push(MatchKind::ListCell);

        self.branches.push(block_val, &mut b.fun.pool.value);

        let args = b.prim_value_list(&[]);
        self.branch_args.push(args, &mut b.fun.pool.value);

        block
    }

    pub fn push_map_item(&mut self, key: Value, b: &mut FunctionBuilder) -> Block {
        let (block, block_val) = b.block_insert_get_val();
        b.block_arg_insert(block);

        self.kinds.push(MatchKind::MapItem);

        self.branches.push(block_val, &mut b.fun.pool.value);

        let args = b.prim_value_list(&[key]);
        self.branch_args.push(args, &mut b.fun.pool.value);

        block
    }

    pub fn push_wildcard(&mut self, b: &mut FunctionBuilder) -> Block {
        let (block, block_val) = b.block_insert_get_val();

        self.kinds.push(MatchKind::Wildcard);

        self.branches.push(block_val, &mut b.fun.pool.value);

        let args = b.prim_value_list(&[]);
        self.branch_args.push(args, &mut b.fun.pool.value);

        block
    }

    pub fn finish(self, block: Block, value: Value, b: &mut FunctionBuilder) {
        let branches = b.prim_value_list_from_entity_list(self.branches);

        let data = b.fun.blocks.get_mut(block).unwrap();
        assert!(data.op.is_none());
        assert!(data.reads.is_empty());

        let mut reads = EntityList::new();
        reads.push(branches, &mut b.fun.pool.value);
        reads.push(value, &mut b.fun.pool.value);

        let branches_num = self.branch_args.len(&b.fun.pool.value);
        for n in 0..branches_num {
            let branch_val = self.branch_args.get(n, &b.fun.pool.value).unwrap();
            reads.push(branch_val, &mut b.fun.pool.value);
        }

        data.op = Some(OpKind::Match {
            branches: self.kinds,
        });
        data.reads = reads;

        b.graph_update_block(block);
        b.fun.graph_validate_block(block);
    }

}

pub struct MapPutBuilder {
    ok: Block,
    fail: Block,
    reads: EntityList<Value>,
    actions: Vec<MapPutUpdate>,
}
impl MapPutBuilder {

    pub fn new(value: Value, b: &mut FunctionBuilder) -> Self {
        let (ok, ok_val) = b.block_insert_get_val();
        b.block_arg_insert(ok);

        let (fail, fail_val) = b.block_insert_get_val();
        b.block_arg_insert(fail);

        let mut reads = EntityList::new();
        reads.push(ok_val, &mut b.fun.pool.value);
        reads.push(fail_val, &mut b.fun.pool.value);

        reads.push(value, &mut b.fun.pool.value);

        MapPutBuilder {
            ok,
            fail,
            reads,
            actions: Vec::new(),
        }
    }

    pub fn push_kv(&mut self, key: Value, val: Value, action: MapPutUpdate,
                  b: &mut FunctionBuilder)
    {
        self.actions.push(action);

        self.reads.push(key, &mut b.fun.pool.value);
        self.reads.push(val, &mut b.fun.pool.value);
    }

    pub fn finish(self, block: Block, b: &mut FunctionBuilder) -> (Block, Block) {
        let data = b.fun.blocks.get_mut(block).unwrap();
        assert!(data.op.is_none());
        assert!(data.reads.is_empty());

        data.op = Some(OpKind::MapPut { action: self.actions });
        data.reads = self.reads;

        b.graph_update_block(block);

        (self.ok, self.fail)
    }

}

