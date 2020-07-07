use libeir_diagnostics::SourceSpan;

use cranelift_entity::EntityList;

use crate::binary::BinaryEntrySpecifier;
use crate::operation::{DynOp, OpBuild};
use crate::IntoValue;
use crate::{BasicType, CallKind, MapPutUpdate, MatchKind, OpKind};
use crate::{Block, PatternClause, Value};

use super::FunctionBuilder;

/// Operation constructors
impl<'a> FunctionBuilder<'a> {
    pub fn op_call_flow<'b, V>(&'b mut self, block: Block, target: V, args: &[Value])
    where
        V: IntoValue,
    {
        let target_val = self.value(target);

        let data = self.fun.blocks.get_mut(block).unwrap();
        assert!(data.op.is_none());
        assert!(data.reads.is_empty());

        data.op = Some(OpKind::Call(CallKind::ControlFlow));
        data.reads.push(target_val, &mut self.fun.pool.value);
        data.reads
            .extend(args.iter().cloned(), &mut self.fun.pool.value);

        self.graph_update_block(block);
    }

    pub fn op_call_function_next<'b, V>(
        &'b mut self,
        span: SourceSpan,
        block: Block,
        target: V,
        ret: Value,
        thr: Value,
        args: &[Value],
    ) where
        V: IntoValue,
    {
        let target_val = self.value(target);

        let data = self.fun.blocks.get_mut(block).unwrap();
        assert!(data.op.is_none());
        assert!(data.reads.is_empty());

        data.op = Some(OpKind::Call(CallKind::Function));
        data.reads.push(target_val, &mut self.fun.pool.value);
        data.reads.push(ret, &mut self.fun.pool.value);
        data.reads.push(thr, &mut self.fun.pool.value);
        data.reads
            .extend(args.iter().cloned(), &mut self.fun.pool.value);
        data.location = self.fun.locations.location(None, None, None, span);

        self.graph_update_block(block);
    }
    pub fn op_call_function<'b, V>(
        &'b mut self,
        span: SourceSpan,
        block: Block,
        target: V,
        args: &[Value],
    ) -> (Block, Block)
    where
        V: IntoValue,
    {
        let (ret, ret_val) = self.block_insert_get_val();
        self.block_arg_insert(ret);
        let (thr, thr_val) = self.block_insert_get_val();
        self.block_arg_insert(thr);
        self.block_arg_insert(thr);
        self.block_arg_insert(thr);

        self.op_call_function_next(span, block, target, ret_val, thr_val, args);

        (ret, thr)
    }

    pub fn op_trace_capture_raw_next(&mut self, span: SourceSpan, block: Block, next: Value) {
        let data = self.fun.blocks.get_mut(block).unwrap();
        assert!(data.op.is_none());
        assert!(data.reads.is_empty());

        data.op = Some(OpKind::TraceCaptureRaw);
        data.reads.push(next, &mut self.fun.pool.value);
        data.location = self.fun.locations.location(None, None, None, span);

        self.graph_update_block(block);
    }
    pub fn op_trace_capture_raw(&mut self, span: SourceSpan, block: Block) -> Block {
        let cont = self.fun.block_insert();
        let cont_val = self.value(cont);
        self.fun.block_arg_insert(cont);

        self.op_trace_capture_raw_next(span, block, cont_val);

        cont
    }

    pub fn op_intrinsic<'b, O: OpBuild>(
        &'b mut self,
        block: Block,
        op: O,
        args: &[Value],
        _token: O::Token,
    ) {
        assert!(self.fun.dialect.contains_op::<O>());

        let data = self.fun.blocks.get_mut(block).unwrap();
        assert!(data.op.is_none());
        assert!(data.reads.is_empty());

        let dyn_op = DynOp::new(op);
        data.op = Some(OpKind::Dyn(dyn_op));
        data.reads
            .extend(args.iter().cloned(), &mut self.fun.pool.value);

        self.graph_update_block(block);
    }

    pub fn op_map_put_build(&mut self, span: SourceSpan, value: Value) -> MapPutBuilder {
        MapPutBuilder::new(span, value, self)
    }

    pub fn op_unpack_value_list_next(
        &mut self,
        block: Block,
        target: Value,
        list: Value,
        num: usize,
    ) {
        let data = self.fun.blocks.get_mut(block).unwrap();
        assert!(data.op.is_none());
        assert!(data.reads.is_empty());

        data.op = Some(OpKind::UnpackValueList(num));
        data.reads.push(target, &mut self.fun.pool.value);
        data.reads.push(list, &mut self.fun.pool.value);

        self.graph_update_block(block);
    }
    pub fn op_unpack_value_list(&mut self, block: Block, list: Value, num: usize) -> Block {
        let cont = self.fun.block_insert();
        let cont_val = self.value(cont);
        for _ in 0..num {
            self.fun.block_arg_insert(cont);
        }
        self.op_unpack_value_list_next(block, cont_val, list, num);
        cont
    }

    pub fn op_if_bool_next(
        &mut self,
        span: SourceSpan,
        block: Block,
        t: Value,
        f: Value,
        o: Value,
        value: Value,
    ) {
        let data = self.fun.blocks.get_mut(block).unwrap();
        assert!(data.op.is_none());
        assert!(data.reads.is_empty());

        data.op = Some(OpKind::IfBool);
        data.reads.push(t, &mut self.fun.pool.value);
        data.reads.push(f, &mut self.fun.pool.value);
        data.reads.push(o, &mut self.fun.pool.value);
        data.reads.push(value, &mut self.fun.pool.value);
        data.location = self.fun.locations.location(None, None, None, span);

        self.graph_update_block(block);
    }
    pub fn op_if_bool(
        &mut self,
        span: SourceSpan,
        block: Block,
        value: Value,
    ) -> (Block, Block, Block) {
        let true_cont = self.fun.block_insert();
        let true_cont_val = self.value(true_cont);
        let false_cont = self.fun.block_insert();
        let false_cont_val = self.value(false_cont);
        let non_cont = self.fun.block_insert();
        let non_cont_val = self.value(non_cont);

        self.op_if_bool_next(
            span,
            block,
            true_cont_val,
            false_cont_val,
            non_cont_val,
            value,
        );

        (true_cont, false_cont, non_cont)
    }

    pub fn op_if_bool_strict_next(
        &mut self,
        span: SourceSpan,
        block: Block,
        t: Value,
        f: Value,
        value: Value,
    ) {
        let data = self.fun.blocks.get_mut(block).unwrap();
        assert!(data.op.is_none());
        assert!(data.reads.is_empty());

        data.op = Some(OpKind::IfBool);
        data.reads.push(t, &mut self.fun.pool.value);
        data.reads.push(f, &mut self.fun.pool.value);
        data.reads.push(value, &mut self.fun.pool.value);
        data.location = self.fun.locations.location(None, None, None, span);

        self.graph_update_block(block);
    }
    pub fn op_if_bool_strict(
        &mut self,
        span: SourceSpan,
        block: Block,
        value: Value,
    ) -> (Block, Block) {
        let true_cont = self.fun.block_insert();
        let true_cont_val = self.value(true_cont);
        let false_cont = self.fun.block_insert();
        let false_cont_val = self.value(false_cont);

        self.op_if_bool_strict_next(span, block, true_cont_val, false_cont_val, value);

        (true_cont, false_cont)
    }

    pub fn op_unreachable(&mut self, span: SourceSpan, block: Block) {
        let data = self.fun.blocks.get_mut(block).unwrap();
        assert!(data.op.is_none());
        assert!(data.reads.is_empty());

        data.op = Some(OpKind::Unreachable);
        data.location = self.fun.locations.location(None, None, None, span);

        self.graph_update_block(block);
    }

    pub fn op_match_build(&mut self, span: SourceSpan) -> MatchBuilder {
        MatchBuilder::new(span)
    }
}

pub struct MatchBuilder {
    span: SourceSpan,
    branches: EntityList<Value>,
    branch_args: EntityList<Value>,

    kinds: Vec<MatchKind>,
}
impl Default for MatchBuilder {
    fn default() -> Self {
        MatchBuilder {
            span: SourceSpan::UNKNOWN,
            branches: EntityList::new(),
            branch_args: EntityList::new(),

            kinds: Vec::new(),
        }
    }
}
impl MatchBuilder {
    pub fn new(span: SourceSpan) -> Self {
        let mut this = Self::default();
        this.span = span;
        this
    }

    pub fn push_value_next(&mut self, next: Value, val: Value, b: &mut FunctionBuilder) {
        self.kinds.push(MatchKind::Value);

        self.branches.push(next, &mut b.fun.pool.value);

        let args = b.prim_value_list(&[val]);
        self.branch_args.push(args, &mut b.fun.pool.value);
    }
    pub fn push_value(&mut self, val: Value, b: &mut FunctionBuilder) -> Block {
        let (block, block_val) = b.block_insert_get_val();
        self.push_value_next(block_val, val, b);
        block
    }

    pub fn push_type_next(&mut self, next: Value, typ: BasicType, b: &mut FunctionBuilder) {
        self.kinds.push(MatchKind::Type(typ));

        self.branches.push(next, &mut b.fun.pool.value);

        let args = b.prim_value_list(&[]);
        self.branch_args.push(args, &mut b.fun.pool.value);
    }
    pub fn push_type(&mut self, typ: BasicType, b: &mut FunctionBuilder) -> Block {
        let (block, block_val) = b.block_insert_get_val();
        self.push_type_next(block_val, typ, b);
        block
    }

    pub fn push_tuple_next(&mut self, next: Value, arity: usize, b: &mut FunctionBuilder) {
        self.kinds.push(MatchKind::Tuple(arity));

        self.branches.push(next, &mut b.fun.pool.value);

        let args = b.prim_value_list(&[]);
        self.branch_args.push(args, &mut b.fun.pool.value);
    }
    pub fn push_tuple(&mut self, arity: usize, b: &mut FunctionBuilder) -> Block {
        let (block, block_val) = b.block_insert_get_val();
        for _ in 0..arity {
            b.block_arg_insert(block);
        }

        self.push_tuple_next(block_val, arity, b);

        block
    }

    pub fn push_binary(
        &mut self,
        specifier: BinaryEntrySpecifier,
        size: Option<Value>,
        b: &mut FunctionBuilder,
    ) -> Block {
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

    pub fn push_list_cell_next(&mut self, next: Value, b: &mut FunctionBuilder) {
        self.kinds.push(MatchKind::ListCell);

        self.branches.push(next, &mut b.fun.pool.value);

        let args = b.prim_value_list(&[]);
        self.branch_args.push(args, &mut b.fun.pool.value);
    }
    pub fn push_list_cell(&mut self, b: &mut FunctionBuilder) -> Block {
        let (block, block_val) = b.block_insert_get_val();
        b.block_arg_insert(block);
        b.block_arg_insert(block);

        self.push_list_cell_next(block_val, b);

        block
    }

    pub fn push_map_item_next(&mut self, next: Value, key: Value, b: &mut FunctionBuilder) {
        self.kinds.push(MatchKind::MapItem);

        self.branches.push(next, &mut b.fun.pool.value);

        let args = b.prim_value_list(&[key]);
        self.branch_args.push(args, &mut b.fun.pool.value);
    }
    pub fn push_map_item(&mut self, key: Value, b: &mut FunctionBuilder) -> Block {
        let (block, block_val) = b.block_insert_get_val();
        b.block_arg_insert(block);

        self.push_map_item_next(block_val, key, b);

        block
    }

    pub fn push_wildcard_next(&mut self, next: Value, b: &mut FunctionBuilder) {
        self.kinds.push(MatchKind::Wildcard);

        self.branches.push(next, &mut b.fun.pool.value);

        let args = b.prim_value_list(&[]);
        self.branch_args.push(args, &mut b.fun.pool.value);
    }
    pub fn push_wildcard(&mut self, span: SourceSpan, b: &mut FunctionBuilder) -> Block {
        let (block, block_val) = b.block_insert_get_val();
        {
            let mut block_data = b.fun.blocks.get_mut(block).unwrap();
            block_data.location = b.fun.locations.location(None, None, None, span);
        }
        self.push_wildcard_next(block_val, b);
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
        data.location = b.fun.locations.location(None, None, None, self.span);

        b.graph_update_block(block);
        b.fun.graph_validate_block(block);
    }
}

pub struct MapPutBuilder {
    span: SourceSpan,
    ok: Block,
    fail: Block,
    reads: EntityList<Value>,
    actions: Vec<MapPutUpdate>,
}
impl MapPutBuilder {
    pub fn new(span: SourceSpan, value: Value, b: &mut FunctionBuilder) -> Self {
        let (ok, ok_val) = b.block_insert_get_val();
        b.block_arg_insert(ok);

        let (fail, fail_val) = b.block_insert_get_val();
        b.block_arg_insert(fail);

        let mut reads = EntityList::new();
        reads.push(ok_val, &mut b.fun.pool.value);
        reads.push(fail_val, &mut b.fun.pool.value);

        reads.push(value, &mut b.fun.pool.value);

        MapPutBuilder {
            span,
            ok,
            fail,
            reads,
            actions: Vec::new(),
        }
    }

    pub fn push_kv(
        &mut self,
        key: Value,
        val: Value,
        action: MapPutUpdate,
        b: &mut FunctionBuilder,
    ) {
        self.actions.push(action);

        self.reads.push(key, &mut b.fun.pool.value);
        self.reads.push(val, &mut b.fun.pool.value);
    }

    pub fn finish(self, block: Block, b: &mut FunctionBuilder) -> (Block, Block) {
        let data = b.fun.blocks.get_mut(block).unwrap();
        assert!(data.op.is_none());
        assert!(data.reads.is_empty());

        data.op = Some(OpKind::MapPut {
            action: self.actions,
        });
        data.reads = self.reads;
        data.location = b.fun.locations.location(None, None, None, self.span);

        b.graph_update_block(block);

        (self.ok, self.fail)
    }
}
