use super::{ Ebb, Op, Value, EbbCall };
use super::{ OpData, EbbData, EbbCallData };
use super::{ ValueType, WriteToken };
use super::{ Function };
use super::{ AttributeKey, AttributeValue };

use crate::{ FunctionIdent, ConstantTerm, AtomicTerm, ClosureEnv };
use crate::Clause;
use crate::op::{ OpKind, ComparisonOperation, CallType };

use matches::assert_matches;
use ::cranelift_entity::{ EntityList };

#[derive(Copy, Clone, Debug)]
pub struct BuilderPosition(Option<Ebb>, Option<Op>);

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
enum BuilderState {
    Build,
    BuildingOp(Op),
    OutstandingEbbCalls(usize),
}
impl BuilderState {
    fn building_op(&self) -> Op {
        match self {
            BuilderState::BuildingOp(op) => *op,
            _ => panic!("Builder not in BuildingOp state"),
        }
    }
}

pub struct FunctionBuilder<'a> {
    fun: &'a mut Function,

    current_ebb: Option<Ebb>,
    current_op: Option<Op>,

    state: BuilderState,

    val_buf: Option<Vec<Value>>,
}

// Manual operation builder interface
impl<'a> FunctionBuilder<'a> {

    pub fn op_build_start(&mut self, kind: OpKind) {
        let op = self.insert_op(OpData {
            kind: kind,
            reads: EntityList::new(),
            writes: EntityList::new(),
            ebb_calls: EntityList::new(),
        });
        self.state = BuilderState::BuildingOp(op);
    }

    pub fn op_build_write(&mut self) -> Value {
        let op = self.state.building_op();
        let result = self.fun.new_variable();
        self.fun.ops[op].writes.push(result, &mut self.fun.value_pool);
        result
    }

    pub fn op_build_read(&mut self, val: Value) {
        let op = self.state.building_op();
        self.fun.ops[op].reads.push(val, &mut self.fun.value_pool);
    }

    pub fn op_build_ebb_call(&mut self, call: EbbCall) {
        let op = self.state.building_op();
        self.fun.ops[op].ebb_calls.push(call, &mut self.fun.ebb_call_pool);
    }

    pub fn op_build_end(&mut self) {
        // TODO validate op
        assert_matches!(self.state, BuilderState::BuildingOp(_));
        self.state = BuilderState::Build;
    }

}

impl<'a> FunctionBuilder<'a> {

    pub fn new(fun: &'a mut Function) -> FunctionBuilder<'a> {
        FunctionBuilder {
            fun: fun,

            current_ebb: None,
            current_op: None,

            state: BuilderState::Build,

            val_buf: Some(Vec::new()),
        }
    }

    pub fn function<'b>(&'b self) -> &'b Function {
        self.fun
    }
    pub fn function_mut<'b>(&'b mut self) -> &'b mut Function {
        self.fun
    }

    pub fn put_attribute(&mut self, key: AttributeKey, val: AttributeValue) {
        self.fun.attributes.insert(key, val);
    }

    pub fn gen_variables(&mut self, num: usize, args: &mut Vec<Value>) {
        args.clear();
        for _ in 0..num {
            args.push(self.fun.new_variable());
        }
    }

    fn insert_op(&mut self, data: OpData) -> Op {
        assert!(self.state == BuilderState::Build);

        // Must be in a block
        assert!(self.current_ebb.is_some());
        assert!(!self.fun.ebbs[self.current_ebb.unwrap()].finished);

        // If we are not at the beginning, the last Op can't be a unconditional Jump
        self.assert_not_terminated();

        let op = self.fun.ops.push(data);
        self.fun.layout.insert_op_after(
            self.current_ebb.unwrap(), self.current_op, op);

        for branch in self.fun.ops[op].ebb_calls.as_slice(&self.fun.ebb_call_pool) {
            assert!(self.fun.ebb_calls[*branch].source.is_none());
            self.fun.ebb_calls[*branch].source = Some(op);
        }

        self.current_op = Some(op);

        op
    }

    pub fn remove_op(&mut self, op: Op) {
        assert!(self.state == BuilderState::Build);
        if self.current_op == Some(op) {
            self.current_op = self.fun.layout.ops[op].prev;
        }

        // Enable reuse of branches
        for branch in self.fun.ops[op].ebb_calls.as_slice(&self.fun.ebb_call_pool) {
            self.fun.ebb_calls[*branch].source = None;
        }

        self.fun.layout.remove_op(op);
    }

    pub fn assert_not_terminated(&self) {
        if let Some(op) = self.current_op {
            assert!(!self.fun.ops[op].kind.is_block_terminator());
        }
    }

    pub fn ebb_concat(&mut self, next: Ebb, args: &[Value]) {
        assert!(self.state == BuilderState::Build);
        self.assert_at_end();
        self.assert_not_terminated();

        let current = self.current_ebb.unwrap();

        let mut val_buf = self.val_buf.take().unwrap();
        val_buf.clear();
        val_buf.extend(self.fun.ebb_args(next).iter().cloned());

        assert!(val_buf.len() == args.len());

        for (src, dest) in args.iter().zip(val_buf.iter()) {
            self.op_move_internal(*src, *dest);
        }

        self.val_buf = Some(val_buf);

        self.fun.layout.concat_ebb(current, next);
    }

    pub fn ebb_split(&mut self) -> Ebb {
        assert!(self.state == BuilderState::Build);
        let op = self.current_op.unwrap();
        assert!(!self.fun.op_kind(op).is_block_terminator());

        let new_ebb = self.insert_ebb();
        self.fun.layout.split_ebb_into(op, new_ebb);

        new_ebb
    }

    /// Can only be called when there are no blocks in the function
    pub fn insert_ebb_entry(&mut self) -> Ebb {
        let ebb = self.fun.ebbs.push(EbbData {
            arguments: EntityList::new(),
            finished: false,
        });
        self.fun.layout.insert_ebb_first(ebb);
        ebb
    }

    pub fn insert_ebb(&mut self) -> Ebb {
        let ebb = self.fun.ebbs.push(EbbData {
            arguments: EntityList::new(),
            finished: false,
        });
        self.fun.layout.insert_ebb_after(self.current_ebb.unwrap(), ebb);
        ebb
    }

    pub fn finish_ebb(&mut self, ebb: Ebb) {
        self.fun.ebbs[ebb].finished = true;
    }

    pub fn add_ebb_argument(&mut self, ebb: Ebb) -> Value {
        assert!(!self.fun.ebbs[ebb].finished);
        let value = self.fun.new_variable();
        self.fun.ebbs[ebb].arguments.push(value, &mut self.fun.value_pool);
        value
    }

    pub fn position_at_end(&mut self, ebb: Ebb) {
        assert!(self.state == BuilderState::Build);
        self.current_ebb = Some(ebb);
        let last_op = self.fun.layout.ebbs[ebb].last_op;
        self.current_op = last_op;
    }

    pub fn position_at_start(&mut self, ebb: Ebb) {
        assert!(self.state == BuilderState::Build);
        self.current_ebb = Some(ebb);
        self.current_op = None;
    }

    pub fn position_after(&mut self, op: Op) {
        assert!(self.state == BuilderState::Build);
        let ebb = self.fun.layout.ops[op].ebb.unwrap();
        self.current_ebb = Some(ebb);
        self.current_op = Some(op);
    }

    pub fn current_ebb(&self) -> Ebb {
        self.current_ebb.unwrap()
    }
    pub fn assert_at_end(&self) {
        if let Some(inner) = self.fun.layout.ebbs[self.current_ebb.unwrap()].last_op {
            assert!(self.current_op.unwrap() == inner);
        } else {
            assert!(self.current_op.is_none());
        }
    }

    pub fn position_store(&self) -> BuilderPosition {
        assert!(self.state == BuilderState::Build);
        BuilderPosition(self.current_ebb, self.current_op)
    }
    pub fn position_load(&mut self, pos: BuilderPosition) {
        assert!(self.state == BuilderState::Build);
        self.current_ebb = pos.0;
        self.current_op = pos.1;
    }

    pub fn create_ebb_call(&mut self, ebb: Ebb, values: &[Value]) -> EbbCall {
        let values_p = EntityList::from_slice(values, &mut self.fun.value_pool);
        let call = self.fun.ebb_calls.push(EbbCallData {
            source: None,
            target: ebb,
            values: values_p,
        });
        call
    }

    pub fn add_op_ebb_call(&mut self, call: EbbCall) {
        if let BuilderState::OutstandingEbbCalls(outstanding) = self.state {
            let outstanding = outstanding - 1;

            let op = self.current_op.unwrap();
            self.fun.ops[op].ebb_calls
                .push(call, &mut self.fun.ebb_call_pool);
            assert!(self.fun.ebb_calls[call].source.is_none());
            self.fun.ebb_calls[call].source = Some(op);

            if outstanding == 0 {
                self.state = BuilderState::Build;
            } else {
                self.state = BuilderState::OutstandingEbbCalls(outstanding);
            }
        }
    }

    pub fn deposition(&mut self) {
        self.current_ebb = None;
        self.current_op = None;
    }

    pub fn create_atomic(&mut self, atomic: AtomicTerm) -> Value {
        let val = self.fun.values.push(
            ValueType::Constant(ConstantTerm::Atomic(atomic)));
        self.fun.constant_values.insert(val);
        val
    }
    pub fn create_constant(&mut self, constant: ConstantTerm) -> Value {
        let val = self.fun.values.push(
            ValueType::Constant(constant));
        self.fun.constant_values.insert(val);
        val
    }

    //pub fn op_arguments(&mut self, results: &mut Vec<Value>) -> Op {
    //    self.gen_variables(self.fun.ident.arity, results);

    //    let writes = EntityList::from_slice(results, &mut self.fun.value_pool);

    //    self.insert_op(OpData {
    //        kind: OpKind::Arguments,
    //        reads: EntityList::new(),
    //        writes: writes,
    //        ebb_calls: EntityList::new(),
    //    })
    //}

    pub fn op_move_write_token(&mut self, value: Value, token: WriteToken) -> Value {
        self.op_move_internal(value, token.0);
        token.0
    }

    pub fn op_move(&mut self, value: Value) -> Value {
        let result = self.fun.new_variable();
        self.op_move_internal(value, result);
        result
    }

    fn op_move_internal(&mut self, src: Value, dest: Value) {
        let writes = EntityList::from_slice(&[dest], &mut self.fun.value_pool);
        let reads = EntityList::from_slice(&[src], &mut self.fun.value_pool);

        self.insert_op(OpData {
            kind: OpKind::Move,
            reads: reads,
            writes: writes,
            ebb_calls: EntityList::new(),
        });
    }

    pub fn op_jump(&mut self, ebb_call: EbbCall) {
        let ebb_calls = EntityList::from_slice(
            &[ebb_call], &mut self.fun.ebb_call_pool);
        self.insert_op(OpData {
            kind: OpKind::Jump,
            reads: EntityList::new(),
            writes: EntityList::new(),
            ebb_calls: ebb_calls,
        });
    }

    pub fn op_call(&mut self, module: Value, name: Value, arity: usize,
                   args: &[Value]) -> (Value, Value) {
        self.op_call_internal(module, name, args, arity, CallType::Normal).unwrap()
    }
    pub fn op_tail_call(&mut self, module: Value, name: Value, arity: usize,
                        args: &[Value]) {
        assert!(self.op_call_internal(
            module, name, args, arity, CallType::Tail).is_none());
    }
    pub fn op_cont_call(&mut self, module: Value, name: Value, arity: usize,
                        args: &[Value]) {
        assert!(self.op_call_internal(
            module, name, args, arity, CallType::Cont).is_none());
    }

    pub fn op_call_internal(&mut self, module: Value, name: Value,
                            args: &[Value], arity: usize, call_type: CallType
    ) -> Option<(Value, Value)>
    {
        let mut reads = EntityList::from_slice(
            &[module, name], &mut self.fun.value_pool);
        reads.extend(args.iter().cloned(), &mut self.fun.value_pool);

        let result;
        let writes = if call_type == CallType::Normal {
            let result_ok = self.fun.new_variable();
            let result_err = self.fun.new_variable();
            result = Some((result_ok, result_err));
            EntityList::from_slice(
                &[result_ok, result_err], &mut self.fun.value_pool)
        } else {
            result = None;
            EntityList::new()
        };

        self.insert_op(OpData {
            kind: OpKind::Call { call_type, arity: arity },
            reads: reads,
            writes: writes,
            ebb_calls: EntityList::new(),
        });

        if call_type == CallType::Normal {
            self.state = BuilderState::OutstandingEbbCalls(1);
        }

        result
    }

    pub fn op_apply(&mut self, fun: Value, args: &[Value]) -> (Value, Value) {
        self.op_apply_internal(fun, args, CallType::Normal).unwrap()
    }
    pub fn op_tail_apply(&mut self, fun: Value, args: &[Value]) {
        assert!(self.op_apply_internal(fun, args, CallType::Tail).is_none());
    }
    pub fn op_cont_apply(&mut self, fun: Value, args: &[Value]) {
        assert!(self.op_apply_internal(fun, args, CallType::Cont).is_none());
    }

    fn op_apply_internal(&mut self, fun: Value, args: &[Value], call_type: CallType) -> Option<(Value, Value)> {
        let mut reads = EntityList::from_slice(
            &[fun], &mut self.fun.value_pool);
        reads.extend(args.iter().cloned(), &mut self.fun.value_pool);

        let result;
        let writes = if call_type == CallType::Normal {
            let result_ok = self.fun.new_variable();
            let result_err = self.fun.new_variable();
            result = Some((result_ok, result_err));
            EntityList::from_slice(
                &[result_ok, result_err], &mut self.fun.value_pool)
        } else {
            result = None;
            EntityList::new()
        };

        self.insert_op(OpData {
            kind: OpKind::Apply { call_type },
            reads: reads,
            writes: writes,
            ebb_calls: EntityList::new(),
        });

        if call_type == CallType::Normal {
            self.state = BuilderState::OutstandingEbbCalls(1);
        }

        result
    }

    pub fn op_capture_named_function(&mut self, name: FunctionIdent) -> Value {
        let result = self.fun.new_variable();
        let writes = EntityList::from_slice(
            &[result], &mut self.fun.value_pool);

        self.insert_op(OpData {
            kind: OpKind::CaptureNamedFunction(name),
            reads: EntityList::new(),
            writes: writes,
            ebb_calls: EntityList::new(),
        });

        result
    }

    pub fn op_unpack_value_list(&mut self, val_list: Value, num: usize,
                                result: &mut Vec<Value>) {
        if num == 1 {
            result.clear();
            result.push(val_list);
        } else {
            self.gen_variables(num, result);

            let reads = EntityList::from_slice(&[val_list], &mut self.fun.value_pool);
            let writes = EntityList::from_slice(result, &mut self.fun.value_pool);

            self.insert_op(OpData {
                kind: OpKind::UnpackValueList,
                reads: reads,
                writes: writes,
                ebb_calls: EntityList::new(),
            });
        }
    }

    pub fn op_pack_value_list(&mut self, values: &[Value]) -> Value {
        if values.len() == 1 {
            values[0]
        } else {
            let reads = EntityList::from_slice(values, &mut self.fun.value_pool);

            let result = self.fun.new_variable();
            let writes = EntityList::from_slice(
                &[result], &mut self.fun.value_pool);

            self.insert_op(OpData {
                kind: OpKind::PackValueList,
                reads: reads,
                writes: writes,
                ebb_calls: EntityList::new(),
            });

            result
        }
    }

    pub fn op_return_throw(&mut self, value: Value) {
        let reads = EntityList::from_slice(&[value], &mut self.fun.value_pool);
        self.insert_op(OpData {
            kind: OpKind::ReturnThrow,
            reads: reads,
            writes: EntityList::new(),
            ebb_calls: EntityList::new(),
        });
    }

    pub fn op_return_ok(&mut self, value: Value) {
        let reads = EntityList::from_slice(&[value], &mut self.fun.value_pool);
        self.insert_op(OpData {
            kind: OpKind::ReturnOk,
            reads: reads,
            writes: EntityList::new(),
            ebb_calls: EntityList::new(),
        });
    }

    pub fn op_unpack_env(&mut self, value: Value, num_values: usize,
                         results: &mut Vec<Value>) {
        self.gen_variables(num_values, results);

        let writes = EntityList::from_slice(results, &mut self.fun.value_pool);
        let reads = EntityList::from_slice(&[value], &mut self.fun.value_pool);
        self.insert_op(OpData {
            kind: OpKind::UnpackEnv,
            reads: reads,
            writes: writes,
            ebb_calls: EntityList::new(),
        });
    }

    pub fn op_bind_closure(&mut self, ident: FunctionIdent, env: Value) -> Value {
        let result = self.fun.new_variable();

        let writes = EntityList::from_slice(&[result], &mut self.fun.value_pool);
        let reads = EntityList::from_slice(&[env], &mut self.fun.value_pool);
        self.insert_op(OpData {
            kind: OpKind::BindClosure {
                ident: ident,
            },
            reads: reads,
            writes: writes,
            ebb_calls: EntityList::new(),
        });

        result
    }

    pub fn op_make_tuple(&mut self, values: &[Value]) -> Value {
        let result = self.fun.new_variable();

        let reads = EntityList::from_slice(values, &mut self.fun.value_pool);
        let writes = EntityList::from_slice(&[result], &mut self.fun.value_pool);
        self.insert_op(OpData {
            kind: OpKind::MakeTuple,
            reads: reads,
            writes: writes,
            ebb_calls: EntityList::new(),
        });

        result
    }

    pub fn op_make_list(&mut self, head: &[Value], tail: Value) -> Value {
        let mut reads = EntityList::from_slice(
            &[tail], &mut self.fun.value_pool);
        reads.extend(head.iter().cloned(), &mut self.fun.value_pool);

        let result = self.fun.new_variable();
        let writes = EntityList::from_slice(&[result], &mut self.fun.value_pool);

        self.insert_op(OpData {
            kind: OpKind::MakeList,
            reads: reads,
            writes: writes,
            ebb_calls: EntityList::new(),
        });

        result
    }

    pub fn op_map_empty(&mut self) -> Value {
        let result = self.fun.new_variable();
        let writes = EntityList::from_slice(&[result], &mut self.fun.value_pool);

        self.insert_op(OpData {
            kind: OpKind::MapEmpty,
            reads: EntityList::new(),
            writes: writes,
            ebb_calls: EntityList::new(),
        });

        result
    }

    pub fn op_map_put(&mut self, map: Value, key: Value, val: Value,
                      update: bool, fail_call: EbbCall) -> Value {
        let result = self.fun.new_variable();
        let writes = EntityList::from_slice(&[result], &mut self.fun.value_pool);

        let reads = EntityList::from_slice(
            &[map, key, val], &mut self.fun.value_pool);

        let ebb_calls = EntityList::from_slice(
            &[fail_call], &mut self.fun.ebb_call_pool);

        self.insert_op(OpData {
            kind: OpKind::MapPut { update },
            reads: reads,
            writes: writes,
            ebb_calls: ebb_calls,
        });

        result
    }

    pub fn op_make_binary(&mut self, values: &[Value]) -> Value {
        assert!(values.len() % 5 == 0);

        let result = self.fun.new_variable();
        let writes = EntityList::from_slice(&[result], &mut self.fun.value_pool);

        let reads = EntityList::from_slice(values, &mut self.fun.value_pool);

        self.insert_op(OpData {
            kind: OpKind::MakeBinary,
            reads: reads,
            writes: writes,
            ebb_calls: EntityList::new(),
        });

        result
    }

    pub fn op_unpack_tuple(&mut self, value: Value, num: usize, ret: &mut Vec<Value>, fail: EbbCall) {
        self.gen_variables(num, ret);

        let writes = EntityList::from_slice(ret, &mut self.fun.value_pool);
        let reads = EntityList::from_slice(&[value], &mut self.fun.value_pool);
        let calls = EntityList::from_slice(&[fail], &mut self.fun.ebb_call_pool);
        self.insert_op(OpData {
            kind: OpKind::UnpackTuple,
            reads: reads,
            writes: writes,
            ebb_calls: calls,
        });
    }

    pub fn op_unpack_list_cell(&mut self, value: Value, fail: EbbCall) -> (Value, Value) {
        let head = self.fun.new_variable();
        let tail = self.fun.new_variable();
        let writes = EntityList::from_slice(&[head, tail], &mut self.fun.value_pool);

        let reads = EntityList::from_slice(&[value], &mut self.fun.value_pool);

        let calls = EntityList::from_slice(&[fail], &mut self.fun.ebb_call_pool);

        self.insert_op(OpData {
            kind: OpKind::UnpackListCell,
            reads: reads,
            writes: writes,
            ebb_calls: calls,
        });

        (head, tail)
    }

    pub fn op_is_map(&mut self, value: Value, fail: EbbCall) {
        let reads = EntityList::from_slice(&[value], &mut self.fun.value_pool);
        let calls = EntityList::from_slice(&[fail], &mut self.fun.ebb_call_pool);
        self.insert_op(OpData {
            kind: OpKind::UnpackListCell,
            reads: reads,
            writes: EntityList::new(),
            ebb_calls: calls,
        });
    }

    pub fn op_map_get(&mut self, map: Value, key: Value, fail: EbbCall) -> Value {
        let result = self.fun.new_variable();
        let writes = EntityList::from_slice(&[result], &mut self.fun.value_pool);
        let reads = EntityList::from_slice(&[map, key], &mut self.fun.value_pool);
        let calls = EntityList::from_slice(&[fail], &mut self.fun.ebb_call_pool);
        self.insert_op(OpData {
            kind: OpKind::MapGet,
            reads: reads,
            writes: writes,
            ebb_calls: calls,
        });
        result
    }

    pub fn op_primop(&mut self, name: crate::Atom, values: &[Value]) -> Value {
        let result = self.fun.new_variable();
        let writes = EntityList::from_slice(&[result], &mut self.fun.value_pool);

        let reads = EntityList::from_slice(values, &mut self.fun.value_pool);

        self.insert_op(OpData {
            kind: OpKind::PrimOp(name),
            reads: reads,
            writes: writes,
            ebb_calls: EntityList::new(),
        });

        result
    }

    pub fn op_make_no_value(&mut self) -> Value {
        let result = self.fun.new_variable();
        let writes = EntityList::from_slice(&[result], &mut self.fun.value_pool);

        self.insert_op(OpData {
            kind: OpKind::PackValueList,
            reads: EntityList::new(),
            writes: writes,
            ebb_calls: EntityList::new(),
        });

        result
    }

    pub fn op_make_closure_env(&mut self, lambda_env: ClosureEnv, values: &[Value]) -> Value {
        let result = self.fun.new_variable();
        let writes = EntityList::from_slice(&[result], &mut self.fun.value_pool);

        let reads = EntityList::from_slice(values, &mut self.fun.value_pool);

        self.insert_op(OpData {
            kind: OpKind::MakeClosureEnv { env_idx: lambda_env },
            reads: reads,
            writes: writes,
            ebb_calls: EntityList::new(),
        });

        result
    }

    pub fn op_case_start(&mut self, clauses: Vec<Clause>,
                         value: Value, value_vars: &[Value], body: Ebb) -> Value {
        let result = self.fun.new_variable();
        let writes = EntityList::from_slice(&[result], &mut self.fun.value_pool);

        let mut reads = EntityList::from_slice(&[value], &mut self.fun.value_pool);
        reads.extend(value_vars.iter().cloned(), &mut self.fun.value_pool);

        let call = self.create_ebb_call(body, &[]);
        let branches = EntityList::from_slice(&[call], &mut self.fun.ebb_call_pool);

        self.insert_op(OpData {
            kind: OpKind::CaseStart {
                clauses: clauses,
            },
            reads: reads,
            writes: writes,
            ebb_calls: branches,
        });

        result
    }

    pub fn op_case_body(&mut self, case_val: Value, num_clauses: usize) {
        let reads = EntityList::from_slice(&[case_val], &mut self.fun.value_pool);
        self.insert_op(OpData {
            kind: OpKind::Case(num_clauses),
            reads: reads,
            writes: EntityList::new(),
            ebb_calls: EntityList::new(),
        });
        self.state = BuilderState::OutstandingEbbCalls(num_clauses + 1);
    }

    pub fn op_case_values(&mut self, case_val: Value, num_results: usize,
                          results: &mut Vec<Value>) {
        self.gen_variables(num_results, results);
        let reads = EntityList::from_slice(&[case_val], &mut self.fun.value_pool);
        let writes = EntityList::from_slice(results, &mut self.fun.value_pool);
        self.insert_op(OpData {
            kind: OpKind::CaseValues,
            reads: reads,
            writes: writes,
            ebb_calls: EntityList::new(),
        });
    }

    pub fn op_case_guard_ok(&mut self, case_val: Value) {
        let reads = EntityList::from_slice(&[case_val], &mut self.fun.value_pool);
        self.insert_op(OpData {
            kind: OpKind::CaseGuardOk,
            reads: reads,
            writes: EntityList::new(),
            ebb_calls: EntityList::new(),
        });
    }
    pub fn op_case_guard_fail(&mut self, case_val: Value, clause_num: usize, ebb: Ebb) {
        let reads = EntityList::from_slice(&[case_val], &mut self.fun.value_pool);
        let branch = self.create_ebb_call(ebb, &[]);
        let branches = EntityList::from_slice(&[branch], &mut self.fun.ebb_call_pool);
        self.insert_op(OpData {
            kind: OpKind::CaseGuardFail { clause_num },
            reads: reads,
            writes: EntityList::new(),
            ebb_calls: branches,
        });
    }

    pub fn op_branch_not_truthy(&mut self, value: Value, call: EbbCall) {
        let reads = EntityList::from_slice(&[value], &mut self.fun.value_pool);
        let calls = EntityList::from_slice(&[call], &mut self.fun.ebb_call_pool);
        self.insert_op(OpData {
            kind: OpKind::IfTruthy,
            reads: reads,
            writes: EntityList::new(),
            ebb_calls: calls,
        });
    }

    pub fn op_receive_start(&mut self, timeout: Value, body: Ebb) -> Value {
        let result = self.fun.new_variable();
        let writes = EntityList::from_slice(&[result], &mut self.fun.value_pool);

        let reads = EntityList::from_slice(&[timeout], &mut self.fun.value_pool);

        let call = self.create_ebb_call(body, &[]);
        let branches = EntityList::from_slice(&[call], &mut self.fun.ebb_call_pool);

        self.insert_op(OpData {
            kind: OpKind::ReceiveStart,
            reads: reads,
            writes: writes,
            ebb_calls: branches,
        });

        result
    }

    pub fn op_receive_wait(&mut self, structure: Value,
                           match_call: EbbCall, timeout_call: EbbCall) {
        let reads = EntityList::from_slice(&[structure], &mut self.fun.value_pool);
        let branches = EntityList::from_slice(
            &[match_call, timeout_call], &mut self.fun.ebb_call_pool);

        self.insert_op(OpData {
            kind: OpKind::ReceiveWait,
            reads: reads,
            writes: EntityList::new(),
            ebb_calls: branches,
        });
    }

    pub fn op_receive_get_message(&mut self, structure: Value) -> Value {
        let result = self.fun.new_variable();
        let writes = EntityList::from_slice(&[result], &mut self.fun.value_pool);

        let reads = EntityList::from_slice(&[structure], &mut self.fun.value_pool);

        self.insert_op(OpData {
            kind: OpKind::ReceiveGetMessage,
            reads: reads,
            writes: writes,
            ebb_calls: EntityList::new(),
        });

        result
    }

    pub fn op_unreachable(&mut self) {
        self.insert_op(OpData {
            kind: OpKind::Unreachable,
            reads: EntityList::new(),
            writes: EntityList::new(),
            ebb_calls: EntityList::new(),
        });
    }

    pub fn op_exc_trace(&mut self, val: Value) -> Value {
        let result = self.fun.new_variable();
        let writes = EntityList::from_slice(&[result], &mut self.fun.value_pool);

        let reads = EntityList::from_slice(&[val], &mut self.fun.value_pool);

        self.insert_op(OpData {
            kind: OpKind::ExcTrace,
            reads: reads,
            writes: writes,
            ebb_calls: EntityList::new(),
        });

        result
    }

    pub fn op_equal(&mut self, lhs: Value, rhs: Value, call: EbbCall) {
        let reads = EntityList::from_slice(&[lhs, rhs], &mut self.fun.value_pool);
        let branches = EntityList::from_slice(&[call], &mut self.fun.ebb_call_pool);
        self.insert_op(OpData {
            kind: OpKind::ComparisonOperation(ComparisonOperation::Equal),
            reads: reads,
            writes: EntityList::new(),
            ebb_calls: branches,
        });
    }

}



