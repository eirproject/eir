use std::collections::HashMap;

use inkwell::context::Context;
use inkwell::values::{ FunctionValue, PointerValue,
                       BasicValueEnum, PhiValue,
                       IntValue };
use inkwell::types::BasicTypeEnum;
use inkwell::module::{ Module };
use inkwell::builder::Builder;
use inkwell::basic_block::BasicBlock;

use crate::target::TargetEmit;

use eir::FunctionIdent;
use eir::Module as EirModule;
use eir::Function as EirFunction;
use eir::{ Value, Ebb, Op };
use eir::{ ValueType };
use eir::AttributeKey;
use eir::op::OpKind;
use eir::op::CallType;

pub fn mangle_string(string: &str) -> String {
    string
        .replace("_", "__")
        .replace("+", "_p")
        .replace("-", "_m")
        .replace("<", "_l")
        .replace(">", "_g")
        .replace("!", "_x")
}

pub fn mangle_ident(ident: &FunctionIdent) -> String {
    let module_str = mangle_string(ident.module.as_str());
    let name_str = mangle_string(ident.name.as_str());
    format!(
        "GNIF{}_{}{}_{}{}_{}_{}",
        module_str.len(), module_str,
        name_str.len(), name_str,
        ident.arity,
        ident.lambda.map(|l| l.0.to_string()).unwrap_or("n".to_string()),
        ident.lambda.map(|l| l.1.to_string()).unwrap_or("n".to_string()),
    )
}

pub struct LowerCtx<'a> {
    pub context: &'a Context,
    pub module: &'a Module,
    pub builder: &'a Builder,
    pub fn_val: FunctionValue,
    pub protos: &'a HashMap<FunctionIdent, FunctionValue>,
    ebb_map: HashMap<Ebb, BasicBlock>,
    phi_map: HashMap<(Ebb, usize), PhiValue>,
    val_map: HashMap<Value, BasicValueEnum>,
    pub loc_id: &'a mut u64,
    pub eir_mod: &'a EirModule,
    pub eir_fun: &'a EirFunction,
}

fn emit_read<Target>(
    target: &mut Target,
    ctx: &mut LowerCtx,
    fun: &EirFunction,
    read: Value,
) -> BasicValueEnum
    where Target: TargetEmit
{
    let data = fun.value(read);
    match data {
        ValueType::Variable => {
            println!("Read: {}", read);
            ctx.val_map[&read]
        },
        ValueType::Constant(constant) => {
            target.emit_constant(ctx, constant)
        },
    }
}

pub fn build_stack_arr(
    ctx: &mut LowerCtx,
    typ: BasicTypeEnum,
    values: &[BasicValueEnum],
) -> (PointerValue, IntValue) {
    let i64_type = ctx.context.i64_type();
    let i32_type = ctx.context.i32_type();

    let values_len = i32_type.const_int(
        values.len() as u64, false);
    let values_arr = ctx.builder.build_array_alloca(
        typ, values_len, "");

    for (idx, read) in values.iter().enumerate() {
        let elem_ptr = unsafe {
            ctx.builder.build_gep(
                values_arr,
                &[i64_type.const_int(idx as u64, false)],
                "",
            )
        };
        ctx.builder.build_store(elem_ptr, *read);
    }

    (values_arr, values_len)
}



fn add_branch_phis<Target>(
    target: &mut Target,
    ctx: &mut LowerCtx,
    fun: &EirFunction,
    current: &BasicBlock,
    target_ebb: Ebb,
    in_args: &[Value],
) where Target: TargetEmit
{
    for (idx, src_arg) in in_args.iter().enumerate() {
        let phi = ctx.phi_map[&(target_ebb, idx)];
        println!("BranchPhiRead {:?}", src_arg);
        //let val = ctx.val_map[src_arg];
        let val = emit_read(target, ctx, fun, *src_arg);
        phi.add_incoming(&[(&val, current)]);
    }
}

#[cfg(feature = "print_trace")]
fn debug_trace_printf(ctx: &mut LowerCtx, text: &str) {
    let string_const = crate::primitives::make_c_string_const(
        ctx.context, ctx.module, text);
    let string_const_ptr = string_const.as_pointer_value()
        .const_cast(ctx.nif_types.i8_ptr.into_pointer_type());
    ctx.builder.build_call(
        ctx.nif_types.printf, &[string_const_ptr.into()], "debug print");
}
#[cfg(not(feature = "print_trace"))]
fn debug_trace_printf(_ctx: &mut LowerCtx, _text: &str) {}

fn emit_op<Target>(
    target: &mut Target,
    ctx: &mut LowerCtx,
    fun: &EirFunction,
    op: Op,
) where Target: TargetEmit
{
    let mut read_buf = Vec::new();
    let mut out_buf: Vec<BasicValueEnum> = Vec::new();

    // Reads
    read_buf.clear();
    for read in fun.op_reads(op) {
        let value = emit_read(
            target,
            ctx,
            fun,
            *read,
        );
        read_buf.push(value);
    }

    let reads = fun.op_reads(op);
    let writes = fun.op_writes(op);
    let branches = fun.op_branches(op);

    match fun.op_kind(op) {
        OpKind::MakeClosureEnv { .. } => {
            debug_trace_printf(ctx, "MakeClosureEnv\n");
            assert!(writes.len() == 1);
            let value_enum = target.emit_make_closure_env(ctx, &read_buf);
            ctx.val_map.insert(writes[0], value_enum);
        },
        OpKind::UnpackEnv { .. } => {
            debug_trace_printf(ctx, "UnpackEnv\n");
            assert!(reads.len() == 1);

            out_buf.clear();
            target.emit_unpack_closure_env(
                ctx, read_buf[0], &mut out_buf, writes.len());
            assert!(out_buf.len() == writes.len());

            for (val, ssa) in out_buf.iter().zip(writes.iter()) {
                ctx.val_map.insert(*ssa, *val);
            }
        },
        OpKind::CaptureNamedFunction(ident) => {
            debug_trace_printf(ctx, "CaptureNamedFunction\n");
            assert!(writes.len() == 1);
            assert!(ident.lambda.is_none());

            let fun_val = target.emit_capture_named_function(ctx, ident);
            ctx.val_map.insert(writes[0], fun_val);
        },
        OpKind::BindClosure { ident } => {
            debug_trace_printf(ctx, "BindClosure\n");
            assert!(writes.len() == 1);

            let clo_val = target.emit_bind_closure(ctx, ident, read_buf[0]);
            ctx.val_map.insert(writes[0], clo_val);
        },
        OpKind::Jump => {
            debug_trace_printf(ctx, "Jump\n");
            assert!(writes.len() == 0);
            let ebb_call = branches[0];
            let target_ebb = fun.ebb_call_target(ebb_call);
            let from_bb = ctx.builder.get_insert_block().unwrap();
            add_branch_phis(target, ctx, fun, &from_bb, target_ebb,
                            fun.ebb_call_args(ebb_call));
            ctx.builder.build_unconditional_branch(&ctx.ebb_map[&target_ebb]);
        },
        OpKind::Move => {
            assert!(writes.len() == 1);
            ctx.val_map.insert(writes[0], read_buf[0]);
        },
        // TODO: These should be removed in a compilation pass,
        // and should not exist in the IR at this point.
        // For now we use tuples as a hack.
        OpKind::PackValueList => {
            debug_trace_printf(ctx, "PackValueList\n");
            assert!(writes.len() == 1);
            let value_enum = target.emit_make_tuple(ctx, &read_buf);
            ctx.val_map.insert(writes[0], value_enum);
        },
        OpKind::UnpackValueList => {
            debug_trace_printf(ctx, "UnpackValueList\n");
            //let err_bb = ctx.context.append_basic_block(&ctx.fn_val, "val_unpack_err");

            let (ok_bb, err_bb) = target.emit_unpack_tuple(
                ctx, read_buf[0], writes.len(),
                &mut out_buf);

            ctx.builder.position_at_end(&err_bb);
            target.emit_unreachable_fail(ctx);

            ctx.builder.position_at_end(&ok_bb);

            for (out, write) in out_buf.iter().zip(writes.iter()) {
                ctx.val_map.insert(*write, *out);
            }
        },
        OpKind::Apply { call_type } => {
            assert!(writes.len() == 0);
            debug_trace_printf(ctx, "Apply\n");
            if *call_type == CallType::Cont {
                assert!(reads.len() == 2);
                target.emit_apply_cont(ctx, read_buf[0], read_buf[1]);
            } else {
                assert!(*call_type == CallType::Tail);
                target.emit_apply_norm(ctx, read_buf[0], &read_buf[1..]);
            }
            ctx.builder.build_unreachable();
        },
        OpKind::Call { arity, call_type } => {
            assert!(writes.len() == 0);
            assert!(*call_type == CallType::Tail);
            if fun.value_is_constant(reads[0]) && fun.value_is_constant(reads[1]) {
                let module = fun.value_constant(reads[0]).atom().unwrap();
                let name = fun.value_constant(reads[1]).atom().unwrap();
                let ident = FunctionIdent {
                    module: module.clone(),
                    name: name.clone(),
                    lambda: None,
                    arity: *arity,
                };

                debug_trace_printf(ctx, &format!("Call {}\n", ident));
                target.emit_call_const(ctx, &ident, &read_buf[2..]);

            } else {
                debug_trace_printf(ctx, "CallDyn\n");
                target.emit_call_dyn(ctx, read_buf[0], read_buf[1], &read_buf[2..]);
            }
            ctx.builder.build_unreachable();
        },
        OpKind::ComparisonOperation(comp_op) => {
            assert!(writes.len() == 0);
            debug_trace_printf(ctx, "ComparisonOperation\n");

            let cmp_res = target.emit_compare(
                ctx, read_buf[0], read_buf[1], comp_op);

            // Target independent branch on result
            let fail_call = branches[0];
            let fail_target = fun.ebb_call_target(fail_call);

            let from_bb = ctx.builder.get_insert_block().unwrap();

            let ok_bb = ctx.context.append_basic_block(&ctx.fn_val, "compare_eq_ok");
            ctx.builder.build_conditional_branch(
                cmp_res, &ok_bb, &ctx.ebb_map[&fail_target]);

            add_branch_phis(target, ctx, fun, &from_bb, fail_target,
                            fun.ebb_call_args(fail_call));

            ctx.builder.position_at_end(&ok_bb);
        },
        OpKind::MakeTuple => {
            assert!(writes.len() == 1);
            let fun_tuple = target.emit_make_tuple(ctx, &read_buf);
            ctx.val_map.insert(writes[0], fun_tuple);
        },
        OpKind::IfTruthy => {
            let cond = target.emit_is_truthy(ctx, read_buf[0]);

            // Target independent branch
            let fail_call = branches[0];
            let fail_target = fun.ebb_call_target(fail_call);

            let from_bb = ctx.builder.get_insert_block().unwrap();

            let ok_bb = ctx.context.append_basic_block(&ctx.fn_val, "if_truthy_true");

            ctx.builder.build_conditional_branch(
                cond, &ok_bb, &ctx.ebb_map[&fail_target]);

            add_branch_phis(target, ctx, fun, &from_bb, fail_target,
                            fun.ebb_call_args(fail_call));

            ctx.builder.position_at_end(&ok_bb);
        },
        // TODO: This really shouldn't be a OP, it should be a function call
        OpKind::MapPut { update } => {
            let map_val = read_buf[0];
            let key_val = read_buf[1];
            let val_val = read_buf[2];

            let fail_call = branches[0];
            let fail_target = fun.ebb_call_target(fail_call);

            let from_bb = ctx.builder.get_insert_block().unwrap();
            let ok_bb = ctx.context.append_basic_block(
                &ctx.fn_val, "map_put_ok");

            let (success, new_map) = target.emit_map_put(
                ctx, map_val, key_val, val_val, *update);

            add_branch_phis(target, ctx, fun, &from_bb, fail_target,
                            fun.ebb_call_args(fail_call));
            ctx.builder.build_conditional_branch(success, &ok_bb,
                                                 &ctx.ebb_map[&fail_target]);

            ctx.builder.position_at_end(&ok_bb);
            ctx.val_map.insert(writes[0], new_map);
        },
        OpKind::UnpackTuple => {
            debug_trace_printf(ctx, "UnpackTuple\n");

            let fail_call = branches[0];
            let fail_target = fun.ebb_call_target(fail_call);
            let err_ebb = fun.ebb_call_target(fail_call);

            // TODO FIXME: EbbCall args
            assert!(ctx.eir_fun.ebb_call_args(branches[0]).len() == 0);

            let (ok_bb, err_bb) = target.emit_unpack_tuple(
                ctx, read_buf[0], writes.len(),
                &mut out_buf);

            ctx.builder.position_at_end(&err_bb);
            ctx.builder.build_unconditional_branch(&ctx.ebb_map[&err_ebb]);
            add_branch_phis(target, ctx, fun, &err_bb, fail_target,
                            fun.ebb_call_args(fail_call));

            ctx.builder.position_at_end(&ok_bb);
            for (out, write) in out_buf.iter().zip(writes.iter()) {
                ctx.val_map.insert(*write, *out);
            }

        },
        OpKind::Unreachable => {
            debug_trace_printf(ctx, "UnpackTuple\n");
            target.emit_unreachable_fail(ctx);
        },
        kind => unimplemented!("{:?}", kind),
    }

}

fn emit_ebb<Target>(
    target: &mut Target,
    ctx: &mut LowerCtx,
    fun: &EirFunction,
    ebb: Ebb,
) where Target: TargetEmit
{

    let bb = &ctx.ebb_map[&ebb];
    ctx.builder.position_at_end(bb);

    for op in fun.iter_op(ebb) {
        emit_op(target, ctx, fun, op);
    }

}

pub fn emit_fun<Target>(
    target: &mut Target,
    context: &Context,
    module: &Module,
    protos: &mut HashMap<FunctionIdent, FunctionValue>,
    loc_id: &mut u64,
    eir_mod: &EirModule,
    fun: &EirFunction,
    fn_val: FunctionValue,
) where Target: TargetEmit
{
    println!("{}", fun.ident());

    let builder = context.create_builder();
    let mut ctx = LowerCtx {
        context: context,
        module: module,
        builder: &builder,
        fn_val: fn_val,
        ebb_map: HashMap::new(),
        phi_map: HashMap::new(),
        val_map: HashMap::new(),
        loc_id: loc_id,
        eir_mod: eir_mod,
        eir_fun: fun,
        protos: protos,
    };

    let entry_bb = context.append_basic_block(&fn_val, "entry");

    let entry_ebb = fun.ebb_entry();
    let ebb_args = ctx.eir_fun.ebb_args(entry_ebb);

    // Add llvm bbs corresponding to the entry of eir ebbs.
    for ebb in fun.iter_ebb() {
        let bb = context.append_basic_block(&fn_val, &format!("{}", ebb));
        builder.position_at_end(&bb);
        ctx.ebb_map.insert(ebb, bb);
        for (idx, arg) in fun.ebb_args(ebb).iter().enumerate() {
            let typ = target.type_for(&mut ctx, *arg);
            let phi = builder.build_phi(typ, &format!("{}", arg));
            ctx.phi_map.insert((ebb, idx), phi);
            ctx.val_map.insert(*arg, phi.as_basic_value());
        }
    }

    let mut args_out = Vec::new();
    ctx.builder.position_at_end(&entry_bb);
    target.emit_preamble(&mut ctx, &mut args_out);

    //// Arguments and entry bb
    for (n, _arg) in ebb_args.iter().enumerate() {
        let value = args_out[n];
        ctx.phi_map[&(entry_ebb, n)].add_incoming(&[(&value, &entry_bb)]);
    }

    if fun.has_attribute(&AttributeKey::Continuation) {
        assert!(fun.ident().lambda.is_some());
        assert!(ebb_args.len() == 2);
    } else {
        if fun.ident().lambda.is_some() {
            // env, ok_cont, err_cont
            assert!(ebb_args.len() >= 3);
        } else {
            // ok_cont, err_cont
            assert!(ebb_args.len() >= 2);
        }
    }

    debug_trace_printf(&mut ctx, "Entry!\n");

    builder.build_unconditional_branch(&ctx.ebb_map[&entry_ebb]);

    // ebbs in eir ir
    for ebb in fun.iter_ebb() {
        emit_ebb(target, &mut ctx, fun, ebb);
    }

}

