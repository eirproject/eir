use std::collections::HashMap;

use inkwell::AddressSpace;
use inkwell::IntPredicate;
use inkwell::context::Context;
use inkwell::values::{ FunctionValue, ArrayValue, StructValue, PointerValue,
                       GlobalValue, AnyValueEnum, BasicValueEnum, PhiValue,
                       IntValue };
use inkwell::values::BasicValue;
use inkwell::types::{ StructType, BasicType, BasicTypeEnum };
use inkwell::module::{ Module };
use inkwell::builder::Builder;
use inkwell::basic_block::BasicBlock;

use num_traits::cast::ToPrimitive;

use crate::target::nif::NifTypes;

use eir::FunctionIdent;
use eir::Module as EirModule;
use eir::Function as EirFunction;
use eir::{ Value, Ebb, Op };
use eir::{ ConstantTerm, AtomicTerm };
use eir::{ ValueType };
use eir::AttributeKey;
use eir::op::OpKind;
use eir::op::ComparisonOperation;
use eir::op::CallType;

pub fn mangle_string(string: &str) -> String {
    string
        .replace("_", "__")
        .replace("+", "_p")
        .replace("-", "_m")
        .replace("<", "_l")
        .replace(">", "_g")
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
    pub nif_types: &'a NifTypes,
    ebb_map: HashMap<Ebb, BasicBlock>,
    phi_map: HashMap<(Ebb, usize), PhiValue>,
    val_map: HashMap<Value, BasicValueEnum>,
    pub loc_id: &'a mut u64,
    pub env: Option<BasicValueEnum>,
    pub eir_mod: &'a EirModule,
}

fn emit_read(
    ctx: &mut LowerCtx,
    fun: &EirFunction,
    read: Value,
) -> BasicValueEnum
{
    let data = fun.value(read);
    match data {
        ValueType::Variable => {
            println!("Read: {}", read);
            ctx.val_map[&read]
        },
        ValueType::Constant(constant) => {
            match constant {
                ConstantTerm::Atomic(AtomicTerm::Integer(num)) => {
                    let i64_type = ctx.context.i64_type();
                    let num_const = i64_type.const_int(
                        num.to_i64().unwrap() as u64, true);
                    let cs = ctx.builder.build_call(
                        ctx.nif_types.enif_make_long,
                        &[ctx.env.unwrap(), num_const.into()],
                        "make_int64"
                    );
                    cs.try_as_basic_value().left().unwrap()
                }
                ConstantTerm::Atomic(AtomicTerm::Atom(val)) => {
                    let i8_type = ctx.context.i8_type();
                    let i8_ptr = i8_type.ptr_type(AddressSpace::Generic);
                    let string_const = crate::primitives::make_c_string_const(
                        ctx.context, ctx.module, val.as_str());
                    let string_const_ptr = string_const.as_pointer_value()
                        .const_cast(i8_ptr);

                    let i64_type = ctx.context.i64_type();
                    let string_len_val = i64_type.const_int(
                        val.as_str().bytes().len() as u64, true);

                    let cs = ctx.builder.build_call(
                        ctx.nif_types.enif_make_atom_len,
                        &[ctx.env.unwrap(), string_const_ptr.into(), string_len_val.into()],
                        "make_atom_len"
                    );
                    cs.try_as_basic_value().left().unwrap()
                }
                ConstantTerm::Atomic(AtomicTerm::Nil) => {
                    let i32_type = ctx.context.i32_type();
                    let call_ret = ctx.builder.build_call(
                        ctx.nif_types.enif_make_list_from_array,
                        &[
                            ctx.env.unwrap(),
                            ctx.nif_types.term_ptr_type.into_pointer_type()
                                .const_null().into(),
                            i32_type.const_int(0, false).into(),
                        ],
                        "",
                    );
                    call_ret.try_as_basic_value().left().unwrap()
                }
                _ => unimplemented!("{:?}", constant),
            }
        },
    }
}

pub fn build_stack_arr(
    ctx: &mut LowerCtx,
    values: &[BasicValueEnum],
) -> (PointerValue, IntValue) {
    let i64_type = ctx.context.i64_type();
    let i32_type = ctx.context.i32_type();

    let values_len = i32_type.const_int(
        values.len() as u64, false);
    let values_arr = ctx.builder.build_array_alloca(
        ctx.nif_types.term_type, values_len, "");

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



fn add_branch_phis(
    ctx: &mut LowerCtx,
    fun: &EirFunction,
    current: &BasicBlock,
    target: Ebb,
    in_args: &[Value],
) {
    for (idx, src_arg) in in_args.iter().enumerate() {
        let phi = ctx.phi_map[&(target, idx)];
        println!("BranchPhiRead {:?}", src_arg);
        //let val = ctx.val_map[src_arg];
        let val = emit_read(ctx, fun, *src_arg);
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

use crate::target::nif::emit as specific;

fn emit_op(
    ctx: &mut LowerCtx,
    eir_mod: &EirModule,
    fun: &EirFunction,
    op: Op,
) {
    let mut read_buf = Vec::new();
    let mut out_buf: Vec<BasicValueEnum> = Vec::new();

    let void_type = ctx.context.void_type();
    let i8_type = ctx.context.i8_type();
    let i32_type = ctx.context.i32_type();
    let i64_type = ctx.context.i64_type();

    // Reads
    read_buf.clear();
    for read in fun.op_reads(op) {
        let value = emit_read(
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
            let value_enum = specific::emit_make_closure_env(ctx, &read_buf);
            ctx.val_map.insert(writes[0], value_enum);
        },
        OpKind::UnpackEnv { .. } => {
            debug_trace_printf(ctx, "UnpackEnv\n");
            assert!(reads.len() == 1);

            out_buf.clear();
            specific::emit_unpack_closure_env(
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

            let fun_val = specific::emit_capture_named_function(ctx, ident);
            ctx.val_map.insert(writes[0], fun_val);
        },
        OpKind::BindClosure { ident } => {
            debug_trace_printf(ctx, "BindClosure\n");
            assert!(writes.len() == 1);

            let clo_val = specific::emit_bind_closure(ctx, ident, read_buf[0]);
            ctx.val_map.insert(writes[0], clo_val);
        },
        OpKind::Jump => {
            debug_trace_printf(ctx, "Jump\n");
            assert!(writes.len() == 0);
            let ebb_call = branches[0];
            let target = fun.ebb_call_target(ebb_call);
            let from_bb = ctx.builder.get_insert_block().unwrap();
            add_branch_phis(ctx, fun, &from_bb, target,
                            fun.ebb_call_args(ebb_call));
            ctx.builder.build_unconditional_branch(&ctx.ebb_map[&target]);
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
            let value_enum = specific::emit_make_tuple(ctx, &read_buf);
            ctx.val_map.insert(writes[0], value_enum);
        },
        OpKind::UnpackValueList => {
            debug_trace_printf(ctx, "UnpackValueList\n");
            let err_bb = ctx.context.append_basic_block(&ctx.fn_val, "val_unpack_err");

            let ok_bb = specific::emit_unpack_tuple(
                ctx, read_buf[0], writes.len(),
                &mut out_buf, &err_bb);

            ctx.builder.position_at_end(&err_bb);
            ctx.builder.build_call(ctx.nif_types.unreachable_fail, &[
                i64_type.const_int(*ctx.loc_id as u64, false).into(),
            ], "");
            *ctx.loc_id += 1;
            ctx.builder.build_unreachable();

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
                specific::emit_apply_cont(ctx, read_buf[0], read_buf[1]);
            } else {
                assert!(*call_type == CallType::Tail);
                specific::emit_apply_norm(ctx, read_buf[0], &read_buf[1..]);
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
                specific::emit_call_const(ctx, &ident, &read_buf[2..]);

            } else {
                debug_trace_printf(ctx, "CallDyn\n");
                specific::emit_call_dyn(ctx, read_buf[0], read_buf[1], &read_buf[2..]);
            }
            ctx.builder.build_unreachable();
        },
        OpKind::ComparisonOperation(comp_op) => {
            assert!(writes.len() == 0);
            debug_trace_printf(ctx, "ComparisonOperation\n");

            let cmp_res = specific::emit_compare(
                ctx, read_buf[0], read_buf[1], comp_op);

            // Target independent branch on result
            let fail_call = branches[0];
            let fail_target = fun.ebb_call_target(fail_call);

            let from_bb = ctx.builder.get_insert_block().unwrap();

            let ok_bb = ctx.context.append_basic_block(&ctx.fn_val, "compare_eq_ok");
            ctx.builder.build_conditional_branch(
                cmp_res, &ok_bb, &ctx.ebb_map[&fail_target]);

            add_branch_phis(ctx, fun, &from_bb, fail_target,
                            fun.ebb_call_args(fail_call));

            ctx.builder.position_at_end(&ok_bb);
        },
        OpKind::MakeTuple => {
            assert!(writes.len() == 1);
            let fun_tuple = specific::emit_make_tuple(ctx, &read_buf);
            ctx.val_map.insert(writes[0], fun_tuple);
        },
        OpKind::IfTruthy => {
            let cond = specific::emit_is_truthy(ctx, read_buf[0]);

            // Target independent branch
            let fail_call = branches[0];
            let fail_target = fun.ebb_call_target(fail_call);

            let from_bb = ctx.builder.get_insert_block().unwrap();

            let ok_bb = ctx.context.append_basic_block(&ctx.fn_val, "if_truthy_true");

            ctx.builder.build_conditional_branch(
                cond, &ok_bb, &ctx.ebb_map[&fail_target]);

            add_branch_phis(ctx, fun, &from_bb, fail_target,
                            fun.ebb_call_args(fail_call));

            ctx.builder.position_at_end(&ok_bb);
        },
        // TODO: This really shouldn't be a OP, it should be a function call
        OpKind::MapPut { update } => {
            let map_val = read_buf[0];
            let key_val = read_buf[1];
            let val_val = read_buf[2];

            //let n_fun = if *update {
            //    ctx.nif_types.enif_make_map_update
            //} else {
            //    ctx.nif_types.enif_make_map_put
            //};

            //let ret_map_val = ctx.builder.build_alloca(ctx.nif_types.term_type, "");
            //let call_ret = ctx.builder.build_call(
            //    n_fun,
            //    &[
            //        ctx.env.unwrap(),
            //        map_val,
            //        key_val,
            //        val_val,
            //        ret_map_val.into(),
            //    ],
            //    ""
            //);

            let fail_call = branches[0];
            let fail_target = fun.ebb_call_target(fail_call);

            let from_bb = ctx.builder.get_insert_block().unwrap();
            let ok_bb = ctx.context.append_basic_block(
                &ctx.fn_val, "map_put_ok");

            let (success, new_map) = specific::emit_map_put(
                ctx, map_val, key_val, val_val, *update);

            // Check return true
            //let cmp_res = ctx.builder.build_int_compare(
            //    IntPredicate::EQ,
            //    call_ret.try_as_basic_value().left().unwrap().into_int_value(),
            //    i32_type.const_int(1, false).into(),
            //    "",
            //);

            add_branch_phis(ctx, fun, &from_bb, fail_target,
                            fun.ebb_call_args(fail_call));
            ctx.builder.build_conditional_branch(success, &ok_bb,
                                                 &ctx.ebb_map[&fail_target]);

            ctx.builder.position_at_end(&ok_bb);
            ctx.val_map.insert(writes[0], new_map);
        },
        kind => unimplemented!("{:?}", kind),
    }

}

fn emit_ebb(
    ctx: &mut LowerCtx,
    eir_mod: &EirModule,
    fun: &EirFunction,
    ebb: Ebb,
) {

    let bb = &ctx.ebb_map[&ebb];
    ctx.builder.position_at_end(bb);

    for op in fun.iter_op(ebb) {
        emit_op(ctx, eir_mod, fun, op);
    }

}

pub fn emit_fun(
    context: &Context,
    module: &Module,
    nif_types: &NifTypes,
    protos: &HashMap<FunctionIdent, FunctionValue>,
    loc_id: &mut u64,
    eir_mod: &EirModule,
    fun: &EirFunction,
    fn_val: FunctionValue,
) {
    println!("{}", fun.ident());

    let builder = context.create_builder();
    let mut ctx = LowerCtx {
        context: context,
        module: module,
        builder: &builder,
        fn_val: fn_val,
        protos: protos,
        nif_types: nif_types,
        ebb_map: HashMap::new(),
        phi_map: HashMap::new(),
        val_map: HashMap::new(),
        loc_id: loc_id,
        env: None,
        eir_mod: eir_mod,
    };

    let entry_bb = context.append_basic_block(&fn_val, "entry");
    let entry_ebb = fun.ebb_entry();

    // Add llvm bbs corresponding to the entry of eir ebbs.
    for ebb in fun.iter_ebb() {
        let bb = context.append_basic_block(&fn_val, &format!("{}", ebb));
        builder.position_at_end(&bb);
        ctx.ebb_map.insert(ebb, bb);
        for (idx, arg) in fun.ebb_args(ebb).iter().enumerate() {
            let phi = builder.build_phi(ctx.nif_types.term_type, &format!("{}", arg));
            ctx.phi_map.insert((ebb, idx), phi);
            ctx.val_map.insert(*arg, phi.as_basic_value());
        }
    }

    let ebb_args = fun.ebb_args(entry_ebb);

    // Arguments and entry bb
    builder.position_at_end(&entry_bb);
    let env = fn_val.get_nth_param(0).unwrap();
    ctx.env = Some(env);
    let mut arg_offset = 1;
    if fun.ident().lambda.is_none() {
        arg_offset = 2;
    }

    for (n, _arg) in ebb_args.iter().enumerate() {
        let value = fn_val.get_nth_param(n as u32 + arg_offset).unwrap();
        ctx.phi_map[&(entry_ebb, n)].add_incoming(&[(&value, &entry_bb)]);
    }

    if fun.has_attribute(&AttributeKey::Continuation) {
        let ebb_args = fun.ebb_args(entry_ebb);
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
        emit_ebb(&mut ctx, eir_mod, fun, ebb);
    }

}

