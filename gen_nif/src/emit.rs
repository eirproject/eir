use std::collections::HashMap;

use inkwell::AddressSpace;
use inkwell::IntPredicate;
use inkwell::context::Context;
use inkwell::values::{ FunctionValue, ArrayValue, StructValue, PointerValue,
                       GlobalValue, AnyValueEnum, BasicValueEnum, PhiValue };
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

struct LowerCtx<'a> {
    context: &'a Context,
    module: &'a Module,
    builder: &'a Builder,
    fn_val: FunctionValue,
    protos: &'a HashMap<FunctionIdent, FunctionValue>,
    nif_types: &'a NifTypes,
    ebb_map: HashMap<Ebb, BasicBlock>,
    phi_map: HashMap<(Ebb, usize), PhiValue>,
    val_map: HashMap<Value, BasicValueEnum>,
    loc_id: &'a mut u64,
}

fn emit_read(
    ctx: &mut LowerCtx,
    env: BasicValueEnum,
    fun: &EirFunction,
    read: Value,
) -> BasicValueEnum
{
    let data = fun.value(read);
    match data {
        ValueType::Variable => ctx.val_map[&read],
        ValueType::Constant(constant) => {
            match constant {
                ConstantTerm::Atomic(AtomicTerm::Integer(num)) => {
                    let i64_type = ctx.context.i64_type();
                    let num_const = i64_type.const_int(
                        num.to_i64().unwrap() as u64, true);
                    let cs = ctx.builder.build_call(
                        ctx.nif_types.enif_make_long,
                        &[env, num_const.into()],
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
                        &[env, string_const_ptr.into(), string_len_val.into()],
                        "make_atom_len"
                    );
                    cs.try_as_basic_value().left().unwrap()
                }
                _ => unimplemented!("{:?}", constant),
            }
        },
    }
}

fn build_make_tuple(
    ctx: &mut LowerCtx,
    env: BasicValueEnum,
    values: &[BasicValueEnum],
) -> BasicValueEnum {
    let i32_type = ctx.context.i32_type();
    let i64_type = ctx.context.i64_type();

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

    let call_ret = ctx.builder.build_call(
        ctx.nif_types.enif_make_tuple_from_array,
        &[
            env.into(),
            values_arr.into(),
            values_len.into(),
        ],
        "",
    );
    call_ret.try_as_basic_value().left().unwrap()
}

fn build_unpack_tuple(
    ctx: &mut LowerCtx,
    env: BasicValueEnum,
    tuple_val: BasicValueEnum,
    arity: usize,
    out_vals: &mut Vec<BasicValueEnum>,
    err_bb: &BasicBlock,
) -> BasicBlock {
    let i32_type = ctx.context.i32_type();
    let i64_type = ctx.context.i64_type();

    let values_len_ptr = ctx.builder.build_alloca(
        i32_type, "");
    let values_arr_ptr = ctx.builder.build_alloca(
        ctx.nif_types.term_ptr_type, "");

    let call_ret = ctx.builder.build_call(
        ctx.nif_types.enif_get_tuple,
        &[
            env.into(),
            tuple_val.into(),
            values_len_ptr.into(),
            values_arr_ptr.into(),
        ],
        "",
    );

    let int_ok_bb = ctx.context.append_basic_block(&ctx.fn_val, "tuple_unpack_ok");
    let ok_bb = ctx.context.append_basic_block(&ctx.fn_val, "tuple_arity_ok");

    // Check return true
    let cmp_res = ctx.builder.build_int_compare(
        IntPredicate::EQ,
        call_ret.try_as_basic_value().left().unwrap().into_int_value(),
        i32_type.const_int(1, false).into(),
        "",
    );
    ctx.builder.build_conditional_branch(cmp_res, &int_ok_bb, err_bb);

    // Check arity
    ctx.builder.position_at_end(&int_ok_bb);
    let arity_val = ctx.builder.build_load(values_len_ptr, "");
    let cmp_res = ctx.builder.build_int_compare(
        IntPredicate::EQ,
        arity_val.into_int_value(),
        i32_type.const_int(arity as u64, false).into(),
        "",
    );
    ctx.builder.build_conditional_branch(cmp_res, &ok_bb, err_bb);

    ctx.builder.position_at_end(&ok_bb);
    let values_arr = ctx.builder.build_load(values_arr_ptr, "");

    out_vals.clear();
    for idx in 0..arity {
        let elem_ptr = unsafe {
            ctx.builder.build_gep(
                values_arr.into_pointer_value(),
                &[i64_type.const_int(idx as u64, false)],
                "",
            )
        };
        let term = ctx.builder.build_load(elem_ptr, "");
        out_vals.push(term);
    }

    ok_bb
}

fn add_branch_phis(
    ctx: &mut LowerCtx,
    env: BasicValueEnum,
    fun: &EirFunction,
    current: &BasicBlock,
    target: Ebb,
    in_args: &[Value],
) {
    for (idx, src_arg) in in_args.iter().enumerate() {
        let phi = ctx.phi_map[&(target, idx)];
        println!("BranchPhiRead {:?}", src_arg);
        //let val = ctx.val_map[src_arg];
        let val = emit_read(ctx, env, fun, *src_arg);
        phi.add_incoming(&[(&val, current)]);
    }
}

fn debug_printf(ctx: &mut LowerCtx, text: &str) {
    let string_const = crate::primitives::make_c_string_const(
        ctx.context, ctx.module, text);
    let string_const_ptr = string_const.as_pointer_value()
        .const_cast(ctx.nif_types.i8_ptr.into_pointer_type());
    ctx.builder.build_call(
        ctx.nif_types.printf, &[string_const_ptr.into()], "debug print");
}

fn emit_op(
    ctx: &mut LowerCtx,
    env: BasicValueEnum,
    eir_mod: &EirModule,
    fun: &EirFunction,
    op: Op,
) {
    let mut read_buf = Vec::new();
    let mut out_buf = Vec::new();

    let i8_type = ctx.context.i8_type();
    let i32_type = ctx.context.i32_type();
    let i64_type = ctx.context.i64_type();

    // Reads
    read_buf.clear();
    for read in fun.op_reads(op) {
        let value = emit_read(
            ctx,
            env, fun,
            *read,
        );
        read_buf.push(value);
    }

    let reads = fun.op_reads(op);
    let writes = fun.op_writes(op);
    let branches = fun.op_branches(op);

    match fun.op_kind(op) {
        OpKind::MakeClosureEnv { .. } => {
            debug_printf(ctx, "MakeClosureEnv\n");
            assert!(writes.len() == 1);
            let value_enum = build_make_tuple(ctx, env, &read_buf);
            ctx.val_map.insert(writes[0], value_enum);
        },
        OpKind::UnpackEnv { .. } => {
            debug_printf(ctx, "UnpackEnv\n");
            let err_bb = ctx.context.append_basic_block(&ctx.fn_val, "env_unpack_err");

            let ok_bb = build_unpack_tuple(
                ctx,
                env,
                read_buf[0],
                writes.len(),
                &mut out_buf,
                &err_bb,
            );

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
        OpKind::BindClosure { ident } => {
            debug_printf(ctx, "BindClosure\n");
            assert!(writes.len() == 1);
            let fun_val = ctx.protos[ident];

            assert!(ident.module == eir_mod.name);
            let mut arity = eir_mod.functions[ident].entry_arg_num();
            if ident.lambda.is_none() { arity += 1; }

            // First value in fun tuple: :native_fun
            let string_const = crate::primitives::make_c_string_const(
                ctx.context, ctx.module, "native_fun");
            let string_const_ptr = string_const.as_pointer_value()
                .const_cast(i8_type.ptr_type(AddressSpace::Generic));
            let native_fun_call = ctx.builder.build_call(
                ctx.nif_types.enif_make_atom_len,
                &[
                    env.into(),
                    string_const_ptr.into(),
                    i64_type.const_int(10, false).into(),
                ],
                ""
            );
            let native_fun_term = native_fun_call.try_as_basic_value().left().unwrap();

            // Second value in fun tuple: Function ptr
            let fun_ptr_value = fun_val.as_global_value().as_pointer_value();
            let fun_ptr_value_casted = ctx.builder.build_pointer_cast(
                fun_ptr_value, ctx.nif_types.i8_ptr.into_pointer_type(), "");
            let args: Vec<BasicValueEnum> = vec![
                env.into(),
                fun_ptr_value_casted.into(),
                i32_type.const_int(arity as u64, false).into(),
            ];
            let call = ctx.builder.build_call(
                ctx.nif_types.make_fun_ptr_res, &args, "");
            let fun_ptr_term = call.try_as_basic_value().left().unwrap();

            // Third value in fun tuple: Env tuple
            let env_tup_term = read_buf[0];

            // Make tuple
            out_buf.clear();
            out_buf.push(native_fun_term);
            out_buf.push(fun_ptr_term);
            out_buf.push(env_tup_term);

            let fun_tuple = build_make_tuple(ctx, env, &out_buf);
            ctx.val_map.insert(writes[0], fun_tuple);
        },
        OpKind::Jump => {
            debug_printf(ctx, "Jump\n");
            assert!(writes.len() == 0);
            let ebb_call = branches[0];
            let target = fun.ebb_call_target(ebb_call);
            let from_bb = ctx.builder.get_insert_block().unwrap();
            add_branch_phis(ctx, env, fun, &from_bb, target,
                            fun.ebb_call_args(ebb_call));
            ctx.builder.build_unconditional_branch(&ctx.ebb_map[&target]);
        },
        OpKind::Move => {
            assert!(writes.len() == 1);
            ctx.val_map.insert(writes[0], read_buf[0]);
        },
        OpKind::PackValueList => {
            debug_printf(ctx, "PackValueList\n");
            assert!(writes.len() == 1);
            let value_enum = build_make_tuple(ctx, env, &read_buf);
            ctx.val_map.insert(writes[0], value_enum);
        },
        OpKind::UnpackValueList => {
            debug_printf(ctx, "UnpackValueList\n");
            let err_bb = ctx.context.append_basic_block(&ctx.fn_val, "val_unpack_err");

            let ok_bb = build_unpack_tuple(ctx, env, read_buf[0], writes.len(),
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
            debug_printf(ctx, "Apply\n");
            if *call_type == CallType::Cont {
                assert!(reads.len() == 2);
                ctx.builder.build_call(
                    ctx.nif_types.nifrt_cont_call,
                    &[
                        env.into(),
                        read_buf[0],
                        read_buf[1],
                    ],
                    "",
                );
            } else {
                unimplemented!()
            }
            ctx.builder.build_unreachable();
        },
        OpKind::Call { arity, call_type } => {
            assert!(writes.len() == 0);
            assert!(*call_type == CallType::Tail);
            debug_printf(ctx, "Call\n");
            if fun.value_is_constant(reads[0]) && fun.value_is_constant(reads[1]) {
                let module = fun.value_constant(reads[0]).atom().unwrap();
                let name = fun.value_constant(reads[1]).atom().unwrap();
                let ident = FunctionIdent {
                    module: module.clone(),
                    name: name.clone(),
                    lambda: None,
                    arity: *arity,
                };

                let mut args = vec![
                    env,
                    read_buf[0], // TODO
                ];
                args.extend(read_buf[2..].iter().cloned());

                let fun = ctx.protos[&ident];
                ctx.builder.build_call(
                    fun,
                    &args,
                    ""
                );
            } else {
                unimplemented!()
            }
            ctx.builder.build_unreachable();
        },
        OpKind::ComparisonOperation(ComparisonOperation::Equal) => {
            assert!(writes.len() == 0);
            debug_printf(ctx, "ComparisonOperation\n");
            let call = ctx.builder.build_call(
                ctx.nif_types.enif_compare,
                &[
                    read_buf[0],
                    read_buf[1],
                ],
                "",
            );
            let res = call.try_as_basic_value().left().unwrap();

            let cmp_res = ctx.builder.build_int_compare(
                IntPredicate::EQ,
                res.into_int_value(),
                i32_type.const_int(0, false).into(),
                "",
            );

            let fail_call = branches[0];
            let fail_target = fun.ebb_call_target(fail_call);

            let from_bb = ctx.builder.get_insert_block().unwrap();

            let ok_bb = ctx.context.append_basic_block(&ctx.fn_val, "compare_eq_ok");
            ctx.builder.build_conditional_branch(cmp_res, &ok_bb, &ctx.ebb_map[&fail_target]);

            add_branch_phis(ctx, env, fun, &from_bb, fail_target, fun.ebb_call_args(fail_call));

            ctx.builder.position_at_end(&ok_bb);
        },
        OpKind::MakeNoValue => {
            assert!(writes.len() == 1);
            println!("NoValue {:?}", writes);
        },
        kind => unimplemented!("{:?}", kind),
    }

}

fn emit_ebb(
    ctx: &mut LowerCtx,
    env: BasicValueEnum,
    eir_mod: &EirModule,
    fun: &EirFunction,
    ebb: Ebb,
) {

    let bb = &ctx.ebb_map[&ebb];
    ctx.builder.position_at_end(bb);

    for op in fun.iter_op(ebb) {
        emit_op(ctx, env, eir_mod, fun, op);
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

    let string_const = crate::primitives::make_c_string_const(
        ctx.context, ctx.module, &format!("Entry!\n"));
    let string_const_ptr = string_const.as_pointer_value()
        .const_cast(ctx.nif_types.i8_ptr.into_pointer_type());
    ctx.builder.build_call(
        ctx.nif_types.printf, &[string_const_ptr.into()], "debug print");

    builder.build_unconditional_branch(&ctx.ebb_map[&entry_ebb]);

    // ebbs in eir ir
    for ebb in fun.iter_ebb() {
        emit_ebb(&mut ctx, env, eir_mod, fun, ebb);
    }

}

