use std::collections::HashMap;

use crate::emit::LowerCtx;
use crate::emit::build_stack_arr;
use super::NifTypes;

use inkwell::types::BasicTypeEnum;
use inkwell::values::{ BasicValueEnum, IntValue, FunctionValue };
use inkwell::basic_block::BasicBlock;
use inkwell::IntPredicate;
use inkwell::AddressSpace;

use eir::Value;
use eir::FunctionIdent;
use eir::{ ConstantTerm, AtomicTerm };
use eir::op::ComparisonOperation;

use num_traits::cast::ToPrimitive;

use super::NifTarget;

impl crate::target::TargetEmit for NifTarget {

    fn emit_preamble(
        &mut self,
        ctx: &mut LowerCtx,
        args_out: &mut Vec<BasicValueEnum>,
    ) {
        let env = ctx.fn_val.get_nth_param(0).unwrap();
        self.env = Some(env);
        let mut arg_offset = 1;
        if ctx.eir_fun.ident().lambda.is_none() {
            arg_offset = 2;
        }

        let entry_ebb = ctx.eir_fun.ebb_entry();
        let ebb_args = ctx.eir_fun.ebb_args(entry_ebb);

        args_out.clear();
        for (n, _arg) in ebb_args.iter().enumerate() {
            let value = ctx.fn_val.get_nth_param(n as u32 + arg_offset).unwrap();
            args_out.push(value);
        }

    }

    fn emit_constant(
        &mut self,
        ctx: &mut LowerCtx,
        constant: &ConstantTerm,
    ) -> BasicValueEnum {
        match constant {
            ConstantTerm::Atomic(AtomicTerm::Integer(num)) => {
                let i64_type = ctx.context.i64_type();
                let num_const = i64_type.const_int(
                    num.to_i64().unwrap() as u64, true);
                let cs = ctx.builder.build_call(
                    self.types.enif_make_long,
                    &[self.env.unwrap(), num_const.into()],
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
                    self.types.enif_make_atom_len,
                    &[self.env.unwrap(), string_const_ptr.into(), string_len_val.into()],
                    "make_atom_len"
                );
                cs.try_as_basic_value().left().unwrap()
            }
            ConstantTerm::Atomic(AtomicTerm::Nil) => {
                let i32_type = ctx.context.i32_type();
                let call_ret = ctx.builder.build_call(
                    self.types.enif_make_list_from_array,
                    &[
                        self.env.unwrap(),
                        self.types.term_ptr_type.into_pointer_type()
                            .const_null().into(),
                        i32_type.const_int(0, false).into(),
                    ],
                    "",
                );
                call_ret.try_as_basic_value().left().unwrap()
            }
            _ => unimplemented!("{:?}", constant),
        }
    }

    fn type_for(
        &mut self,
        ctx: &mut LowerCtx,
        val: Value,
    ) -> BasicTypeEnum {
        self.types.term_type
    }

    fn emit_unreachable_fail(
        &mut self,
        ctx: &mut LowerCtx,
    ) {
        let i64_type = ctx.context.i64_type();
        ctx.builder.build_call(self.types.unreachable_fail, &[
            i64_type.const_int(*ctx.loc_id as u64, false).into(),
        ], "");
        *ctx.loc_id += 1;
        ctx.builder.build_unreachable();
    }

    fn emit_make_tuple(
        &mut self,
        ctx: &mut LowerCtx,
        values: &[BasicValueEnum],
    ) -> BasicValueEnum {
        let (arr_ptr, arr_len) = build_stack_arr(ctx, self.types.term_type, values);

        let call_ret = ctx.builder.build_call(
            self.types.enif_make_tuple_from_array,
            &[
                self.env.unwrap().into(),
                arr_ptr.into(),
                arr_len.into(),
            ],
            "",
        );
        call_ret.try_as_basic_value().left().unwrap()
    }

    fn emit_make_closure_env(
        &mut self,
        ctx: &mut LowerCtx,
        vars: &[BasicValueEnum]
    ) -> BasicValueEnum
    {
        self.emit_make_tuple(ctx, &vars)
    }

    fn emit_unpack_tuple(
        &mut self,
        ctx: &mut LowerCtx,
        tuple_val: BasicValueEnum,
        arity: usize,
        out_vals: &mut Vec<BasicValueEnum>,
    ) -> (BasicBlock, BasicBlock) {
        let i32_type = ctx.context.i32_type();
        let i64_type = ctx.context.i64_type();

        let values_len_ptr = ctx.builder.build_alloca(
            i32_type, "");
        let values_arr_ptr = ctx.builder.build_alloca(
            self.types.term_ptr_type, "");

        let call_ret = ctx.builder.build_call(
            self.types.enif_get_tuple,
            &[
                self.env.unwrap().into(),
                tuple_val.into(),
                values_len_ptr.into(),
                values_arr_ptr.into(),
            ],
            "",
        );

        let err_bb = ctx.context.append_basic_block(&ctx.fn_val, "tuple_unpack_err");
        let int_ok_bb = ctx.context.append_basic_block(&ctx.fn_val, "tuple_unpack_ok");
        let ok_bb = ctx.context.append_basic_block(&ctx.fn_val, "tuple_arity_ok");

        // Check return true
        let cmp_res = ctx.builder.build_int_compare(
            IntPredicate::EQ,
            call_ret.try_as_basic_value().left().unwrap().into_int_value(),
            i32_type.const_int(1, false).into(),
            "",
        );
        ctx.builder.build_conditional_branch(cmp_res, &int_ok_bb, &err_bb);

        // Check arity
        ctx.builder.position_at_end(&int_ok_bb);
        let arity_val = ctx.builder.build_load(values_len_ptr, "");
        let cmp_res = ctx.builder.build_int_compare(
            IntPredicate::EQ,
            arity_val.into_int_value(),
            i32_type.const_int(arity as u64, false).into(),
            "",
        );
        ctx.builder.build_conditional_branch(cmp_res, &ok_bb, &err_bb);

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

        (ok_bb, err_bb)
    }

    fn emit_unpack_closure_env(
        &mut self,
        ctx: &mut LowerCtx,
        env_val: BasicValueEnum,
        out: &mut Vec<BasicValueEnum>,
        num_free: usize,
    ) {
        let i64_type = ctx.context.i64_type();

        let (ok_bb, err_bb) = self.emit_unpack_tuple(
            ctx,
            env_val,
            num_free,
            out,
        );

        ctx.builder.position_at_end(&err_bb);
        self.emit_unreachable_fail(ctx);
        //ctx.builder.build_call(self.types.unreachable_fail, &[
        //    i64_type.const_int(*ctx.loc_id as u64, false).into(),
        //], "");
        //*ctx.loc_id += 1;
        //ctx.builder.build_unreachable();

        ctx.builder.position_at_end(&ok_bb);
    }

    fn emit_capture_named_function(
        &mut self,
        ctx: &mut LowerCtx,
        ident: &FunctionIdent,
    ) -> BasicValueEnum
    {
        let i8_type = ctx.context.i8_type();
        let i32_type = ctx.context.i32_type();
        let i64_type = ctx.context.i64_type();

        let mut out_buf = vec![]; // TODO

        let fun_val = ctx.protos[ident];

        let arity = ident.arity + 3; // +3 for env, ok_cont, err_cont

        // First value in fun tuple: :native_fun
        let string_const = crate::primitives::make_c_string_const(
            ctx.context, ctx.module, "native_fun");
        let string_const_ptr = string_const.as_pointer_value()
            .const_cast(i8_type.ptr_type(AddressSpace::Generic));
        let native_fun_call = ctx.builder.build_call(
            self.types.enif_make_atom_len,
            &[
                self.env.unwrap().into(),
                string_const_ptr.into(),
                i64_type.const_int(10, false).into(),
            ],
            ""
        );
        let native_fun_term = native_fun_call.try_as_basic_value().left().unwrap();

        // Second value in fun tuple: Function ptr
        let fun_ptr_value = fun_val.as_global_value().as_pointer_value();
        let fun_ptr_value_casted = ctx.builder.build_pointer_cast(
            fun_ptr_value, self.types.i8_ptr.into_pointer_type(), "");
        let args: Vec<BasicValueEnum> = vec![
            self.env.unwrap().into(),
            fun_ptr_value_casted.into(),
            i32_type.const_int(arity as u64, false).into(),
        ];
        let call = ctx.builder.build_call(
            self.types.make_fun_ptr_res, &args, "");
        let fun_ptr_term = call.try_as_basic_value().left().unwrap();

        // Make tuple
        out_buf.clear();
        out_buf.push(native_fun_term);
        out_buf.push(fun_ptr_term);
        out_buf.push(native_fun_term);

        self.emit_make_tuple(ctx, &out_buf)
    }

    fn emit_bind_closure(
        &mut self,
        ctx: &mut LowerCtx,
        ident: &FunctionIdent,
        clo_env: BasicValueEnum,
    ) -> BasicValueEnum {
        let mut out_buf = vec![]; // TODO

        let i8_type = ctx.context.i8_type();
        let i32_type = ctx.context.i32_type();
        let i64_type = ctx.context.i64_type();

        let fun_val = ctx.protos[ident];

        assert!(ident.module == ctx.eir_mod.name);
        let mut arity = ctx.eir_mod.functions[ident].entry_arg_num();
        assert!(ident.lambda.is_some());
        //if ident.lambda.is_none() { arity += 1; }

        // First value in fun tuple: :native_fun
        let string_const = crate::primitives::make_c_string_const(
            ctx.context, ctx.module, "native_fun");
        let string_const_ptr = string_const.as_pointer_value()
            .const_cast(i8_type.ptr_type(AddressSpace::Generic));
        let native_fun_call = ctx.builder.build_call(
            self.types.enif_make_atom_len,
            &[
                self.env.unwrap().into(),
                string_const_ptr.into(),
                i64_type.const_int(10, false).into(),
            ],
            ""
        );
        let native_fun_term = native_fun_call.try_as_basic_value().left().unwrap();

        // Second value in fun tuple: Function ptr
        let fun_ptr_value = fun_val.as_global_value().as_pointer_value();
        let fun_ptr_value_casted = ctx.builder.build_pointer_cast(
            fun_ptr_value, self.types.i8_ptr.into_pointer_type(), "");
        let args: Vec<BasicValueEnum> = vec![
            self.env.unwrap().into(),
            fun_ptr_value_casted.into(),
            i32_type.const_int(arity as u64, false).into(),
        ];
        let call = ctx.builder.build_call(
            self.types.make_fun_ptr_res, &args, "");
        let fun_ptr_term = call.try_as_basic_value().left().unwrap();

        // Third value in fun tuple: Env tuple
        let env_tup_term = clo_env;

        // Make tuple
        out_buf.clear();
        out_buf.push(native_fun_term);
        out_buf.push(fun_ptr_term);
        out_buf.push(env_tup_term);

        self.emit_make_tuple(ctx, &out_buf)
    }

    fn emit_apply_cont(
        &mut self,
        ctx: &mut LowerCtx,
        cont: BasicValueEnum,
        res: BasicValueEnum,
    ) {
        ctx.builder.build_call(
            self.types.nifrt_cont_call,
            &[
                self.env.unwrap(),
                cont,
                res,
            ],
            "",
        );
    }

    fn emit_apply_norm(
        &mut self,
        ctx: &mut LowerCtx,
        fun: BasicValueEnum,
        args: &[BasicValueEnum],
    ) {
        let void_type = ctx.context.void_type();
        let i32_type = ctx.context.i32_type();

        let mut out_buf = Vec::new(); // TODO
        let mut typ_buf = Vec::new(); // TODO

        let call_ret = ctx.builder.build_call(
            self.types.nifrt_unpack_fun_ptr,
            &[
                self.env.unwrap(),
                i32_type.const_int(args.len() as u64+1, false).into(),
                fun,
            ],
            "",
        );

        let nil_atom = ctx.builder.build_load(
            self.types.nifrt_atom_nil.as_pointer_value(), "");

        out_buf.clear();
        typ_buf.clear();
        out_buf.push(self.env.unwrap());
        typ_buf.push(self.types.env_ptr_type);
        out_buf.push(nil_atom);
        typ_buf.push(self.types.term_type);
        for v in args.iter() {
            out_buf.push(*v);
            typ_buf.push(self.types.term_type);
        }

        let fun_typ = void_type.fn_type(&typ_buf, false);
        let fun_ptr_typ = fun_typ.ptr_type(AddressSpace::Generic);

        let casted_fun = ctx.builder.build_pointer_cast(
            call_ret.try_as_basic_value().left().unwrap()
                .into_pointer_value(),
            fun_ptr_typ,
            "",
        );

        ctx.builder.build_call(
            casted_fun,
            &out_buf,
            ""
        );
    }

    fn emit_call_const(
        &mut self,
        ctx: &mut LowerCtx,
        ident: &FunctionIdent,
        args: &[BasicValueEnum],
    ) {
        let nil_atom = ctx.builder.build_load(
            self.types.nifrt_atom_nil.as_pointer_value(), "");

        let mut f_args = vec![
            self.env.unwrap(),
            nil_atom, // Nil env
        ];
        f_args.extend(args.iter().cloned());

        let fun = ctx.protos[&ident];
        ctx.builder.build_call(
            fun,
            &f_args,
            "tail_call_const",
        );
    }

    fn emit_call_dyn(
        &mut self,
        _ctx: &mut LowerCtx,
        _module: BasicValueEnum,
        _name: BasicValueEnum,
        _args: &[BasicValueEnum],
    ) {
        unimplemented!()
    }

    fn emit_compare(
        &mut self,
        ctx: &mut LowerCtx,
        lhs: BasicValueEnum,
        rhs: BasicValueEnum,
        op: &ComparisonOperation,
    ) -> IntValue
    {
        let i32_type = ctx.context.i32_type();

        match op {
            ComparisonOperation::Equal => {
                let call = ctx.builder.build_call(
                    self.types.enif_compare,
                    &[
                        lhs,
                        rhs,
                    ],
                    "",
                );
                let res = call.try_as_basic_value().left().unwrap();

                ctx.builder.build_int_compare(
                    IntPredicate::EQ,
                    res.into_int_value(),
                    i32_type.const_int(0, false).into(),
                    "",
                )
            }
            _ => unimplemented!(),
        }
    }

    fn emit_is_truthy(
        &mut self,
        ctx: &mut LowerCtx,
        val: BasicValueEnum,
    ) -> IntValue {
        let nil_atom = ctx.builder.build_load(
            self.types.nifrt_atom_nil.as_pointer_value(), "");
        let a = ctx.builder.build_int_compare(
            IntPredicate::EQ,
            val.into_int_value(),
            nil_atom.into_int_value(),
            ""
        );
        let false_atom = ctx.builder.build_load(
            self.types.nifrt_atom_false.as_pointer_value(), "");
        let b = ctx.builder.build_int_compare(
            IntPredicate::EQ,
            val.into_int_value(),
            false_atom.into_int_value(),
            ""
        );

        let ored = ctx.builder.build_or(a, b, "");
        ctx.builder.build_not(ored, "")
    }

    fn emit_map_put(
        &mut self,
        ctx: &mut LowerCtx,
        map: BasicValueEnum,
        key: BasicValueEnum,
        val: BasicValueEnum,
        update: bool,
    ) -> (IntValue, BasicValueEnum)
    {
        let i32_type = ctx.context.i32_type();

        let n_fun = if update {
            self.types.enif_make_map_update
        } else {
            self.types.enif_make_map_put
        };

        let ret_map_val = ctx.builder.build_alloca(self.types.term_type, "");
        let call_ret = ctx.builder.build_call(
            n_fun,
            &[
                self.env.unwrap(),
                map,
                key,
                val,
                ret_map_val.into(),
            ],
            ""
        );

        // Check return true
        let cmp_res = ctx.builder.build_int_compare(
            IntPredicate::EQ,
            call_ret.try_as_basic_value().left().unwrap().into_int_value(),
            i32_type.const_int(1, false).into(),
            "",
        );

        let ret_val = ctx.builder.build_load(ret_map_val, "");
        (cmp_res, ret_val)
    }

}
