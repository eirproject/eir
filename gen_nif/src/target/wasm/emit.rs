use crate::emit::LowerCtx;

use inkwell::AddressSpace;
use inkwell::IntPredicate;
use inkwell::types::BasicTypeEnum;
use inkwell::values::{ BasicValueEnum, IntValue, FunctionValue };
use inkwell::basic_block::BasicBlock;

use eir::Value;
use eir::Atom;
use eir::FunctionIdent;
use eir::{ ConstantTerm, AtomicTerm };
use eir::op::ComparisonOperation;

use super::WasmTarget;

impl crate::target::TargetEmit for WasmTarget {

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
            ConstantTerm::Atomic(AtomicTerm::Atom(atom)) => {
                let atom_ptr = self.get_atom(&ctx.module, atom);
                let atom_val = ctx.builder.build_load(atom_ptr, "");
                atom_val
            }
            ConstantTerm::Atomic(AtomicTerm::Integer(num)) => {
                let i64_type = ctx.context.i64_type();

                // TODO: Handle large
                use num_traits::cast::ToPrimitive;
                let num_sm = num.to_i64().unwrap();

                let num_const = i64_type.const_int(num_sm as u64, true);

                let call = ctx.builder.build_call(
                    self.types.whirlrt_term_make_smallint,
                    &[
                        self.env.unwrap(),
                        num_const.into(),
                    ],
                    ""
                );

                call.try_as_basic_value().left().unwrap()
            },
            _ => unimplemented!("emit_constant {:?}", constant),
        }
    }

    fn type_for(
        &mut self,
        _ctx: &mut LowerCtx,
        _val: Value,
    ) -> BasicTypeEnum {
        self.types.term_type
    }

    fn emit_unreachable_fail(
        &mut self,
        ctx: &mut LowerCtx,
    ) {
        ctx.builder.build_call(
            self.types.whirlrt_unreachable_fail,
            &[
                self.env.unwrap(),
            ],
            "unreachable_fail",
        );
        ctx.builder.build_unreachable();
    }

    fn emit_make_tuple(
        &mut self,
        ctx: &mut LowerCtx,
        values: &[BasicValueEnum],
    ) -> BasicValueEnum {
        let (arr_ptr, arr_len) = crate::emit::build_stack_arr(
            ctx, self.types.term_type, values);

        let call = ctx.builder.build_call(
            self.types.whirlrt_term_make_tuple,
            &[
                self.env.unwrap(),
                arr_len.into(),
                arr_ptr.into(),
            ],
            "",
        );
        call.try_as_basic_value().left().unwrap()
    }

    fn emit_make_closure_env(
        &mut self,
        ctx: &mut LowerCtx,
        vars: &[BasicValueEnum]
    ) -> BasicValueEnum
    {
        // TODO: MakeClosureEnv and BindClosure should be made into a
        // single OP!!
        // This is really really hacky!
        let (arr_ptr, arr_len) = crate::emit::build_stack_arr(
            ctx, self.types.term_type, vars);

        let call = ctx.builder.build_call(
            self.types.whirlrt_term_make_tuple,
            &[
                self.env.unwrap(),
                arr_len.into(),
                arr_ptr.into(),
            ],
            "",
        );
        call.try_as_basic_value().left().unwrap()
    }

    fn emit_unpack_tuple(
        &mut self,
        ctx: &mut LowerCtx,
        tuple_val: BasicValueEnum,
        arity: usize,
        out: &mut Vec<BasicValueEnum>,
    ) -> (BasicBlock, BasicBlock) {
        let i32_type = ctx.context.i32_type();
        let buf_len = i32_type.const_int(arity as u64, false);
        let ret_buf = ctx.builder.build_array_alloca(
            self.types.term_type,
            buf_len.into(),
            ""
        );
        let call = ctx.builder.build_call(
            self.types.whirlrt_term_unpack_tuple,
            &[
                self.env.unwrap(),
                tuple_val,
                buf_len.into(),
                ret_buf.into(),
            ],
            ""
        );

        let call_ret = call.try_as_basic_value().left().unwrap();

        let ok_bb = ctx.fn_val.append_basic_block("unpack_tuple_ok");
        let err_bb = ctx.fn_val.append_basic_block("unpack_tuple_err");
        ctx.builder.build_conditional_branch(
            call_ret.into_int_value(),
            &ok_bb,
            &err_bb,
        );

        ctx.builder.position_at_end(&ok_bb);
        out.clear();
        for n in 0..arity {
            let n_val = i32_type.const_int(n as u64, false);
            let ptr = unsafe { ctx.builder.build_gep(ret_buf, &[n_val], "") };
            let val = ctx.builder.build_load(ptr, "");
            out.push(val);
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
        let i32_type = ctx.context.i32_type();
        let buf_len = i32_type.const_int(num_free as u64, false);
        let ret_buf = ctx.builder.build_array_alloca(
            self.types.term_type,
            buf_len.into(),
            ""
        );
        ctx.builder.build_call(
            self.types.whirlrt_term_unpack_closure_env,
            &[
                self.env.unwrap(),
                env_val,
                buf_len.into(),
                ret_buf.into(),
            ],
            ""
        );
        out.clear();
        for n in 0..num_free {
            let n_val = i32_type.const_int(n as u64, false);
            let ptr = unsafe { ctx.builder.build_gep(ret_buf, &[n_val], "") };
            let val = ctx.builder.build_load(ptr, "");
            out.push(val);
        }
    }

    fn emit_capture_named_function(
        &mut self,
        ctx: &mut LowerCtx,
        ident: &FunctionIdent,
    ) -> BasicValueEnum
    {
        let i8_type = self.context.i8_type();

        let fun_val = ctx.protos[ident];
        let fun_ptr_value = fun_val.as_global_value().as_pointer_value();
        let fun_ptr_value_casted = ctx.builder.build_pointer_cast(
            fun_ptr_value, i8_type.ptr_type(AddressSpace::Generic), "");

        let call = ctx.builder.build_call(
            self.types.whirlrt_term_make_fun,
            &[
                self.env.unwrap(),
                fun_ptr_value_casted.into(),
            ],
            ""
        );
        call.try_as_basic_value().left().unwrap()
    }

    fn emit_bind_closure(
        &mut self,
        ctx: &mut LowerCtx,
        ident: &FunctionIdent,
        clo_env: BasicValueEnum,
    ) -> BasicValueEnum {
        let fun_val = ctx.protos[ident];

        let i8_type = self.context.i8_type();

        let fun_ptr_value = fun_val.as_global_value().as_pointer_value();
        let fun_ptr_value_casted = ctx.builder.build_pointer_cast(
            fun_ptr_value, i8_type.ptr_type(AddressSpace::Generic), "");

        ctx.builder.build_call(
            self.types.whirlrt_temp_hacky_transmute_tup_to_fun_env,
            &[
                self.env.unwrap(),
                clo_env,
                fun_ptr_value_casted.into(),
            ],
            ""
        );
        clo_env
    }

    fn emit_apply_cont(
        &mut self,
        ctx: &mut LowerCtx,
        cont: BasicValueEnum,
        res: BasicValueEnum,
    ) {
        ctx.builder.build_call(
            self.types.whirlrt_call_cont,
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

        // TODO fail properly if value is not a function
        let call = ctx.builder.build_call(
            self.types.whirlrt_term_get_fun,
            &[
                self.env.unwrap(),
                fun,
            ],
            ""
        );
        let fun_ptr_uncasted = call.try_as_basic_value().left().unwrap();

        let mut args_typ = vec![
            self.types.process_env_ptr_type,
            self.types.term_type,
        ];
        args_typ.extend((0..(args.len())).map(|_| self.types.term_type));
        let fun_typ = void_type.fn_type(&args_typ, false);
        let fun_ptr_typ = fun_typ.ptr_type(AddressSpace::Generic);

        let fun_ptr = ctx.builder.build_pointer_cast(
            fun_ptr_uncasted.into_pointer_value(), fun_ptr_typ, "");

        let mut c_args = vec![
            self.env.unwrap(),
            fun,
        ];
        c_args.extend(args.iter().cloned());
        ctx.builder.build_call(
            fun_ptr,
            &c_args,
            ""
        );
    }

    fn emit_call_const(
        &mut self,
        ctx: &mut LowerCtx,
        ident: &FunctionIdent,
        args: &[BasicValueEnum],
    ) {
        let i8_type = ctx.context.i8_type();

        println!("ConstCall: {:?}", ident);
        for key in ctx.protos.keys() {
            println!("{}", key);
        }
        let fun_val = ctx.protos[ident];

        let fun_ptr_value = fun_val.as_global_value().as_pointer_value();
        let fun_ptr_value_casted = ctx.builder.build_pointer_cast(
            fun_ptr_value, i8_type.ptr_type(AddressSpace::Generic), "");

        let call = ctx.builder.build_call(
            self.types.whirlrt_term_make_fun,
            &[
                self.env.unwrap(),
                fun_ptr_value_casted.into(),
            ],
            ""
        );

        let mut c_args = vec![
            self.env.unwrap(),
            call.try_as_basic_value().left().unwrap(),
        ];
        c_args.extend(args.iter().cloned());
        ctx.builder.build_call(
            fun_val,
            &c_args,
            ""
        );
    }

    fn emit_call_dyn(
        &mut self,
        _ctx: &mut LowerCtx,
        _module: BasicValueEnum,
        _name: BasicValueEnum,
        _args: &[BasicValueEnum],
    ) {
        unimplemented!();
    }

    fn emit_compare(
        &mut self,
        ctx: &mut LowerCtx,
        lhs: BasicValueEnum,
        rhs: BasicValueEnum,
        op: &ComparisonOperation,
    ) -> IntValue
    {
        match op {
            ComparisonOperation::Equal => {
                let call = ctx.builder.build_call(
                    self.types.whirlrt_term_eq,
                    &[
                        self.env.unwrap(),
                        lhs,
                        rhs,
                    ],
                    "",
                );
                call.try_as_basic_value().left().unwrap()
                    .into_int_value()
            }
            _ => unimplemented!(),
        }
    }

    fn emit_is_truthy(
        &mut self,
        ctx: &mut LowerCtx,
        val: BasicValueEnum,
    ) -> IntValue {
        let nil_atom_ptr = self.get_atom(&ctx.module, &Atom::from_str("nil"));
        let nil_atom = ctx.builder.build_load(nil_atom_ptr, "");
        let false_atom_ptr = self.get_atom(&ctx.module, &Atom::from_str("false"));
        let false_atom = ctx.builder.build_load(false_atom_ptr, "");

        let a = ctx.builder.build_int_compare(
            IntPredicate::EQ, val.into_int_value(), nil_atom.into_int_value(), "");
        let b = ctx.builder.build_int_compare(
            IntPredicate::EQ, val.into_int_value(), false_atom.into_int_value(), "");

        let comb = ctx.builder.build_or(a, b, "");
        ctx.builder.build_not(comb, "")
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
        unimplemented!();
    }

}
