use crate::emit::LowerCtx;

use inkwell::types::BasicTypeEnum;
use inkwell::values::{ BasicValueEnum, IntValue, FunctionValue };
use inkwell::basic_block::BasicBlock;

use eir::Value;
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
        unimplemented!();
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
        unimplemented!();
    }

    fn emit_unpack_tuple(
        &mut self,
        ctx: &mut LowerCtx,
        tuple_val: BasicValueEnum,
        arity: usize,
        out_vals: &mut Vec<BasicValueEnum>,
        err_bb: &BasicBlock,
    ) -> BasicBlock {
        unimplemented!();
    }

    fn emit_unpack_closure_env(
        &mut self,
        ctx: &mut LowerCtx,
        env_val: BasicValueEnum,
        out: &mut Vec<BasicValueEnum>,
        num_free: usize,
    ) {
        unimplemented!();
    }

    fn emit_capture_named_function(
        &mut self,
        ctx: &mut LowerCtx,
        ident: &FunctionIdent,
    ) -> BasicValueEnum
    {
        unimplemented!();
    }

    fn emit_bind_closure(
        &mut self,
        ctx: &mut LowerCtx,
        ident: &FunctionIdent,
        clo_env: BasicValueEnum,
    ) -> BasicValueEnum {
        unimplemented!();
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
        unimplemented!();
    }

    fn emit_call_const(
        &mut self,
        ctx: &mut LowerCtx,
        ident: &FunctionIdent,
        args: &[BasicValueEnum],
    ) {
        unimplemented!();
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
        unimplemented!();
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
