use crate::Data;
use eir::FunctionIdent;
use inkwell::values::FunctionValue;

pub mod nif;
pub mod wasm;

use crate::emit::LowerCtx;

use eir::Value;
use eir::Function as EirFunction;
use eir::{ ConstantTerm, AtomicTerm };
use eir::op::ComparisonOperation;

use inkwell::values::BasicValueEnum;
use inkwell::basic_block::BasicBlock;
use inkwell::values::IntValue;
use inkwell::types::BasicTypeEnum;
use inkwell::context::Context;
use inkwell::module::Module;

pub use self::nif::NifTarget;
pub use self::wasm::WasmTarget;

pub trait Target: TargetEmit {
    type Options;

    fn new(
        context: Context,
        module: &Module,
        options: Self::Options,
    ) -> Self;

    fn gen_prototype(
        &mut self,
        data: &mut Data,
        fun: &EirFunction
    ) -> FunctionValue;

    fn gen_prototype_pub(
        &mut self,
        data: &mut Data,
        ident: &FunctionIdent
    ) -> FunctionValue;

    fn finalize(
        &mut self,
        data: &mut Data,
    );

}

pub trait TargetEmit {

    fn emit_preamble(
        &mut self,
        ctx: &mut LowerCtx,
        args_out: &mut Vec<BasicValueEnum>,
    );

    fn emit_constant(
        &mut self,
        ctx: &mut LowerCtx,
        constant: &ConstantTerm,
    ) -> BasicValueEnum;

    fn type_for(
        &mut self,
        ctx: &mut LowerCtx,
        val: Value,
    ) -> BasicTypeEnum;

    fn emit_unreachable_fail(
        &mut self,
        ctx: &mut LowerCtx,
    );

    fn emit_make_tuple(
        &mut self,
        ctx: &mut LowerCtx,
        values: &[BasicValueEnum],
    ) -> BasicValueEnum;

    fn emit_make_closure_env(
        &mut self,
        ctx: &mut LowerCtx,
        vars: &[BasicValueEnum]
    ) -> BasicValueEnum;

    fn emit_unpack_tuple(
        &mut self,
        ctx: &mut LowerCtx,
        tuple_val: BasicValueEnum,
        arity: usize,
        out_vals: &mut Vec<BasicValueEnum>,
    ) -> (BasicBlock, BasicBlock);

    fn emit_unpack_closure_env(
        &mut self,
        ctx: &mut LowerCtx,
        env_val: BasicValueEnum,
        out: &mut Vec<BasicValueEnum>,
        num_free: usize,
    );

    fn emit_capture_named_function(
        &mut self,
        ctx: &mut LowerCtx,
        ident: &FunctionIdent,
    ) -> BasicValueEnum;

    fn emit_bind_closure(
        &mut self,
        ctx: &mut LowerCtx,
        ident: &FunctionIdent,
        clo_env: BasicValueEnum,
    ) -> BasicValueEnum;

    fn emit_apply_cont(
        &mut self,
        ctx: &mut LowerCtx,
        cont: BasicValueEnum,
        res: BasicValueEnum,
    );

    fn emit_apply_norm(
        &mut self,
        ctx: &mut LowerCtx,
        fun: BasicValueEnum,
        args: &[BasicValueEnum],
    );

    fn emit_call_const(
        &mut self,
        ctx: &mut LowerCtx,
        ident: &FunctionIdent,
        args: &[BasicValueEnum],
    );

    fn emit_call_dyn(
        &mut self,
        _ctx: &mut LowerCtx,
        _module: BasicValueEnum,
        _name: BasicValueEnum,
        _args: &[BasicValueEnum],
    );

    fn emit_compare(
        &mut self,
        ctx: &mut LowerCtx,
        lhs: BasicValueEnum,
        rhs: BasicValueEnum,
        op: &ComparisonOperation,
    ) -> IntValue;

    fn emit_is_truthy(
        &mut self,
        ctx: &mut LowerCtx,
        val: BasicValueEnum,
    ) -> IntValue;

    fn emit_map_put(
        &mut self,
        ctx: &mut LowerCtx,
        map: BasicValueEnum,
        key: BasicValueEnum,
        val: BasicValueEnum,
        update: bool,
    ) -> (IntValue, BasicValueEnum);

}
