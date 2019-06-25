use std::collections::HashMap;

use libeir_ir::{
    Module as IrModule,
    FunctionBuilder,
    Value as IrValue,
    Block as IrBlock,
    IntoValue,
};
use libeir_ir::pattern::{
    PatternClause,
    PatternNode,
    PatternMergeFail,
};
use libeir_ir::constant::{ ConstantContainer, IntoConst, Const };

use libeir_diagnostics::{ ByteSpan, ByteIndex, DUMMY_SPAN };
use libeir_intern::{ Symbol, Ident };

use libeir_util::hashmap_stack::HashMapStack;

use cranelift_entity::{ ListPool, EntityList };

use crate::parser::ast::{ Module, ResolvedFunctionName, NamedFunction, Function, FunctionClause, Expr, Guard };
use crate::parser::ast::{ Literal, BinaryExpr, BinaryOp, UnaryExpr, UnaryOp };

macro_rules! map_block {
    ($block:ident, $call:expr) => {
        {
            let (block, val) = $call;
            $block = block;
            val
        }
    }
}

mod pattern;
use pattern::lower_clause;

mod expr;
use expr::lower_block;

mod errors;
use errors::LowerError;

mod exception_handler_stack;
use exception_handler_stack::ExceptionHandlerStack;

mod scope;
use scope::ScopeToken;

#[cfg(test)]
mod tests;

// Actual error info is located in context, this is only applicable when we
// encounter something we don't know how to proceed from and need to bail.
type LowerResult<T> = Result<T, ()>;

struct LowerCtx<'a> {
    module: &'a Module,

    scope: scope::ScopeTracker,
    exc_stack: ExceptionHandlerStack,

    sentinel_value: Option<IrValue>,

    errors: Vec<LowerError>,
    failed: bool,
}

impl<'a> LowerCtx<'a> {

    /// Since we want to catch as many errors as possible in a single
    /// compiler invocation, we frequently purposefully generate invalid
    /// IR so that the lowering process can continue.
    /// In a case where, say, we have an unresolved variable, we need
    /// a dummy value that we can use.
    /// If this value is used in the resulting IR, it is REQUIRED that
    /// `failed` be set to true.
    pub fn sentinel(&self) -> IrValue {
        assert!(self.failed);
        self.sentinel_value.unwrap()
    }

    pub fn error(&mut self, err: LowerError) {
        self.failed = true;
        self.errors.push(err);
    }

    pub fn resolve(&mut self, ident: Ident) -> IrValue {
        match self.scope.resolve(ident) {
            Ok(val) => val,
            Err(err) => {
                self.error(err);
                self.sentinel()
            }
        }
    }

    pub fn bind(&mut self, ident: Ident, val: IrValue) {
        match self.scope.bind(ident, val) {
            Ok(()) => (),
            Err(err) => {
                self.error(err);
            }
        }
    }

    pub fn call_function<M, F>(&mut self, b: &mut FunctionBuilder, mut block: IrBlock,
                                  m: M, f: F, args: &[IrValue]) -> (IrBlock, IrValue)
    where
        M: IntoValue, F: IntoValue
    {
        block = b.op_capture_function(block, m, f, args.len());
        let fun_val = b.block_args(block)[0];

        let (ok_block, ok_block_val) = b.block_insert_get_val();
        let ok_res = b.block_arg_insert(ok_block);

        let (fail_block, fail_block_val) = b.block_insert_get_val();
        let fail_type = b.block_arg_insert(fail_block);
        let fail_error = b.block_arg_insert(fail_block);
        let fail_trace = b.block_arg_insert(fail_block);

        b.op_call(block, fun_val, args);
        self.exc_stack.make_error_jump(b, fail_block, fail_type, fail_error, fail_trace);

        (ok_block, ok_res)
    }

}

pub fn lower_module(module: &Module) -> IrModule {

    // TODO sort functions for more deterministic compilation

    let mut ir_module = IrModule::new(module.name);

    let mut ctx = LowerCtx {
        module,

        scope: scope::ScopeTracker::new(),
        exc_stack: ExceptionHandlerStack::new(),

        sentinel_value: None,

        errors: Vec::new(),
        failed: false,
    };

    for (ident, function) in module.functions.iter() {
        assert!(ctx.scope.height() == 0);
        println!("Fun Name: {:?}", ident);

        let mut fun = ir_module.add_function(ident.function, function.arity);
        let mut builder = FunctionBuilder::new(&mut fun);

        // We do not want the sentinel value to be a constant,
        // since that would interfere with potential constant
        // comparisons while lowering. Insert an orphaned block
        // with an argument that we use.
        // This has the added benefit of generating actually
        // invalid IR when used.
        let sentinel_block = builder.block_insert();
        let sentinel_value = builder.block_arg_insert(sentinel_block);
        ctx.sentinel_value = Some(sentinel_value);

        lower_top_function(&mut ctx, &mut builder, function);
    }

    ir_module
}

fn lower_function(
    ctx: &mut LowerCtx, b: &mut FunctionBuilder,
    fun: &Function,
) -> IrBlock {
    let entry = b.block_insert();

    match fun {
        Function::Named(named) => {
            unimplemented!()
        }
        Function::Unnamed(lambda) => {
            lower_function_base(ctx, b, entry, lambda.span, lambda.arity, &lambda.clauses);
        }
    }

    entry
}

fn lower_function_base(
    ctx: &mut LowerCtx, b: &mut FunctionBuilder,
    // The block the function should be lowered into
    entry: IrBlock,
    span: ByteSpan,
    arity: usize,
    clauses: &[FunctionClause],
) {
    let mut block = entry;

    // Ok and Fail continuations
    let ok_cont = b.block_arg_insert(entry);
    let err_cont = b.block_arg_insert(entry);

    ctx.exc_stack.push_handler(err_cont);

    // Add arguments and pack them into a value list
    let mut args_val_builder = b.op_pack_value_list_build();
    for _ in 0..arity {
        let arg = b.block_arg_insert(entry);
        args_val_builder.push_value(arg, b);
    }
    args_val_builder.block = Some(block);
    block = args_val_builder.finish(b);
    let args_list = b.block_args(block)[0];

    // Join block after case
    let join_block = b.block_insert();
    let join_arg = b.block_arg_insert(join_block);
    b.op_call(join_block, ok_cont, &[join_arg]);

    // Match fail block
    let match_fail_block = b.block_insert();
    b.op_call(match_fail_block, err_cont, &[]); // TODO

    let entry_exc_height = ctx.exc_stack.len();

    // Top level function case expression
    {
        let mut func_case = b.op_case_build();

        func_case.match_on = Some(args_list);
        func_case.no_match = Some(b.value(match_fail_block));

        for clause in clauses.iter() {
            match lower_clause(ctx, b, &mut block, clause.params.iter(), clause.guard.as_ref()) {
                Some(lowered) => {

                    // Add to case
                    let body_val = b.value(lowered.body);
                    func_case.push_clause(lowered.clause, lowered.guard, body_val, b);

                    let (body_ret_block, body_ret) = lower_block(ctx, b, lowered.body, &clause.body);

                    // Call to join block
                    b.op_call(body_ret_block, join_block, &[body_ret]);

                    // Pop scope pushed in lower_clause
                    ctx.scope.pop(lowered.scope_token);
                },
                // When the pattern of the clause is unmatchable, we don't add it to
                // the case.
                None => continue,
            }
            assert!(ctx.exc_stack.len() == entry_exc_height)
        }

        func_case.finish(block, b);
    }

    ctx.exc_stack.pop_handler();

}

fn lower_top_function(ctx: &mut LowerCtx, b: &mut FunctionBuilder, function: &NamedFunction) {
    let entry = b.block_insert();
    b.block_set_entry(entry);

    lower_function_base(ctx, b, entry, function.span, function.arity, &function.clauses);
}

