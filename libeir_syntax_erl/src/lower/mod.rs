use std::sync::Arc;

use libeir_ir::operation::case::Case;
use libeir_ir::{
    Block as IrBlock, FunctionBuilder, IntoValue, Location, Module as IrModule, Value as IrValue,
};

use libeir_diagnostics::{CodeMap, SourceSpan};
use libeir_intern::{Ident, Symbol};
use libeir_util_parse::ErrorReceiver;

use crate::parser::ast::{Function, FunctionClause, Module, NamedFunction};
use crate::evaluator::ResolveRecordIndexError;

macro_rules! map_block {
    ($block:expr, $call:expr) => {{
        let (block, val) = $call;
        $block = block;
        val
    }};
}

mod pattern;
use pattern::lower_clause;

mod expr;
use expr::{lower_block, lower_single};

mod errors;
pub use errors::LowerError;

mod exception_handler_stack;
use exception_handler_stack::ExceptionHandlerStack;

mod scope;
use scope::ScopeToken;

#[cfg(test)]
mod tests;

pub(crate) struct LowerCtx<'a> {
    codemap: Arc<CodeMap>,
    module: &'a Module,

    scope: scope::ScopeTracker,
    exc_stack: ExceptionHandlerStack,

    sentinel_value: Option<IrValue>,

    errors: &'a mut (dyn ErrorReceiver<E = LowerError, W = LowerError> + 'a),

    val_buf: Vec<IrValue>,

    fun_num: usize,
    /// Top is current function name.
    /// Used to generate debug info.
    functions: Vec<String>,
}

impl<'a> LowerCtx<'a> {
    /// Since we want to catch as many errors as possible in a single
    /// compiler invocation, we frequently purposefully generate invalid
    /// IR so that the lowering process can continue.
    /// In a case where, say, we have an unresolved variable, we need
    /// a dummy value that we can use.
    /// If this value is used in the resulting IR, it is REQUIRED that
    /// `error` be called at least once, which sets the error receiver
    /// to the failed state.
    pub fn sentinel(&self) -> IrValue {
        self.sentinel_value.unwrap()
    }

    pub fn error(&mut self, err: LowerError) {
        self.errors.error(err);
    }

    pub fn warn(&mut self, err: LowerError) {
        self.errors.warning(err);
    }

    pub fn failed(&self) -> bool {
        self.errors.is_failed()
    }

    /// Resolves the index of a field in a record.
    pub fn resolve_rec_idx(&self, name: Ident, field: Ident) -> Result<usize, crate::evaluator::ResolveRecordIndexError> {
        let rec = self
            .module
            .records
            .get(&name.name)
            .ok_or(ResolveRecordIndexError::NoRecord)?;
        let idx = rec
            .field_idx_map
            .get(&field)
            .ok_or(ResolveRecordIndexError::NoField)?;
        Ok(*idx)
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

    pub fn bind_shadow(&mut self, ident: Ident, val: IrValue) {
        match self.scope.bind_shadow(ident, val) {
            Ok(()) => (),
            Err(err) => {
                self.warn(err);
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

    pub fn function_name(&self) -> String {
        self.functions[self.functions.len() - 1].clone()
    }
    pub fn current_location(&self, b: &mut FunctionBuilder, span: SourceSpan) -> Location {
        b.fun_mut().locations.from_bytespan(
            &self.codemap,
            span,
            Some(self.module.name.as_str().to_string()),
            Some(self.function_name()),
        )
    }

    pub fn call_function<M, F>(
        &mut self,
        b: &mut FunctionBuilder,
        block: IrBlock,
        span: SourceSpan,
        m: M,
        f: F,
        args: &[IrValue],
    ) -> (IrBlock, IrValue)
    where
        M: IntoValue,
        F: IntoValue,
    {
        let fun_val = b.prim_capture_function(span, m, f, args.len());

        self.val_buf.clear();
        for arg in args.iter() {
            self.val_buf.push(*arg);
        }

        let loc = self.current_location(b, span);
        b.block_set_location(block, loc);

        let (ok_block, fail_block) = b.op_call_function(span, block, fun_val, args);
        b.block_set_location(fail_block, loc);

        let fail_type = b.block_args(fail_block)[0];
        let fail_error = b.block_args(fail_block)[1];
        let fail_trace = b.block_args(fail_block)[2];
        self.exc_stack
            .make_error_jump_trace(b, fail_block, fail_type, fail_error, fail_trace);

        let ok_res = b.block_args(ok_block)[0];

        (ok_block, ok_res)
    }
}

pub fn lower_module<'a>(
    errors: &'a mut (dyn ErrorReceiver<E = LowerError, W = LowerError> + 'a),
    codemap: Arc<CodeMap>,
    module: &Module,
) -> Result<IrModule, ()> {
    // TODO sort functions for more deterministic compilation

    let mut ir_module = IrModule::new_with_span(module.name, module.span);

    let mut ctx = LowerCtx {
        codemap,
        module,

        scope: scope::ScopeTracker::new(),
        exc_stack: ExceptionHandlerStack::new(),

        sentinel_value: None,

        errors,

        val_buf: Vec::new(),

        fun_num: 0,
        functions: Vec::new(),
    };

    for (ident, function) in module.functions.iter() {
        assert!(ctx.scope.height() == 0);
        ctx.fun_num = 0;

        let fun_def = ir_module.add_function(function.span, ident.function, function.arity);
        let mut fun = fun_def.function_mut();
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

    ctx.exc_stack.finish();

    if ctx.failed() {
        Err(())
    } else {
        Ok(ir_module)
    }
}

fn lower_function(ctx: &mut LowerCtx, b: &mut FunctionBuilder, fun: &Function) -> IrBlock {
    let entry = b.block_insert_with_span(Some(fun.span()));

    ctx.fun_num += 1;
    let base_fun = &ctx.functions[0];
    let new_fun = format!("{}-fun-{}", base_fun, ctx.fun_num);
    ctx.functions.push(new_fun);

    match fun {
        Function::Named(named) => {
            let entry_val = b.value(entry);
            ctx.bind(named.name.var(), entry_val);

            lower_function_base(ctx, b, entry, named.span, named.arity, &named.clauses);
        }
        Function::Unnamed(lambda) => {
            lower_function_base(ctx, b, entry, lambda.span, lambda.arity, &lambda.clauses);
        }
    }

    ctx.functions.pop().unwrap();

    entry
}

fn lower_function_base(
    ctx: &mut LowerCtx,
    b: &mut FunctionBuilder,
    // The block the function should be lowered into
    entry: IrBlock,
    span: SourceSpan,
    arity: usize,
    clauses: &[FunctionClause],
) {
    let match_loc = ctx.current_location(b, span);

    let mut block = entry;

    // Ok and Fail continuations
    let ok_cont = b.block_arg_insert(entry);
    let err_cont = b.block_arg_insert(entry);

    ctx.exc_stack.push_handler(err_cont);

    // Add arguments and pack them into a value list
    let mut args_val = Vec::new();
    for _ in 0..arity {
        let arg = b.block_arg_insert(entry);
        args_val.push(arg);
    }
    let args_list = b.prim_value_list(&args_val);

    // Join block after case
    let join_block = b.block_insert();
    b.block_set_location(join_block, match_loc);
    let join_arg = b.block_arg_insert(join_block);
    b.op_call_flow(join_block, ok_cont, &[join_arg]);

    // Match fail block
    let match_fail_block = b.block_insert();
    b.block_set_location(match_fail_block, match_loc);
    {
        let typ_val = b.value(Symbol::intern("error"));
        let err_val = b.value(Symbol::intern("function_clause"));
        ctx.exc_stack
            .make_error_jump(b, span, match_fail_block, typ_val, err_val);
    }

    let entry_exc_height = ctx.exc_stack.len();

    // Top level function case expression
    {
        // TODO: Fuse locations of function heads
        //let mut func_case = b.op_case_build(span);
        let mut func_case = Case::builder();
        func_case.set_span(span);

        func_case.match_on = Some(args_list);
        func_case.no_match = Some(b.value(match_fail_block));

        for clause in clauses.iter() {
            match lower_clause(
                ctx,
                &mut func_case.container,
                b,
                &mut block,
                true,
                clause.span,
                clause.params.iter(),
                clause.guard.as_ref(),
            ) {
                Ok(lowered) => {
                    let (scope_token, body) = lowered.make_body(ctx, b);

                    // Add to case
                    let body_val = b.value(body);
                    func_case.push_clause(lowered.clause, lowered.guard, body_val, b);
                    for value in lowered.values.iter() {
                        func_case.push_value(*value, b);
                    }

                    let (body_ret_block, body_ret) = lower_block(ctx, b, body, &clause.body);

                    // Call to join block
                    b.op_call_flow(body_ret_block, join_block, &[body_ret]);

                    // Pop scope pushed in lower_clause
                    ctx.scope.pop(scope_token);
                }
                // When the pattern of the clause is unmatchable, we don't add it to
                // the case.
                Err(_lowered) => {}
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

    let fun_name = format!("{}/{}", function.name.atom(), function.arity);
    assert!(ctx.functions.len() == 0);
    ctx.functions.push(fun_name);

    let loc = ctx.current_location(b, function.span);
    b.block_set_location(entry, loc);

    lower_function_base(
        ctx,
        b,
        entry,
        function.span,
        function.arity,
        &function.clauses,
    );

    ctx.functions.pop().unwrap();
    assert!(ctx.functions.len() == 0);
}
