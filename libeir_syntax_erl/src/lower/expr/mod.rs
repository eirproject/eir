use libeir_ir::constant::NilTerm;
use libeir_ir::operation::case::Case;
use libeir_ir::{Block as IrBlock, FunctionBuilder, Value as IrValue};

use libeir_intern::{Ident, Symbol};

use super::lower_function;

use super::pattern::lower_clause;
use super::LowerCtx;

use crate::parser::ast::UnaryOp;
use crate::parser::ast::{Apply, Remote, UnaryExpr};
use crate::parser::ast::{Arity, Name};
use crate::parser::ast::{Expr, Literal, Var};
use crate::parser::ast::{FunctionName, LocalFunctionName};

pub mod literal;
use literal::lower_literal;

pub mod binary;
mod binary_expr;
mod case;
mod catch;
mod comprehension;
mod record;
pub use binary::TypeName as BinaryTypeName;
mod map;
mod receive;

pub(super) fn lower_block<'a, T>(
    ctx: &mut LowerCtx,
    b: &mut FunctionBuilder,
    block: IrBlock,
    exprs: T,
) -> (IrBlock, IrValue)
where
    T: IntoIterator<Item = &'a Expr>,
{
    let scope_tok = ctx.scope.push();
    let mut block = block;
    let mut value = None;

    for expr in exprs {
        assert!(b.fun().block_kind(block).is_none());
        let (new_block, val) = lower_expr(ctx, b, block, expr);
        assert!(b.fun().block_kind(new_block).is_none());
        block = new_block;
        value = Some(val);
    }

    // This will pop all the scopes that has been pushed by
    // the expressions in the block.
    ctx.scope.pop(scope_tok);

    (block, value.unwrap())
}

pub(super) fn lower_block_same_scope<'a, T>(
    ctx: &mut LowerCtx,
    b: &mut FunctionBuilder,
    block: IrBlock,
    exprs: T,
) -> (IrBlock, IrValue)
where
    T: IntoIterator<Item = &'a Expr>,
{
    let mut block = block;
    let mut value = None;

    for expr in exprs {
        assert!(b.fun().block_kind(block).is_none());
        let (new_block, val) = lower_expr(ctx, b, block, expr);
        assert!(b.fun().block_kind(new_block).is_none());
        block = new_block;
        value = Some(val);
    }

    (block, value.unwrap())
}

pub(super) fn lower_single(
    ctx: &mut LowerCtx,
    b: &mut FunctionBuilder,
    block: IrBlock,
    expr: &Expr,
) -> (IrBlock, IrValue) {
    let scope_tok = ctx.scope.push();
    assert!(b.fun().block_kind(block).is_none());
    let (new_block, val) = lower_expr(ctx, b, block, expr);
    assert!(b.fun().block_kind(new_block).is_none());
    ctx.scope.pop(scope_tok);
    (new_block, val)
}

pub(super) fn lower_single_same_scope(
    ctx: &mut LowerCtx,
    b: &mut FunctionBuilder,
    block: IrBlock,
    expr: &Expr,
) -> (IrBlock, IrValue) {
    assert!(b.fun().block_kind(block).is_none());
    let (new_block, val) = lower_expr(ctx, b, block, expr);
    assert!(b.fun().block_kind(new_block).is_none());
    (new_block, val)
}

fn lower_expr(
    ctx: &mut LowerCtx,
    b: &mut FunctionBuilder,
    block: IrBlock,
    expr: &Expr,
) -> (IrBlock, IrValue) {
    let mut block = block;
    match expr {
        Expr::Apply(Apply {
            span, callee, args, ..
        }) => {
            let span = *span;
            let mut arg_vals = vec![];

            let arity_val = b.value(args.len());

            let callee_val = match &**callee {
                Expr::Remote(Remote {
                    module, function, ..
                }) => {
                    let mod_val = map_block!(block, lower_single(ctx, b, block, module));
                    let fun_val = map_block!(block, lower_single(ctx, b, block, function));

                    b.prim_capture_function(span, mod_val, fun_val, arity_val)
                }
                Expr::Literal(Literal::Atom(_id, name)) => {
                    let local = LocalFunctionName {
                        span: callee.span(),
                        function: *name,
                        arity: args.len(),
                    };

                    let (module, function) = if ctx.module.functions.contains_key(&local) {
                        (ctx.module.name, *name)
                    } else {
                        if let Some(resolved) = ctx.module.imports.get(&local) {
                            assert!(resolved.arity == args.len());
                            (resolved.module, resolved.function)
                        } else {
                            (ctx.module.name, *name)
                        }
                    };

                    let mod_val = b.value(module);
                    let fun_val = b.value(function);

                    b.prim_capture_function(span, mod_val, fun_val, arity_val)
                }
                expr => map_block!(block, lower_single_same_scope(ctx, b, block, expr)),
            };

            for arg in args {
                let (new_block, arg_val) = lower_single_same_scope(ctx, b, block, arg);
                block = new_block;

                arg_vals.push(arg_val);
            }

            let loc = ctx.current_location(b, span);
            b.block_set_location(block, loc);

            let (ok_block, fail_block) = b.op_call_function(span, block, callee_val, &arg_vals);

            let fail_type = b.block_args(fail_block)[0];
            let fail_error = b.block_args(fail_block)[1];
            let fail_trace = b.block_args(fail_block)[2];
            ctx.exc_stack
                .make_error_jump_trace(b, fail_block, fail_type, fail_error, fail_trace);

            let ok_res = b.block_args(ok_block)[0];
            (ok_block, ok_res)
        }
        Expr::Var(Var(_id, var)) => (block, ctx.resolve(*var)),
        Expr::UnaryExpr(UnaryExpr {
            op, operand, span, ..
        }) => {
            let operand_val = map_block!(block, lower_single(ctx, b, block, operand));

            let (block, val) = match op {
                UnaryOp::Not => ctx.call_function(
                    b,
                    block,
                    *span,
                    Symbol::intern("erlang"),
                    Symbol::intern("not"),
                    &[operand_val],
                ),
                UnaryOp::Minus => ctx.call_function(
                    b,
                    block,
                    *span,
                    Symbol::intern("erlang"),
                    Symbol::intern("-"),
                    &[operand_val],
                ),
                UnaryOp::Plus => ctx.call_function(
                    b,
                    block,
                    *span,
                    Symbol::intern("erlang"),
                    Symbol::intern("+"),
                    &[operand_val],
                ),
                op => unimplemented!("{:?}", op),
            };

            (block, val)
        }
        Expr::Match(mat) => {
            let match_val = map_block!(block, lower_single_same_scope(ctx, b, block, &mat.expr));

            let no_match = b.block_insert();
            {
                let typ_val = b.value(Symbol::intern("error"));
                let badmatch_val = b.value(Symbol::intern("badmatch"));
                let err_val = b.prim_tuple(mat.span, &[badmatch_val, match_val]);
                ctx.exc_stack
                    .make_error_jump(b, mat.span, no_match, typ_val, err_val);
            }

            let mut match_case = Case::builder();
            match_case.set_span(mat.span);

            match lower_clause(
                ctx,
                &mut match_case.container,
                b,
                &mut block,
                false,
                mat.span,
                [&mat.pattern].iter().map(|i| &***i),
                None,
            ) {
                Ok(lowered) => {
                    let (_scope_token, body) = lowered.make_body(ctx, b);

                    match_case.match_on = Some(match_val);
                    match_case.no_match = Some(b.value(no_match));

                    // Add clause to match
                    let body_val = b.value(body);
                    match_case.push_clause(lowered.clause, lowered.guard, body_val, b);
                    for value in lowered.values.iter() {
                        match_case.push_value(*value, b);
                    }

                    match_case.finish(block, b);

                    // The scope pushed in lower_case will be popped by the
                    // calling `lower_block`.

                    (body, match_val)
                }
                Err(lowered) => {
                    b.op_call_flow(block, no_match, &[]);

                    let (_scope_token, body) = lowered.make_body(ctx, b);

                    (body, match_val)
                }
            }
        }
        Expr::Cons(cons) => {
            let head = map_block!(block, lower_single(ctx, b, block, &cons.head));
            let tail = map_block!(block, lower_single(ctx, b, block, &cons.tail));
            let list = b.prim_list_cell(cons.span, head, tail);
            (block, list)
        }
        Expr::Nil(_nil) => {
            let nil_val = b.value(NilTerm);
            (block, nil_val)
        }
        Expr::Tuple(tup) => {
            let mut vals = Vec::new();

            for elem in tup.elements.iter() {
                let val = map_block!(block, lower_single_same_scope(ctx, b, block, elem));
                vals.push(val);
            }

            let tuple = b.prim_tuple(tup.span, &vals);
            (block, tuple)
        }
        Expr::Fun(fun) => {
            let fun = lower_function(ctx, b, fun);
            (block, b.value(fun))
        }
        Expr::FunctionName(name) => match name {
            FunctionName::Resolved(resolved) => {
                let module = b.value(resolved.module);
                let function = b.value(resolved.function);
                let arity = b.value(resolved.arity);

                let fun_val = b.prim_capture_function(resolved.span, module, function, arity);

                (block, fun_val)
            }
            FunctionName::PartiallyResolved(partial) => {
                let local = ctx.module.imports.get(&partial.to_local());
                let resolved = if let Some(fun) = local {
                    fun.clone()
                } else {
                    partial.resolve(ctx.module.name)
                };

                let module = b.value(resolved.module);
                let function = b.value(resolved.function);
                let arity = b.value(resolved.arity);

                let fun_val = b.prim_capture_function(partial.span, module, function, arity);

                (block, fun_val)
            }
            FunctionName::Unresolved(unresolved) => {
                let module = match unresolved.module {
                    None => b.value(ctx.module.name),
                    Some(Name::Atom(atom)) => b.value(atom),
                    Some(Name::Var(var)) => ctx.resolve(var),
                };
                let function = match unresolved.function {
                    Name::Atom(atom) => b.value(atom),
                    Name::Var(var) => ctx.resolve(var),
                };
                let arity = match unresolved.arity {
                    Arity::Int(int) => b.value(int),
                    Arity::Var(var) => b.value(var),
                };

                let fun_val = b.prim_capture_function(unresolved.span, module, function, arity);

                (block, fun_val)
            }
        },
        Expr::Receive(recv) => receive::lower_receive(ctx, b, block, recv),
        Expr::Map(map) => map::lower_map_expr(ctx, b, block, map),
        Expr::MapUpdate(map) => map::lower_map_update_expr(ctx, b, block, map),
        Expr::Begin(begin) => lower_block_same_scope(ctx, b, block, &begin.body),
        Expr::Case(case) => case::lower_case_expr(ctx, b, block, case),
        Expr::If(if_expr) => case::lower_if_expr(ctx, b, block, if_expr),
        Expr::Try(try_expr) => catch::lower_try_expr(ctx, b, block, try_expr),
        Expr::Catch(catch_expr) => catch::lower_catch_expr(ctx, b, block, catch_expr),
        Expr::BinaryExpr(binary_expr) => binary_expr::lower_binary_expr(ctx, b, block, binary_expr),
        Expr::Literal(lit) => lower_literal(ctx, b, block, lit),
        Expr::Record(rec) => record::lower_record_expr(ctx, b, block, rec),
        Expr::RecordAccess(rec) => record::lower_record_access_expr(ctx, b, block, rec),
        Expr::RecordUpdate(rec) => record::lower_record_update_expr(ctx, b, block, rec),
        Expr::RecordIndex(rec) => record::lower_record_index(ctx, b, block, rec),
        Expr::ListComprehension(compr) => {
            comprehension::lower_list_comprehension_expr(ctx, b, block, compr)
        }
        Expr::BinaryComprehension(compr) => {
            comprehension::lower_binary_comprehension_expr(ctx, b, block, compr)
        }
        Expr::Binary(bin) => binary::lower_binary_expr(ctx, b, block, None, bin),
        _ => {
            unimplemented!("{:?}", expr);
        }
    }
}
