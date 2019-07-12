use libeir_ir::{
    Module as IrModule,
    FunctionBuilder,
    Value as IrValue,
    Block as IrBlock,
    BinOp as IrBinOp,
};
use libeir_ir::constant::{ IntTerm, AtomTerm, NilTerm, BinaryTerm };

use libeir_intern::{ Ident, Symbol };
use libeir_diagnostics::DUMMY_SPAN;

use super::lower_function;

use super::LowerCtx;
use super::pattern::lower_clause;

use crate::parser::ast::{ Expr, Literal };
use crate::parser::ast::{ Apply, Remote, UnaryExpr };
use crate::parser::ast::UnaryOp;
use crate::parser::ast::{ FunctionName };

pub mod literal;
use literal::lower_literal;

mod record;
mod catch;
mod binary_expr;
mod comprehension;
mod case;
mod binary;
pub use binary::TypeName as BinaryTypeName;

pub(super) fn lower_block<'a, T>(ctx: &mut LowerCtx, b: &mut FunctionBuilder, block: IrBlock,
                                 exprs: T) -> (IrBlock, IrValue) where T: IntoIterator<Item = &'a Expr>
{
    let scope_tok = ctx.scope.push();
    let mut block = block;
    let mut value = None;

    for expr in exprs {
        let (new_block, val) = lower_expr(ctx, b, block, expr);
        block = new_block;
        value = Some(val);
    }

    // This will pop all the scopes that has been pushed by
    // the expressions in the block.
    ctx.scope.pop(scope_tok);

    (block, value.unwrap())
}

fn lower_single(ctx: &mut LowerCtx, b: &mut FunctionBuilder, block: IrBlock,
                expr: &Expr) -> (IrBlock, IrValue) {
    lower_block(ctx, b, block, [expr].iter().map(|v| *v))
}

fn lower_expr(ctx: &mut LowerCtx, b: &mut FunctionBuilder, block: IrBlock,
              expr: &Expr) -> (IrBlock, IrValue)
{
    let mut block = block;
    match expr {
        Expr::Apply(Apply { span, callee, args }) => {
            // TODO reduce allocations

            let (ok_block, ok_block_val) = b.block_insert_get_val();
            let ok_res = b.block_arg_insert(ok_block);

            let (fail_block, fail_block_val) = b.block_insert_get_val();
            let fail_type = b.block_arg_insert(fail_block);
            let fail_error = b.block_arg_insert(fail_block);
            let fail_trace = b.block_arg_insert(fail_block);

            let mut arg_vals = vec![ok_block_val, fail_block_val];

            // Don't really know if it makes sense to put any span on this
            let arity_val = b.value(args.len());

            let callee_val = match &**callee {
                Expr::Remote(Remote { span, module, function }) => {
                    let mod_val = map_block!(block, lower_single(ctx, b, block, module));
                    let fun_val = map_block!(block, lower_single(ctx, b, block, function));

                    b.block_set_span(block, *span);
                    block = b.op_capture_function(block, mod_val, fun_val, arity_val);
                    b.block_args(block)[0]
                }
                Expr::Literal(Literal::Atom(name)) => {
                    let mod_val = b.value(ctx.module.name);
                    let fun_val = b.value(*name);

                    b.block_set_span(block, *span);
                    block = b.op_capture_function(block, mod_val, fun_val, arity_val);
                    b.block_args(block)[0]
                }
                expr => {
                    map_block!(block, lower_single(ctx, b, block, expr))
                }
            };

            for arg in args {
                let (new_block, arg_val) = lower_single(ctx, b, block, arg);
                block = new_block;

                arg_vals.push(arg_val);
            }

            b.block_set_span(block, *span);
            b.op_call(block, callee_val, &arg_vals);
            ctx.exc_stack.make_error_jump(b, fail_block, fail_type, fail_error, fail_trace);

            (ok_block, ok_res)
        }
        Expr::Var(var) => {
            (block, ctx.resolve(*var))
        }
        Expr::UnaryExpr(UnaryExpr { op, operand, span }) => {
            let operand_val = map_block!(block, lower_single(ctx, b, block, operand));

            let (block, val) = match op {
                UnaryOp::Not =>
                    ctx.call_function(b, block, *span, Symbol::intern("erlang"), Symbol::intern("not"), &[operand_val]),
                _ => unimplemented!(),
            };

            (block, val)
        }
        Expr::Match(mat) => {
            let match_val = map_block!(block, lower_single(ctx, b, block, &mat.expr));

            let no_match = b.block_insert();
            {
                let mut block = no_match;
                let typ_val = b.value(Symbol::intern("error"));
                let badmatch_val = b.value(Symbol::intern("badmatch"));
                block = b.op_make_tuple(block, &[badmatch_val, match_val]);
                let err_val = b.block_args(block)[0];
                // TODO trace
                let trace_val = b.value(NilTerm);
                ctx.exc_stack.make_error_jump(b, block, typ_val, err_val, trace_val);
            }

            match lower_clause(ctx, b, &mut block, [&mat.pattern].iter().map(|i| &***i), None) {
                Some(lowered) => {

                    let mut match_case = b.op_case_build();
                    match_case.match_on = Some(match_val);
                    match_case.no_match = Some(b.value(no_match));

                    // Add clause to match
                    let body_val = b.value(lowered.body);
                    match_case.push_clause(lowered.clause, lowered.guard, body_val, b);

                    match_case.finish(block, b);

                    // The scope pushed in lower_case will be popped by the
                    // calling `lower_block`.

                    (lowered.body, match_val)
                }
                None => {
                    b.op_call(block, no_match, &[]);

                    let dummy_cont = b.block_insert();
                    let dummy_val = b.block_arg_insert(dummy_cont);

                    (dummy_cont, dummy_val)
                }
            }
        }
        Expr::Cons(cons) => {
            let head = map_block!(block, lower_single(ctx, b, block, &cons.head));
            let tail = map_block!(block, lower_single(ctx, b, block, &cons.tail));
            b.block_set_span(block, cons.span);
            block = b.op_make_list(block, &[head], tail);
            (block, b.block_args(block)[0])
        }
        Expr::Nil(nil) => {
            let nil_val = b.value((NilTerm, nil.0));
            (block, nil_val)
        }
        Expr::Tuple(tup) => {
            let mut vals = Vec::new();

            for elem in tup.elements.iter() {
                let val = map_block!(block, lower_single(ctx, b, block, elem));
                vals.push(val);
            }

            block = b.op_make_tuple(block, &vals);
            (block, b.block_args(block)[0])
        }
        Expr::Fun(fun) => {
            let fun = lower_function(ctx, b, fun);
            (block, b.value(fun))
        }
        Expr::Receive(recv) => {

            let join_block = b.block_insert();
            let join_arg = b.block_arg_insert(join_block);

            let recv_wait_block = b.block_insert();
            let recv_ref_val = b.block_arg_insert(recv_wait_block);

            let body_block = b.block_insert();
            let body_message_arg = b.block_arg_insert(body_block);

            // The after continuation, called on timeout
            let after_block = b.block_insert();

            // The timeout time
            let after_timeout_val = if let Some(after) = &recv.after {
                let (after_ret_block, after_ret) = lower_block(ctx, b, after_block, &after.body);
                b.op_call(after_ret_block, join_block, &[after_ret]);

                map_block!(block, lower_single(ctx, b, block, &after.timeout))
            } else {
                b.op_unreachable(after_block);
                b.value(Ident::from_str("infinity"))
            };

            // Receive start
            let mut recv_start_b = b.op_intrinsic_build(Symbol::intern("receive_start"));
            recv_start_b.push_value(recv_wait_block, b);
            recv_start_b.push_value(after_timeout_val, b);
            recv_start_b.block = Some(block);
            recv_start_b.finish(b);

            // Receive wait
            let mut recv_start_b = b.op_intrinsic_build(Symbol::intern("receive_wait"));
            recv_start_b.push_value(after_block, b);
            recv_start_b.push_value(body_block, b);
            recv_start_b.block = Some(recv_wait_block);
            recv_start_b.finish(b);

            if let Some(clauses) = &recv.clauses {

                let no_match = b.block_insert();
                b.op_call(no_match, recv_wait_block, &[recv_ref_val]);

                let mut case_b = b.op_case_build();
                case_b.match_on = Some(body_message_arg);
                case_b.no_match = Some(b.value(no_match));

                let entry_exc_height = ctx.exc_stack.len();

                for clause in clauses.iter() {
                    match lower_clause(ctx, b, &mut block, [&clause.pattern].iter().map(|i| *i), clause.guard.as_ref()) {
                        Some(lowered) => {

                            // Add to case
                            let body_val = b.value(lowered.body);
                            case_b.push_clause(lowered.clause, lowered.guard, body_val, b);

                            let (body_ret_block, body_ret) = lower_block(ctx, b, lowered.body, &clause.body);

                            // Call to join block
                            b.op_call(body_ret_block, join_block, &[body_ret]);

                            // Pop scope pushed in lower_clause
                            ctx.scope.pop(lowered.scope_token);
                        }
                        None => continue,
                    }
                    assert!(ctx.exc_stack.len() == entry_exc_height)
                }

                case_b.finish(body_block, b);

            } else {
                b.op_call(body_block, join_block, &[body_message_arg]);
            }

            (join_block, join_arg)
        }
        Expr::FunctionName(name) => {
            match name {
                FunctionName::PartiallyResolved(partial) => {
                    let module = b.value(ctx.module.name);
                    let function = b.value(partial.function);
                    let arity = b.value(partial.arity);

                    block = b.op_capture_function(block, module, function, arity);
                    let fun_val = b.block_args(block)[0];

                    (block, fun_val)
                }
                _ => unimplemented!(),
            }
        }
        Expr::Case(case) => case::lower_case_expr(ctx, b, block, case),
        Expr::If(if_expr) => case::lower_if_expr(ctx, b, block, if_expr),
        Expr::Try(try_expr) => catch::lower_try_expr(ctx, b, block, try_expr),
        Expr::Catch(catch_expr) => catch::lower_catch_expr(ctx, b, block, catch_expr),
        Expr::BinaryExpr(binary_expr) => binary_expr::lower_binary_expr(ctx, b, block, binary_expr),
        Expr::Literal(lit) => lower_literal(ctx, b, block, lit),
        Expr::Record(rec) => record::lower_record_expr(ctx, b, block, rec),
        Expr::RecordAccess(rec) => record::lower_record_access_expr(ctx, b, block, rec),
        Expr::RecordUpdate(rec) => record::lower_record_update_expr(ctx, b, block, rec),
        Expr::ListComprehension(compr) => comprehension::lower_list_comprehension_expr(ctx, b, block, compr),
        Expr::Binary(bin) => binary::lower_binary_expr(ctx, b, block, bin),
        _ => {
            unimplemented!("{:?}", expr);
        }
    }
}
