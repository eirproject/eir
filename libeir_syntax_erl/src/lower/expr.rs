use libeir_ir::{
    FunctionBuilder,
    Value as IrValue,
    Block as IrBlock,
};
use libeir_ir::constant::NilTerm;

use libeir_intern::{ Ident, Symbol };
use libeir_diagnostics::DUMMY_SPAN;

use super::lower_function;

use super::LowerCtx;
use super::pattern::lower_clause;

use crate::parser::ast::{ Name, Arity };
use crate::parser::ast::{ Expr, Var, Literal };
use crate::parser::ast::{ Apply, Remote, UnaryExpr };
use crate::parser::ast::UnaryOp;
use crate::parser::ast::{ FunctionName, LocalFunctionName };


pub mod literal;
use literal::lower_literal;

mod record;
mod catch;
mod binary_expr;
mod comprehension;
mod case;
pub mod binary;
pub use binary::TypeName as BinaryTypeName;
mod map;

pub(super) fn lower_block<'a, T>(ctx: &mut LowerCtx, b: &mut FunctionBuilder, block: IrBlock,
                                 exprs: T) -> (IrBlock, IrValue) where T: IntoIterator<Item = &'a Expr>
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
    ctx: &mut LowerCtx, b: &mut FunctionBuilder, block: IrBlock,
    exprs: T) -> (IrBlock, IrValue) where T: IntoIterator<Item = &'a Expr>
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

pub(super) fn lower_single(ctx: &mut LowerCtx, b: &mut FunctionBuilder, block: IrBlock,
                expr: &Expr) -> (IrBlock, IrValue) {
    let scope_tok = ctx.scope.push();
    assert!(b.fun().block_kind(block).is_none());
    let (new_block, val) = lower_expr(ctx, b, block, expr);
    assert!(b.fun().block_kind(new_block).is_none());
    ctx.scope.pop(scope_tok);
    (new_block, val)
}

pub(super) fn lower_single_same_scope(ctx: &mut LowerCtx, b: &mut FunctionBuilder, block: IrBlock,
                                      expr: &Expr) -> (IrBlock, IrValue) {
    assert!(b.fun().block_kind(block).is_none());
    let (new_block, val) = lower_expr(ctx, b, block, expr);
    assert!(b.fun().block_kind(new_block).is_none());
    (new_block, val)
}

fn lower_expr(ctx: &mut LowerCtx, b: &mut FunctionBuilder, block: IrBlock,
              expr: &Expr) -> (IrBlock, IrValue)
{

    let mut block = block;
    match expr {
        Expr::Apply(Apply { span, callee, args, .. }) => {
            let (ok_block, ok_block_val) = b.block_insert_get_val();
            let ok_res = b.block_arg_insert(ok_block);

            let (fail_block, fail_block_val) = b.block_insert_get_val();
            let fail_type = b.block_arg_insert(fail_block);
            let fail_error = b.block_arg_insert(fail_block);
            let fail_trace = b.block_arg_insert(fail_block);

            let mut arg_vals = vec![ok_block_val, fail_block_val];

            let arity_val = b.value(args.len());

            let callee_val = match &**callee {
                Expr::Remote(Remote { module, function, .. }) => {
                    let mod_val = map_block!(block, lower_single(ctx, b, block, module));
                    let fun_val = map_block!(block, lower_single(ctx, b, block, function));

                    b.prim_capture_function(mod_val, fun_val, arity_val)

                    //b.block_set_span(block, *span);
                    //block = b.op_capture_function(block, mod_val, fun_val, arity_val);
                    //b.block_args(block)[0]
                }
                Expr::Literal(Literal::Atom(_id, name)) => {
                    let local = LocalFunctionName {
                        span: DUMMY_SPAN,
                        function: *name,
                        arity: args.len(),
                    };

                    let (module, function) = if ctx.module.functions.contains_key(&local) {
                        (ctx.module.name, *name)
                    } else {
                        if let Some(resolved) =
                            ctx.module.imports.get(&local)
                        {
                            assert!(resolved.arity == args.len());
                            (resolved.module, resolved.function)
                        } else {
                            (ctx.module.name, *name)
                        }
                    };

                    //let (module, function) = if let Some(resolved) =
                    //    ctx.module.imports.get(&local)
                    //{
                    //    assert!(resolved.arity == args.len());
                    //    (resolved.module, resolved.function)
                    //} else {
                    //    (ctx.module.name, *name)
                    //};

                    //if m_module != module {
                    //    println!("=_____=!!!!!!===== {:?} {:?} {:?}", m_module, module, function);
                    //}


                    let mod_val = b.value(module);
                    let fun_val = b.value(function);

                    b.prim_capture_function(mod_val, fun_val, arity_val)

                    //b.block_set_span(block, *span);
                    //block = b.op_capture_function(block, mod_val, fun_val, arity_val);
                    //b.block_args(block)[0]
                }
                expr => {
                    map_block!(block, lower_single_same_scope(ctx, b, block, expr))
                }
            };

            for arg in args {
                let (new_block, arg_val) = lower_single_same_scope(ctx, b, block, arg);
                block = new_block;

                arg_vals.push(arg_val);
            }

            b.block_set_span(block, *span);
            b.op_call(block, callee_val, &arg_vals);
            ctx.exc_stack.make_error_jump_trace(
                b, fail_block, fail_type, fail_error, fail_trace);

            (ok_block, ok_res)
        }
        Expr::Var(Var(_id, var)) => {
            (block, ctx.resolve(*var))
        }
        Expr::UnaryExpr(UnaryExpr { op, operand, span, .. }) => {
            let operand_val = map_block!(block, lower_single(ctx, b, block, operand));

            let (block, val) = match op {
                UnaryOp::Not =>
                    ctx.call_function(b, block, *span, Symbol::intern("erlang"), Symbol::intern("not"), &[operand_val]),
                UnaryOp::Minus =>
                    ctx.call_function(b, block, *span, Symbol::intern("erlang"), Symbol::intern("-"), &[operand_val]),
                UnaryOp::Plus =>
                    ctx.call_function(b, block, *span, Symbol::intern("erlang"), Symbol::intern("+"), &[operand_val]),
                op => unimplemented!("{:?}", op),
            };

            (block, val)
        }
        Expr::Match(mat) => {
            let match_val = map_block!(block, lower_single_same_scope(
                ctx, b, block, &mat.expr));

            let no_match = b.block_insert();
            {
                let typ_val = b.value(Symbol::intern("error"));
                let badmatch_val = b.value(Symbol::intern("badmatch"));
                let err_val = b.prim_tuple(&[badmatch_val, match_val]);
                ctx.exc_stack.make_error_jump(b, no_match, typ_val, err_val);
            }

            match lower_clause(ctx, b, &mut block, false,
                               [&mat.pattern].iter().map(|i| &***i), None)
            {
                Ok(lowered) => {
                    let (_scope_token, body) = lowered.make_body(ctx, b);

                    let mut match_case = b.op_case_build();
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
                    b.op_call(block, no_match, &[]);

                    let (_scope_token, body) = lowered.make_body(ctx, b);

                    (body, match_val)
                }
            }
        }
        Expr::Cons(cons) => {
            let head = map_block!(block, lower_single(ctx, b, block, &cons.head));
            let tail = map_block!(block, lower_single(ctx, b, block, &cons.tail));
            let list = b.prim_list_cell(head, tail);
            (block, list)
        }
        Expr::Nil(_nil) => {
            let nil_val = b.value(NilTerm);
            (block, nil_val)
        }
        Expr::Tuple(tup) => {
            let mut vals = Vec::new();

            for elem in tup.elements.iter() {
                let val = map_block!(block, lower_single_same_scope(
                    ctx, b, block, elem));
                vals.push(val);
            }

            let tuple = b.prim_tuple(&vals);
            (block, tuple)
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

            let mut body_block = b.block_insert();
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
                    match lower_clause(ctx, b, &mut body_block, false,
                                       [&clause.pattern].iter().map(|i| *i),
                                       clause.guard.as_ref())
                    {
                        Ok(lowered) => {
                            let scope_token = ctx.scope.push();
                            let body = b.block_insert();

                            let body_mapped = b.block_insert();

                            // Map all matched values through receive_done.
                            // This enables us to do things like copy from
                            // a heap fragment to the main process heap.
                            let mut recv_done_b = b.op_intrinsic_build(
                                Symbol::intern("receive_done"));
                            recv_done_b.push_value(body_mapped, b);
                            for bind in lowered.binds.iter() {
                                let val = b.block_arg_insert(body);
                                recv_done_b.push_value(val, b);

                                let mapped_val = b.block_arg_insert(body_mapped);
                                if let Some(name) = bind {
                                    ctx.bind(*name, mapped_val);
                                }
                            }
                            recv_done_b.block = Some(body);
                            recv_done_b.finish(b);

                            // Add to case
                            let body_val = b.value(body);
                            case_b.push_clause(lowered.clause, lowered.guard, body_val, b);
                            for value in lowered.values.iter() {
                                case_b.push_value(*value, b);
                            }

                            let (body_ret_block, body_ret) = lower_block(
                                ctx, b, body_mapped, &clause.body);

                            // Call to join block
                            b.op_call(body_ret_block, join_block, &[body_ret]);

                            // Pop scope pushed in lower_clause
                            ctx.scope.pop(scope_token);
                        }
                        Err(_lowered) => {}
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
                FunctionName::Resolved(resolved) => {
                    let module = b.value(resolved.module);
                    let function = b.value(resolved.function);
                    let arity = b.value(resolved.arity);

                    block = b.op_capture_function(block, module, function, arity);
                    let fun_val = b.block_args(block)[0];

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

                    block = b.op_capture_function(block, module, function, arity);
                    let fun_val = b.block_args(block)[0];

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

                    b.block_set_span(block, unresolved.span);
                    block = b.op_capture_function(block, module, function, arity);
                    let fun_val = b.block_args(block)[0];

                    (block, fun_val)
                }
            }
        }
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
        Expr::ListComprehension(compr) => comprehension::lower_list_comprehension_expr(ctx, b, block, compr),
        Expr::BinaryComprehension(compr) => comprehension::lower_binary_comprehension_expr(ctx, b, block, compr),
        Expr::Binary(bin) => binary::lower_binary_expr(ctx, b, block, None, bin),
        _ => {
            unimplemented!("{:?}", expr);
        }
    }
}
