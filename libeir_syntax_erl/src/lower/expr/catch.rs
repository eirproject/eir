use libeir_ir::{
    FunctionBuilder,
    CaseBuilder,
    Value as IrValue,
    Block as IrBlock,
};

use libeir_intern::Symbol;

use crate::parser::ast::{ NodeId };
use crate::parser::ast::{ Try, Catch };
use crate::parser::ast::{ Expr, Var, Literal };
use crate::parser::ast::{ Name };

use crate::lower::LowerCtx;
use crate::lower::expr::{ lower_block, lower_single };
use crate::lower::pattern::lower_clause;

pub(super) fn lower_try_expr(ctx: &mut LowerCtx, b: &mut FunctionBuilder, mut block: IrBlock,
                             try_expr: &Try) -> (IrBlock, IrValue) {

    let exc_block = b.block_insert();

    let exc_type = b.block_arg_insert(exc_block);
    let exc_error = b.block_arg_insert(exc_block);
    let exc_trace = b.block_arg_insert(exc_block);

    // Lower exprs while catching exceptions
    ctx.exc_stack.push_handler(b.value(exc_block));
    let body_ret = map_block!(block, lower_block(ctx, b, block, &try_expr.exprs));
    ctx.exc_stack.pop_handler();

    let entry_exc_height = ctx.exc_stack.len();

    let join_block = b.block_insert();
    let join_val = b.block_arg_insert(join_block);

    // Clauses
    if let Some(clauses) = try_expr.clauses.as_ref() {
        let no_match = b.block_insert();
        {
            let block = no_match;
            let typ_val = b.value(Symbol::intern("error"));
            let try_clause_val = b.value(Symbol::intern("try_clause"));
            let err_val = b.prim_tuple(&[try_clause_val, body_ret]);
            ctx.exc_stack.make_error_jump(b, block, typ_val, err_val);
        }

        let mut case_b = b.op_case_build();
        case_b.match_on = Some(body_ret);
        case_b.no_match = Some(b.value(no_match));

        for clause in clauses {
            match lower_clause(ctx, b, &mut block, false,
                               [&clause.pattern].iter().map(|i| *i),
                               clause.guard.as_ref())
            {
                Ok(lowered) => {
                    let (scope_token, body) = lowered.make_body(ctx, b);

                    // Add to case
                    let body_val = b.value(body);
                    case_b.push_clause(lowered.clause, lowered.guard, body_val, b);
                    for value in lowered.values.iter() {
                        case_b.push_value(*value, b);
                    }

                    let (body_ret_block, body_ret) = lower_block(
                        ctx, b, body, &clause.body);

                    // Call to join block
                    b.op_call_flow(body_ret_block, join_block, &[body_ret]);

                    // Pop scope pushed in lower_clause
                    ctx.scope.pop(scope_token);
                }
                Err(_lowered) => {},
            }
            assert!(ctx.exc_stack.len() == entry_exc_height)
        }

        case_b.finish(block, b);
    } else {
        b.op_call_flow(block, join_block, &[body_ret]);
    }

    let catch_no_match_block = b.block_insert();

    // Catch
    if let Some(catch_clauses) = try_expr.catch_clauses.as_ref() {
        let mut block = exc_block;

        let match_val = b.prim_value_list(&[exc_type, exc_error]);

        let mut case_b = b.op_case_build();
        case_b.match_on = Some(match_val);
        case_b.no_match = Some(b.value(catch_no_match_block));

        let entry_exc_height = ctx.exc_stack.len();

        for clause in catch_clauses {

            // Convert the kind expression into a pattern expression so
            // that we can use existing pattern matching infrastructure.
            let kind_expr = match clause.kind {
                Name::Atom(atom) => Expr::Literal(Literal::Atom(NodeId(0), atom)),
                Name::Var(var) => Expr::Var(Var(NodeId(0), var)),
            };

            match lower_clause(
                ctx, b, &mut block, false,
                [
                    &kind_expr,
                    &clause.error,
                ].iter().map(|i| *i),
                clause.guard.as_ref(),
            ) {
                Ok(lowered) => {
                    let (scope_token, body) = lowered.make_body(ctx, b);

                    // Add to case
                    let body_val = b.value(body);
                    case_b.push_clause(lowered.clause, lowered.guard, body_val, b);
                    for value in lowered.values.iter() {
                        case_b.push_value(*value, b);
                    }

                    // Bind stack trace in scope
                    ctx.bind(clause.trace, exc_trace);

                    let (body_ret_block, body_ret) = lower_block(
                        ctx, b, body, &clause.body);

                    // Call to join block
                    b.op_call_flow(body_ret_block, join_block, &[body_ret]);

                    // Pop scope pushed in lower_clause
                    ctx.scope.pop(scope_token);
                }
                Err(_lowered) => {},
            }

            assert!(ctx.exc_stack.len() == entry_exc_height)
        }

        case_b.finish(block, b);
    } else {
        b.op_call_flow(exc_block, catch_no_match_block, &[]);
    }

    // After
    if let Some(after) = try_expr.after.as_ref() {

        // Make after lambda
        let after_lambda = b.block_insert();
        let cont = b.block_arg_insert(after_lambda);
        let (after_block_cont, _after_val) = lower_block(ctx, b, after_lambda, &*after);
        b.op_call_flow(after_block_cont, cont, &[]);

        let ret_block = b.block_insert();
        let ret_val = b.block_arg_insert(ret_block);

        // Exception
        let ret_exc_block = b.block_insert();
        let ret_exc_block_val = b.value(ret_exc_block);
        b.op_call_flow(catch_no_match_block, after_lambda, &[ret_exc_block_val]);
        ctx.exc_stack.make_error_jump_trace(
            b, ret_exc_block, exc_type, exc_error, exc_trace);

        // Return regular
        let ret_regular_block = b.block_insert();
        let ret_regular_block_val = b.value(ret_regular_block);
        b.op_call_flow(join_block, after_lambda, &[ret_regular_block_val]);
        b.op_call_flow(ret_regular_block, ret_block, &[join_val]);

        (ret_block, ret_val)
    } else {
        ctx.exc_stack.make_error_jump_trace(
            b, catch_no_match_block, exc_type, exc_error, exc_trace);
        (join_block, join_val)
    }
}

pub(super) fn lower_catch_expr(ctx: &mut LowerCtx, b: &mut FunctionBuilder, mut block: IrBlock,
                               catch_expr: &Catch) -> (IrBlock, IrValue) {
    let exc_block = b.block_insert();

    let exc_type = b.block_arg_insert(exc_block);
    let exc_error = b.block_arg_insert(exc_block);
    let exc_trace = b.block_arg_insert(exc_block);

    let mut case_b = b.op_case_build();

    // Atoms
    let big_exit_atom = b.value(Symbol::intern("EXIT"));

    let make_value_clause = |b: &mut FunctionBuilder, case_b: &mut CaseBuilder, val: IrValue| {
        let clause = b.pat_mut().clause_start();

        // Value
        case_b.push_value(val, b);
        let pat_val = b.pat_mut().clause_value(clause);

        let pat = b.pat_mut().node_empty();
        b.pat_mut().value(pat, pat_val);

        b.pat_mut().clause_node_push(clause, pat);
        b.pat_mut().clause_finish(clause);
        clause
    };

    let error_atom = b.value(Symbol::intern("error"));
    let error_clause = make_value_clause(b, &mut case_b, error_atom);

    let exit_atom = b.value(Symbol::intern("exit"));
    let exit_clause = make_value_clause(b, &mut case_b, exit_atom);

    let throw_atom = b.value(Symbol::intern("throw"));
    let throw_clause = make_value_clause(b, &mut case_b, throw_atom);

    // Join block
    let join_block = b.block_insert();
    let join_arg = b.block_arg_insert(join_block);

    // Lower exprs while catching exceptions
    ctx.exc_stack.push_handler(b.value(exc_block));
    let body_ret = map_block!(block, lower_single(ctx, b, block, &catch_expr.expr));
    ctx.exc_stack.pop_handler();
    b.op_call_flow(block, join_block, &[body_ret]);

    // no_match is unreachable
    let no_match = b.block_insert();
    b.op_unreachable(no_match);

    // Guard lambda returning true
    let guard = b.block_insert();
    let guard_val = b.value(guard);
    let guard_cont = b.block_arg_insert(guard);
    let _guard_throw_cont = b.block_arg_insert(guard);
    let true_val = b.value(true);
    b.op_call_flow(guard, guard_cont, &[true_val]);

    // Actual case
    case_b.match_on = Some(exc_type);
    case_b.no_match = Some(b.value(no_match));

    // Error branch
    {
        let error_block = b.block_insert();
        let error_block_val = b.value(error_block);
        case_b.push_clause(error_clause, guard_val, error_block_val, b);

        let inner_tup = b.prim_tuple(&[exc_error, exc_trace]);
        let ret_tup = b.prim_tuple(&[big_exit_atom, inner_tup]);

        b.op_call_flow(error_block, join_block, &[ret_tup]);
    }

    // Exit branch
    {
        let exit_block = b.block_insert();
        let exit_block_val = b.value(exit_block);
        case_b.push_clause(exit_clause, guard_val, exit_block_val, b);

        let ret_tup = b.prim_tuple(&[big_exit_atom, exc_error]);

        b.op_call_flow(exit_block, join_block, &[ret_tup]);
    }

    // Throw branch
    {
        let throw_block = b.block_insert();
        let throw_block_val = b.value(throw_block);
        case_b.push_clause(throw_clause, guard_val, throw_block_val, b);

        b.op_call_flow(throw_block, join_block, &[exc_error]);
    }

    case_b.finish(exc_block, b);

    (join_block, join_arg)
}
