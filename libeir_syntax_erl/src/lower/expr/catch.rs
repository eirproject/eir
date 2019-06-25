use libeir_ir::{
    Module as IrModule,
    FunctionBuilder,
    Value as IrValue,
    Block as IrBlock,
    BinOp as IrBinOp,
};

use libeir_intern::Symbol;

use crate::parser::ast::{ Try, Catch };
use crate::parser::ast::{ Expr, Literal };
use crate::parser::ast::{ Name };

use crate::lower::LowerCtx;
use crate::lower::expr::{ lower_block, lower_single };
use crate::lower::pattern::lower_clause;

pub(super) fn lower_try_expr(ctx: &mut LowerCtx, b: &mut FunctionBuilder, mut block: IrBlock,
                             try_expr: &Try) -> (IrBlock, IrValue) {
    // TODO: Decide on exception ABI.
    // Right now we are doing 3 arguments, type, error and trace.

    let exc_block = b.block_insert();

    let exc_type = b.block_arg_insert(exc_block);
    let exc_error = b.block_arg_insert(exc_block);
    let exc_trace = b.block_arg_insert(exc_block);

    // Lower exprs while catching exceptions
    ctx.exc_stack.push_handler(b.value(exc_block));
    let body_ret = map_block!(block, lower_block(ctx, b, block, &try_expr.exprs));
    ctx.exc_stack.pop_handler();

    let join_block = b.block_insert();
    let join_val = b.block_arg_insert(join_block);

    let entry_exc_height = ctx.exc_stack.len();

    // Clauses
    if let Some(clauses) = try_expr.clauses.as_ref() {
        // TODO raise try_clause error in no_match
        let no_match = b.block_insert();

        let mut case_b = b.op_case_build();
        case_b.match_on = Some(body_ret);
        case_b.no_match = Some(b.value(no_match));

        for clause in clauses {
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

        case_b.finish(block, b);
    }

    // Catch
    if let Some(catch_clauses) = try_expr.catch_clauses.as_ref() {
        let mut block = b.op_pack_value_list(exc_block, &[exc_type, exc_error]);
        let match_val = b.block_args(block)[0];

        let no_match = b.block_insert();
        ctx.exc_stack.make_error_jump(b, no_match, exc_type, exc_error, exc_trace);

        let mut case_b = b.op_case_build();
        case_b.match_on = Some(match_val);
        case_b.no_match = Some(b.value(no_match));

        let entry_exc_height = ctx.exc_stack.len();

        for clause in catch_clauses {

            // Convert the kind expression into a pattern expression so
            // that we can use existing pattern matching infrastructure.
            let kind_expr = match clause.kind {
                Name::Atom(atom) => Expr::Literal(Literal::Atom(atom)),
                Name::Var(var) => Expr::Var(var),
            };

            match lower_clause(ctx, b, &mut block, [&kind_expr, &clause.error].iter().map(|i| *i), clause.guard.as_ref()) {
                Some(lowered) => {
                    // Add to case
                    let body_val = b.value(lowered.body);
                    case_b.push_clause(lowered.clause, lowered.guard, body_val, b);

                    // Bind stack trace in scope
                    ctx.bind(clause.trace, exc_trace);

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

        case_b.finish(block, b);
    }

    // After
    if let Some(after) = try_expr.after.as_ref() {
        let (after_block, _after_val) = lower_block(ctx, b, join_block, &*after);
        (after_block, join_val)
    } else {
        (join_block, join_val)
    }
}

pub(super) fn lower_catch_expr(ctx: &mut LowerCtx, b: &mut FunctionBuilder, mut block: IrBlock,
                               catch_expr: &Catch) -> (IrBlock, IrValue) {
    let exc_block = b.block_insert();

    let exc_type = b.block_arg_insert(exc_block);
    let exc_error = b.block_arg_insert(exc_block);
    let exc_trace = b.block_arg_insert(exc_block);

    // Atoms
    let big_exit_atom = b.value(Symbol::intern("EXIT"));

    let error_atom = b.cons_mut().from(Symbol::intern("error"));
    let error_pat = b.pat_mut().atomic(error_atom);
    let error_clause = b.pat_mut().clause_start();
    b.pat_mut().clause_node_push(error_clause, error_pat);
    b.pat_mut().clause_finish(error_clause);

    let exit_atom = b.cons_mut().from(Symbol::intern("exit"));
    let exit_pat = b.pat_mut().atomic(exit_atom);
    let exit_clause = b.pat_mut().clause_start();
    b.pat_mut().clause_node_push(exit_clause, exit_pat);
    b.pat_mut().clause_finish(exit_clause);

    let throw_atom = b.cons_mut().from(Symbol::intern("throw"));
    let throw_pat = b.pat_mut().atomic(throw_atom);
    let throw_clause = b.pat_mut().clause_start();
    b.pat_mut().clause_node_push(throw_clause, throw_pat);
    b.pat_mut().clause_finish(throw_clause);

    // Lower exprs while catching exceptions
    ctx.exc_stack.push_handler(b.value(exc_block));
    let body_ret = map_block!(block, lower_single(ctx, b, block, &catch_expr.expr));
    ctx.exc_stack.pop_handler();

    // no_match is unreachable
    let no_match = b.block_insert();
    b.op_unreachable(no_match);

    // Guard lambda returning true
    let guard = b.block_insert();
    let guard_val = b.value(guard);
    let guard_cont = b.block_arg_insert(guard);
    let true_val = b.value(true);
    b.op_call(guard, guard_cont, &[true_val]);

    // Join block
    let join_block = b.block_insert();
    let join_arg = b.block_arg_insert(join_block);

    // Actual case
    let mut case_b = b.op_case_build();
    case_b.match_on = Some(exc_type);
    case_b.no_match = Some(b.value(no_match));

    // Error branch
    {
        let mut error_block = b.block_insert();
        let error_block_val = b.value(error_block);
        case_b.push_clause(error_clause, guard_val, error_block_val, b);

        error_block = b.op_make_tuple(error_block, &[exc_error, exc_trace]);
        let inner_tup = b.block_args(error_block)[0];

        error_block = b.op_make_tuple(error_block, &[big_exit_atom, inner_tup]);
        let ret_tup = b.block_args(error_block)[0];

        b.op_call(error_block, join_block, &[ret_tup]);
    }

    // Exit branch
    {
        let mut exit_block = b.block_insert();
        let exit_block_val = b.value(exit_block);
        case_b.push_clause(exit_clause, guard_val, exit_block_val, b);

        exit_block = b.op_make_tuple(exit_block, &[big_exit_atom, exc_error]);
        let ret_tup = b.block_args(exit_block)[0];

        b.op_call(exit_block, join_block, &[ret_tup]);
    }

    // Throw branch
    {
        let throw_block = b.block_insert();
        let throw_block_val = b.value(throw_block);
        case_b.push_clause(throw_clause, guard_val, throw_block_val, b);

        b.op_call(throw_block, join_block, &[exc_error]);
    }

    case_b.finish(block, b);

    (join_block, join_arg)
}
