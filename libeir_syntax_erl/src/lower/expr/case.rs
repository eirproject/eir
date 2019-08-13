use libeir_ir::{
    FunctionBuilder,
    Value as IrValue,
    Block as IrBlock,
};


use libeir_intern::{ Symbol };

use crate::parser::ast::{ Case, If };

use crate::lower::LowerCtx;
use crate::lower::expr::{ lower_single, lower_block, lower_block_same_scope };
use crate::lower::pattern::lower_clause;
use crate::lower::scope::ScopeMerge;

pub(super) fn lower_case_expr(
    ctx: &mut LowerCtx,
    b: &mut FunctionBuilder,
    mut block: IrBlock,
    case: &Case,
) -> (IrBlock, IrValue)
{
    let match_val = map_block!(block, lower_single(ctx, b, block, &case.expr));

    let no_match = b.block_insert();
    {
        let typ_val = b.value(Symbol::intern("error"));
        let case_clause_val = b.value(Symbol::intern("case_clause"));
        let err_val = b.prim_tuple(&[case_clause_val, match_val]);
        ctx.exc_stack.make_error_jump(b, no_match, typ_val, err_val);
    }

    let mut case_b = b.op_case_build();
    case_b.match_on = Some(match_val);
    case_b.no_match = Some(b.value(no_match));

    let entry_exc_height = ctx.exc_stack.len();

    let mut scope_merge = ScopeMerge::new();

    for clause in case.clauses.iter() {
        match lower_clause(ctx, b, &mut block, [&clause.pattern].iter()
                           .map(|i| *i), clause.guard.as_ref())
        {
            Ok(lowered) => {
                let (scope_token, body) = lowered.make_body(ctx, b);

                // Add to case
                let body_val = b.value(body);
                case_b.push_clause(lowered.clause, lowered.guard, body_val, b);
                for value in lowered.values.iter() {
                    case_b.push_value(*value, b);
                }

                let (body_ret_block, body_ret) = lower_block_same_scope(
                    ctx, b, body, &clause.body);

                let binds = ctx.scope.pop_take(scope_token);
                scope_merge.branch(body_ret_block, body_ret, binds);
            }
            Err(lowered) => {
                let (scope_tok, dummy_body) = lowered.make_body(ctx, b);

                let (body_ret_block, body_ret) = lower_block_same_scope(
                    ctx, b, dummy_body, &clause.body);

                let binds = ctx.scope.pop_take(scope_tok);
                scope_merge.branch(body_ret_block, body_ret, binds);
            },
        }
        assert!(ctx.exc_stack.len() == entry_exc_height)
    }
    case_b.finish(block, b);

    scope_merge.finish(ctx, b)
}

pub(super) fn lower_if_expr(ctx: &mut LowerCtx, b: &mut FunctionBuilder, mut block: IrBlock,
                            if_expr: &If) -> (IrBlock, IrValue)
{
    let match_val = b.prim_value_list(&[]);

    let no_match = b.block_insert();
    {
        let block = no_match;
        let typ_val = b.value(Symbol::intern("error"));
        let badmatch_val = b.value(Symbol::intern("badmatch"));
        let err_val = b.prim_tuple(&[badmatch_val, match_val]);
        ctx.exc_stack.make_error_jump(b, block, typ_val, err_val);
    }

    let mut case_b = b.op_case_build();
    case_b.match_on = Some(match_val);
    case_b.no_match = Some(b.value(no_match));

    let entry_exc_height = ctx.exc_stack.len();

    let mut scope_merge = ScopeMerge::new();

    for clause in if_expr.clauses.iter() {
        match lower_clause(ctx, b, &mut block, [].iter(), Some(&clause.guards)) {
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

                // Pop scope pushed in lower_clause
                let binds = ctx.scope.pop_take(scope_token);
                scope_merge.branch(body_ret_block, body_ret, binds);
            }
            Err(lowered) => {
                let (scope_token, body) = lowered.make_body(ctx, b);

                let (body_ret_block, body_ret) = lower_block(ctx, b, body, &clause.body);

                let binds = ctx.scope.pop_take(scope_token);
                scope_merge.branch(body_ret_block, body_ret, binds);
            },
        }
        assert!(ctx.exc_stack.len() == entry_exc_height)
    }

    case_b.finish(block, b);

    scope_merge.finish(ctx, b)
}
