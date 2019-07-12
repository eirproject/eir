use libeir_ir::{
    FunctionBuilder,
    Value as IrValue,
    Block as IrBlock,
};
use libeir_ir::constant::NilTerm;
use libeir_ir::op::BinOp;

use libeir_intern::{ Symbol, Ident };
use libeir_diagnostics::DUMMY_SPAN;

use crate::parser::ast::{ Case, If };

use crate::lower::LowerCtx;
use crate::lower::expr::{ lower_single, lower_block };
use crate::lower::pattern::lower_clause;

pub(super) fn lower_case_expr(ctx: &mut LowerCtx, b: &mut FunctionBuilder, mut block: IrBlock,
                              case: &Case) -> (IrBlock, IrValue)
{
    let match_val = map_block!(block, lower_single(ctx, b, block, &case.expr));

    let join_block = b.block_insert();
    let join_arg = b.block_arg_insert(join_block);

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

    let mut case_b = b.op_case_build();
    case_b.match_on = Some(match_val);
    case_b.no_match = Some(b.value(no_match));

    let entry_exc_height = ctx.exc_stack.len();

    for clause in case.clauses.iter() {
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

    (join_block, join_arg)
}

pub(super) fn lower_if_expr(ctx: &mut LowerCtx, b: &mut FunctionBuilder, mut block: IrBlock,
                            if_expr: &If) -> (IrBlock, IrValue)
{
    block = b.op_pack_value_list(block, &[]);
    let match_val = b.block_args(block)[0];

    let join_block = b.block_insert();
    let join_arg = b.block_arg_insert(join_block);

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

    let mut case_b = b.op_case_build();
    case_b.match_on = Some(match_val);
    case_b.no_match = Some(b.value(no_match));

    let entry_exc_height = ctx.exc_stack.len();

    for clause in if_expr.clauses.iter() {
        match lower_clause(ctx, b, &mut block, [].iter(), Some(&clause.guards)) {
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

    (join_block, join_arg)

}
