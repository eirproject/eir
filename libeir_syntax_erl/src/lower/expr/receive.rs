use libeir_intern::Ident;
use libeir_ir::{
    FunctionBuilder,
    Value as IrValue,
    Block as IrBlock,
    operation::receive::{
        ReceiveStart, ReceiveWait, ReceiveDone,
    },
};

use crate::{
    lower::{
        LowerCtx, lower_single, lower_block,
        pattern::lower_clause,
    },
    parser::ast::Receive,
};

pub(super) fn lower_receive(
    ctx: &mut LowerCtx,
    b: &mut FunctionBuilder,
    mut block: IrBlock,
    recv: &Receive,
) -> (IrBlock, IrValue)
{

    let join_block = b.block_insert();
    let join_arg = b.block_arg_insert(join_block);

    // The timeout time
    let after_timeout_val = if let Some(after) = &recv.after {
        map_block!(block, lower_single(ctx, b, block, &after.timeout))
    } else {
        b.value(Ident::from_str("infinity"))
    };

    // Receive start
    let recv_wait_block = ReceiveStart::build(b, block, after_timeout_val);
    let recv_ref_val = b.block_args(recv_wait_block)[0];

    // Receive wait
    let (after_block, mut body_block) = ReceiveWait::build(b, recv_wait_block, recv_ref_val);
    let body_message_arg = b.block_args(body_block)[0];

    // If there is a timeout block, the after code
    if let Some(after) = &recv.after {
        let (after_ret_block, after_ret) = lower_block(ctx, b, after_block, &after.body);
        b.op_call_flow(after_ret_block, join_block, &[after_ret]);
    } else {
        b.op_unreachable(after_block);
    };

    if let Some(clauses) = &recv.clauses {

        let no_match = b.block_insert();
        b.op_call_flow(no_match, recv_wait_block, &[recv_ref_val]);

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

                    // Map all matched values through receive_done.
                    // This enables us to do things like copy from
                    // a heap fragment to the main process heap.
                    let values: Vec<_> = lowered
                        .binds
                        .iter()
                        .map(|_bind| b.block_arg_insert(body))
                        .collect();
                    let body_mapped = ReceiveDone::build(b, body, recv_ref_val, &values);

                    for (idx, bind) in lowered.binds.iter().enumerate() {
                        if let Some(name) = bind {
                            let mapped_val = b.block_args(body_mapped)[idx];
                            ctx.bind(*name, mapped_val);
                        }
                    }

                    // Add to case
                    let body_val = b.value(body);
                    case_b.push_clause(lowered.clause, lowered.guard, body_val, b);
                    for value in lowered.values.iter() {
                        case_b.push_value(*value, b);
                    }

                    let (body_ret_block, body_ret) = lower_block(
                        ctx, b, body_mapped, &clause.body);

                    // Call to join block
                    b.op_call_flow(body_ret_block, join_block, &[body_ret]);

                    // Pop scope pushed in lower_clause
                    ctx.scope.pop(scope_token);
                }
                Err(_lowered) => {}
            }
            assert!(ctx.exc_stack.len() == entry_exc_height)
        }

        case_b.finish(body_block, b);

    } else {
        b.op_call_flow(body_block, join_block, &[body_message_arg]);
    }

    (join_block, join_arg)

}
