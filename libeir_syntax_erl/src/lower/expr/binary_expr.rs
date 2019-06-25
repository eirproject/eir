use libeir_ir::{
    FunctionBuilder,
    Value as IrValue,
    Block as IrBlock,
};

use libeir_intern::{ Ident };
use libeir_diagnostics::DUMMY_SPAN;

use crate::parser::ast::{ BinaryExpr };
use crate::parser::ast::{ BinaryOp };

use crate::lower::LowerCtx;
use crate::lower::expr::{ lower_single };

pub(super) fn lower_binary_expr(ctx: &mut LowerCtx, b: &mut FunctionBuilder, mut block: IrBlock,
                                expr: &BinaryExpr) -> (IrBlock, IrValue)
{
    let BinaryExpr { lhs, rhs, op, span } = expr;

    match op {
        BinaryOp::AndAlso => {
            let (l1_block, lhs_val) = lower_single(ctx, b, block, lhs);

            let ret_block = b.block_insert();
            let ret_val = b.block_arg_insert(ret_block);

            let (true1_block, false1_block, non1_block) = b.op_if_bool(l1_block, lhs_val);

            // True branch
            let (l2_block, rhs_val) = lower_single(ctx, b, true1_block, rhs);
            b.op_call(l2_block, ret_block, &[rhs_val]);

            // False branch
            let false_val = b.value(false);
            b.op_call(false1_block, ret_block, &[false_val]);

            // Nonbool branch
            // TODO throw

            (ret_block, ret_val)
        }
        BinaryOp::OrElse => {
            let (l1_block, lhs_val) = lower_single(ctx, b, block, lhs);

            let ret_block = b.block_insert();
            let ret_val = b.block_arg_insert(ret_block);

            let (true1_block, false1_block, non1_block) = b.op_if_bool(l1_block, lhs_val);

            // True branch
            let true_val = b.value(true);
            b.op_call(true1_block, ret_block, &[true_val]);

            // False branch
            let (l2_block, rhs_val) = lower_single(ctx, b, false1_block, rhs);
            b.op_call(l2_block, ret_block, &[rhs_val]);

            // Nonbool branch
            // TODO throw

            (ret_block, ret_val)
        }
        _ => {
            let lhs_val = map_block!(block, lower_single(ctx, b, block, lhs));
            let rhs_val = map_block!(block, lower_single(ctx, b, block, rhs));

            match op {
                BinaryOp::Lt => {
                    ctx.call_function(b, block, Ident::from_str("erlang"), Ident::from_str("<"), &[lhs_val, rhs_val])
                }
                BinaryOp::Lte => {
                    ctx.call_function(b, block, Ident::from_str("erlang"), Ident::from_str("=<"), &[lhs_val, rhs_val])
                }
                BinaryOp::Sub => {
                    ctx.call_function(b, block, Ident::from_str("erlang"), Ident::from_str("-"), &[lhs_val, rhs_val])
                }
                BinaryOp::Add => {
                    ctx.call_function(b, block, Ident::from_str("erlang"), Ident::from_str("+"), &[lhs_val, rhs_val])
                }
                BinaryOp::Append => {
                    ctx.call_function(b, block, Ident::from_str("erlang"), Ident::from_str("++"), &[lhs_val, rhs_val])
                }
                BinaryOp::Multiply => {
                    ctx.call_function(b, block, Ident::from_str("erlang"), Ident::from_str("*"), &[lhs_val, rhs_val])
                }
                BinaryOp::Divide => {
                    ctx.call_function(b, block, Ident::from_str("erlang"), Ident::from_str("/"), &[lhs_val, rhs_val])
                }
                BinaryOp::StrictNotEqual => {
                    ctx.call_function(b, block, Ident::from_str("erlang"), Ident::from_str("=/="), &[lhs_val, rhs_val])
                }
                BinaryOp::StrictEqual => {
                    ctx.call_function(b, block, Ident::from_str("erlang"), Ident::from_str("=:="), &[lhs_val, rhs_val])
                }
                _ => unimplemented!("{:?}", op),
            }
        }
    }

}
