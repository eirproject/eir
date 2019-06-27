use libeir_ir::{
    FunctionBuilder,
    Value as IrValue,
    Block as IrBlock,
};
use libeir_ir::constant::NilTerm;

use libeir_intern::{ Symbol, Ident };

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
            {
                let mut block = non1_block;

                let typ_val = b.value(Symbol::intern("error"));
                let err_atom = b.value(Symbol::intern("badarg"));
                block = b.op_make_tuple(block, &[err_atom, lhs_val]);
                let err_val = b.block_args(block)[0];
                let trace_val = b.value(NilTerm); // TODO: Trace

                ctx.exc_stack.make_error_jump(b, block, typ_val, err_val, trace_val);
            }

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
            {
                let mut block = non1_block;

                let typ_val = b.value(Symbol::intern("error"));
                let err_atom = b.value(Symbol::intern("badarg"));
                block = b.op_make_tuple(block, &[err_atom, lhs_val]);
                let err_val = b.block_args(block)[0];
                let trace_val = b.value(NilTerm); // TODO: Trace

                ctx.exc_stack.make_error_jump(b, block, typ_val, err_val, trace_val);
            }

            (ret_block, ret_val)
        }
        _ => {
            let lhs_val = map_block!(block, lower_single(ctx, b, block, lhs));
            let rhs_val = map_block!(block, lower_single(ctx, b, block, rhs));

            let (m, f) = match op {
                BinaryOp::Lt => (Ident::from_str("erlang"), Ident::from_str("<")),
                BinaryOp::Lte => (Ident::from_str("erlang"), Ident::from_str("=<")),
                BinaryOp::Gt => (Ident::from_str("erlang"), Ident::from_str(">")),
                BinaryOp::Gte => (Ident::from_str("erlang"), Ident::from_str(">=")),
                BinaryOp::Sub => (Ident::from_str("erlang"), Ident::from_str("-")),
                BinaryOp::Add => (Ident::from_str("erlang"), Ident::from_str("+")),
                BinaryOp::Append => (Ident::from_str("erlang"), Ident::from_str("++")),
                BinaryOp::Multiply => (Ident::from_str("erlang"), Ident::from_str("*")),
                BinaryOp::Divide => (Ident::from_str("erlang"), Ident::from_str("/")),
                BinaryOp::Rem => (Ident::from_str("erlang"), Ident::from_str("rem")),
                BinaryOp::StrictNotEqual => (Ident::from_str("erlang"), Ident::from_str("=/=")),
                BinaryOp::StrictEqual => (Ident::from_str("erlang"), Ident::from_str("=:=")),
                _ => unimplemented!("{:?}", op),
            };

            ctx.call_function(b, block, *span, m, f, &[lhs_val, rhs_val])
        }
    }

}
