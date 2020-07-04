use libeir_ir::{
    FunctionBuilder,
    Value as IrValue,
    Block as IrBlock,
};


use libeir_intern::{ Symbol, Ident };

use crate::parser::ast::{ BinaryExpr };
use crate::parser::ast::{ BinaryOp };

use crate::lower::LowerCtx;
use crate::lower::expr::{ lower_single, lower_single_same_scope };

pub(super) fn lower_binary_expr(ctx: &mut LowerCtx, b: &mut FunctionBuilder, mut block: IrBlock,
                                expr: &BinaryExpr) -> (IrBlock, IrValue)
{
    let BinaryExpr { lhs, rhs, op, id: _, span } = expr;
    let span = *span;

    match op {
        BinaryOp::AndAlso => {
            let (l1_block, lhs_val) = lower_single(ctx, b, block, lhs);

            let ret_block = b.block_insert();
            let ret_val = b.block_arg_insert(ret_block);

            let (true1_block, false1_block, non1_block) = b.op_if_bool(span, l1_block, lhs_val);

            // True branch
            let (l2_block, rhs_val) = lower_single(ctx, b, true1_block, rhs);
            b.op_call_flow(l2_block, ret_block, &[rhs_val]);

            // False branch
            let false_val = b.value(false);
            b.op_call_flow(false1_block, ret_block, &[false_val]);

            // Nonbool branch
            let typ_val = b.value(Symbol::intern("error"));
            let err_atom = b.value(Symbol::intern("badarg"));
            let err_val = b.prim_tuple(span, &[err_atom, lhs_val]);
            ctx.exc_stack.make_error_jump(b, span, non1_block, typ_val, err_val);

            (ret_block, ret_val)
        }
        BinaryOp::OrElse => {
            let (l1_block, lhs_val) = lower_single(ctx, b, block, lhs);

            let ret_block = b.block_insert();
            let ret_val = b.block_arg_insert(ret_block);

            let (true1_block, false1_block, non1_block) = b.op_if_bool(span, l1_block, lhs_val);

            // True branch
            let true_val = b.value(true);
            b.op_call_flow(true1_block, ret_block, &[true_val]);

            // False branch
            let (l2_block, rhs_val) = lower_single(ctx, b, false1_block, rhs);
            b.op_call_flow(l2_block, ret_block, &[rhs_val]);

            // Nonbool branch
            {
                let block = non1_block;

                let typ_val = b.value(Symbol::intern("error"));
                let err_atom = b.value(Symbol::intern("badarg"));
                let err_val = b.prim_tuple(span, &[err_atom, lhs_val]);

                ctx.exc_stack.make_error_jump(b, span, block, typ_val, err_val);
            }

            (ret_block, ret_val)
        }
        _ => {
            let lhs_val = map_block!(block, lower_single_same_scope(
                ctx, b, block, lhs));
            let rhs_val = map_block!(block, lower_single_same_scope(
                ctx, b, block, rhs));

            let (m, f) = match op {
                BinaryOp::Lt => (Ident::from_str("erlang"), Ident::from_str("<")),
                BinaryOp::Lte => (Ident::from_str("erlang"), Ident::from_str("=<")),
                BinaryOp::Gt => (Ident::from_str("erlang"), Ident::from_str(">")),
                BinaryOp::Gte => (Ident::from_str("erlang"), Ident::from_str(">=")),
                BinaryOp::Sub => (Ident::from_str("erlang"), Ident::from_str("-")),
                BinaryOp::Add => (Ident::from_str("erlang"), Ident::from_str("+")),
                BinaryOp::Append => (Ident::from_str("erlang"), Ident::from_str("++")),
                BinaryOp::Remove => (Ident::from_str("erlang"), Ident::from_str("--")),
                BinaryOp::Multiply => (Ident::from_str("erlang"), Ident::from_str("*")),
                BinaryOp::Divide => (Ident::from_str("erlang"), Ident::from_str("/")),
                BinaryOp::Rem => (Ident::from_str("erlang"), Ident::from_str("rem")),
                BinaryOp::Div => (Ident::from_str("erlang"), Ident::from_str("div")),
                BinaryOp::Equal => (Ident::from_str("erlang"), Ident::from_str("==")),
                BinaryOp::NotEqual => (Ident::from_str("erlang"), Ident::from_str("/=")),
                BinaryOp::StrictEqual => (Ident::from_str("erlang"), Ident::from_str("=:=")),
                BinaryOp::StrictNotEqual => (Ident::from_str("erlang"), Ident::from_str("=/=")),
                BinaryOp::Band => (Ident::from_str("erlang"), Ident::from_str("band")),
                BinaryOp::Bor => (Ident::from_str("erlang"), Ident::from_str("bor")),
                BinaryOp::Bsl => (Ident::from_str("erlang"), Ident::from_str("bsl")),
                BinaryOp::Bsr => (Ident::from_str("erlang"), Ident::from_str("bsr")),
                BinaryOp::Or => (Ident::from_str("erlang"), Ident::from_str("or")),
                BinaryOp::And => (Ident::from_str("erlang"), Ident::from_str("and")),
                BinaryOp::Send => (Ident::from_str("erlang"), Ident::from_str("!")),
                _ => unimplemented!("{:?}", op),
            };

            ctx.call_function(b, block, span, m, f, &[lhs_val, rhs_val])
        }
    }

}
