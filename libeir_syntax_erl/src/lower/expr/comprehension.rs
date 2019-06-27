use libeir_ir::{
    FunctionBuilder,
    Value as IrValue,
    Block as IrBlock,
};
use libeir_ir::constant::NilTerm;
use libeir_ir::op::BinOp;

use libeir_intern::{ Symbol, Ident };

use crate::parser::ast::{ Expr, ListComprehension };

use crate::lower::LowerCtx;
use crate::lower::expr::{ lower_single };
use crate::lower::pattern::lower_clause;

fn lower_qual<F>(ctx: &mut LowerCtx, b: &mut FunctionBuilder, inner: &F,
              quals: &[Expr], mut block: IrBlock, mut acc: IrValue) -> (IrBlock, IrValue)
where F: Fn(&mut LowerCtx, &mut FunctionBuilder, IrBlock, IrValue) -> (IrBlock, IrValue)
{
    if quals.len() == 0 {
        inner(ctx, b, block, acc)
    } else {
        match &quals[0] {
            Expr::Generator(gen) => {

                //     loop_block(list_val, acc)
                // loop_block(loop_list_arg, loop_acc_arg):
                //     binop equal(is_nil_block, non_nil_block, loop_list_arg, [])
                // is_nil_block:
                //     ret(loop_acc_arg)
                // non_nil_block:
                //     unpack_list_cell(unpack_ok_block, unpack_fail_block, loop_list_arg)
                // unpack_fail_block:
                //     throw non proper list
                // unpack_ok_block(head_val, tail_val):
                //     do match on head_val
                //     loop_block(tail_val, acc)

                let list_val = map_block!(block, lower_single(ctx, b, block, &gen.expr));

                // Loop entry block
                let loop_block = b.block_insert();
                let loop_list_arg = b.block_arg_insert(loop_block);
                let loop_acc_arg = b.block_arg_insert(loop_block);

                // Initial list cell unpack call
                b.op_call(block, loop_block, &[list_val, acc]);

                // Return block
                let ret_block;
                let ret_val;

                // Check for nil and unpack list
                let nil = b.value(NilTerm);
                let (is_nil_block, non_nil_block) = b.op_binop(loop_block, BinOp::Equal, loop_list_arg, nil);

                ret_block = is_nil_block;
                ret_val = loop_acc_arg;

                let (unpack_ok_block, unpack_fail_block) = b.op_unpack_list_cell(non_nil_block, loop_list_arg);
                let head_val = b.block_args(unpack_ok_block)[0];
                let tail_val = b.block_args(unpack_ok_block)[1];

                {
                    let typ = b.value(Symbol::intern("error"));
                    let error = b.value(Symbol::intern("function_clause"));
                    // TODO trace
                    let trace = b.value(NilTerm);
                    ctx.exc_stack.make_error_jump(b, unpack_fail_block, typ, error, trace);
                }

                // When there is no match, continue iterating
                let no_match = b.block_insert();
                b.op_call(no_match, loop_block, &[tail_val, acc]);

                match lower_clause(ctx, b, &mut block, [&*gen.pattern].iter().map(|i| *i), None) {
                    Some(lowered) => {

                        let mut case_b = b.op_case_build();
                        case_b.match_on = Some(head_val);
                        case_b.no_match = Some(b.value(no_match));

                        // Add to case
                        let body_val = b.value(lowered.body);
                        case_b.push_clause(lowered.clause, lowered.guard, body_val, b);

                        let (cont, cont_val) = lower_qual(ctx, b, inner, &quals[1..], lowered.body, acc);
                        b.op_call(cont, loop_block, &[tail_val, cont_val]);

                        // Pop scope pushed in lower_clause
                        ctx.scope.pop(lowered.scope_token);

                        case_b.finish(unpack_ok_block, b);

                        (ret_block, ret_val)
                    }
                    None => unimplemented!(), // TODO warn/error unreachable pattern
                }
            }
            Expr::BinaryGenerator(gen) => {
                unimplemented!()
            }
            expr => {
                let bool_val = map_block!(block, lower_single(ctx, b, block, expr));
                let (true_block, false_block, else_block) = b.op_if_bool(block, bool_val);

                let join_block = b.block_insert();
                let join_arg = b.block_arg_insert(join_block);

                let (cont, cont_val) = lower_qual(ctx, b, inner, &quals[1..], true_block, acc);
                b.op_call(cont, join_block, &[cont_val]);

                b.op_call(false_block, join_block, &[acc]);
                b.op_call(else_block, join_block, &[acc]);

                (join_block, join_arg)
            }
        }
    }
}

pub(super) fn lower_list_comprehension_expr(ctx: &mut LowerCtx, b: &mut FunctionBuilder, mut block: IrBlock,
                                            compr: &ListComprehension) -> (IrBlock, IrValue) {
    let inner = |ctx: &mut LowerCtx, b: &mut FunctionBuilder, mut block: IrBlock, acc: IrValue| {
        let val = map_block!(block, lower_single(ctx, b, block, &compr.body));
        block = b.op_make_list(block, &[val], acc);
        (block, b.block_args(block)[0])
    };

    let nil = b.value(NilTerm);
    let val = map_block!(block, lower_qual(ctx, b, &inner, &compr.qualifiers, block, nil));
    ctx.call_function(b, block, compr.span, Ident::from_str("lists"), Ident::from_str("reverse"), &[val])
}
