use libeir_ir::binary::BinaryEntrySpecifier;
use libeir_ir::constant::NilTerm;
use libeir_ir::operation::binary_construct::{
    BinaryConstructFinish, BinaryConstructPush, BinaryConstructStart,
};
use libeir_ir::operation::case::Case;
use libeir_ir::BinOp;
use libeir_ir::{Block as IrBlock, FunctionBuilder, Value as IrValue};
use libeir_ir::pattern::{PatternContainer, PatternClause};

use libeir_intern::{Ident, Symbol};

use crate::parser::ast::{BinaryComprehension, Expr, ListComprehension};

use crate::lower::expr::binary::lower_binary_expr;
use crate::lower::expr::{lower_single, lower_single_same_scope};
use crate::lower::pattern::lower_clause;
use crate::lower::LowerCtx;

fn make_structural_bin_pattern(
    pat: &mut PatternContainer,
    expr: &Expr,
) -> PatternClause {
    match expr {
        Expr::Binary(bin) => {
            unimplemented!()
        }
        _ => unimplemented!(),
    }
}

fn lower_qual<F>(
    ctx: &mut LowerCtx,
    b: &mut FunctionBuilder,
    inner: &F,
    quals: &[Expr],
    mut block: IrBlock,
    acc: IrValue,
) -> (IrBlock, IrValue)
where
    F: Fn(&mut LowerCtx, &mut FunctionBuilder, IrBlock, IrValue) -> (IrBlock, IrValue),
{
    if quals.len() == 0 {
        inner(ctx, b, block, acc)
    } else {
        match &quals[0] {
            Expr::Generator(gen) => {
                let gen_span = gen.span;

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
                b.op_call_flow(block, loop_block, &[list_val, acc]);

                // Return block
                let ret_block;
                let ret_val;

                // Check for nil and unpack list
                let nil = b.value(NilTerm);
                let comp_res = b.prim_binop(gen_span, BinOp::Equal, loop_list_arg, nil);
                let (is_nil_block, non_nil_block) =
                    b.op_if_bool_strict(gen_span, loop_block, comp_res);

                ret_block = is_nil_block;
                ret_val = loop_acc_arg;

                let mut match_builder = b.op_match_build(gen_span);
                let unpack_ok_block = match_builder.push_list_cell(b);
                let unpack_fail_block = match_builder.push_wildcard(gen_span, b);
                match_builder.finish(non_nil_block, loop_list_arg, b);

                let head_val = b.block_args(unpack_ok_block)[0];
                let tail_val = b.block_args(unpack_ok_block)[1];

                {
                    let typ = b.value(Symbol::intern("error"));
                    let error = b.value(Symbol::intern("function_clause"));
                    ctx.exc_stack
                        .make_error_jump(b, gen_span, unpack_fail_block, typ, error);
                }

                // When there is no match, continue iterating
                let no_match = b.block_insert();
                b.op_call_flow(no_match, loop_block, &[tail_val, acc]);

                block = unpack_ok_block;
                let pattern_span = gen.pattern.span();

                let mut case_b = Case::builder();
                case_b.set_span(pattern_span);
                case_b.match_on = Some(head_val);
                case_b.no_match = Some(b.value(no_match));

                match lower_clause(
                    ctx,
                    &mut case_b.container,
                    b,
                    &mut block,
                    false,
                    pattern_span,
                    [&*gen.pattern].iter().map(|i| *i),
                    None,
                ) {
                    Ok(lowered) => {
                        let (scope_token, body) = lowered.make_body(ctx, b);

                        // Add to case
                        let body_val = b.value(body);
                        case_b.push_clause(lowered.clause, lowered.guard, body_val, b);
                        for value in lowered.values.iter() {
                            case_b.push_value(*value, b);
                        }

                        let (cont, cont_val) =
                            lower_qual(ctx, b, inner, &quals[1..], body, loop_acc_arg);
                        b.op_call_flow(cont, loop_block, &[tail_val, cont_val]);

                        // Pop scope pushed in lower_clause
                        ctx.scope.pop(scope_token);

                        case_b.finish(block, b);

                        (ret_block, ret_val)
                    }
                    Err(_) => unimplemented!(), // TODO warn/error unreachable pattern
                }
            }
            Expr::BinaryGenerator(gen) => {
                let gen_span = gen.span;

                //     loop_block(bin_val, acc)
                // loop_block(loop_bin_arg, loop_acc_arg):
                //     do match on loop_bin_arg
                //         1. full pattern, append acc and continue
                //         2. structural pattern, continue
                //         3. wildcard, break

                let bin_val = map_block!(block, lower_single(ctx, b, block, &gen.expr));

                // Loop entry block
                let loop_block = b.block_insert();
                let loop_bin_arg = b.block_arg_insert(loop_block);
                let loop_acc_arg = b.block_arg_insert(loop_block);

                let pattern_span = gen.pattern.span();

                // No match block, break
                let no_match = b.block_insert();

                let mut case_b = Case::builder();
                case_b.set_span(pattern_span);
                case_b.match_on = Some(loop_bin_arg);
                case_b.no_match = Some(b.value(no_match));

                let structual_pattern = make_structural_bin_pattern(&mut case_b.container, &gen.expr);

                (no_match, loop_acc_arg)
            },
            expr => {
                let bool_val = map_block!(block, lower_single_same_scope(ctx, b, block, expr));
                let span = expr.span();
                let (true_block, false_block, else_block) = b.op_if_bool(span, block, bool_val);

                let join_block = b.block_insert();
                let join_arg = b.block_arg_insert(join_block);

                let (cont, cont_val) = lower_qual(ctx, b, inner, &quals[1..], true_block, acc);
                b.op_call_flow(cont, join_block, &[cont_val]);

                b.op_call_flow(false_block, join_block, &[acc]);
                b.op_call_flow(else_block, join_block, &[acc]);

                (join_block, join_arg)
            }
        }
    }
}

pub(super) fn lower_list_comprehension_expr(
    ctx: &mut LowerCtx,
    b: &mut FunctionBuilder,
    mut block: IrBlock,
    compr: &ListComprehension,
) -> (IrBlock, IrValue) {
    let inner = |ctx: &mut LowerCtx, b: &mut FunctionBuilder, mut block: IrBlock, acc: IrValue| {
        let span = compr.body.span();
        let val = map_block!(block, lower_single(ctx, b, block, &compr.body));
        let cell = b.prim_list_cell(span, val, acc);
        (block, cell)
    };

    let nil = b.value(NilTerm);
    let val = map_block!(
        block,
        lower_qual(ctx, b, &inner, &compr.qualifiers, block, nil)
    );
    ctx.call_function(
        b,
        block,
        compr.span,
        Ident::from_str("lists"),
        Ident::from_str("reverse"),
        &[val],
    )
}

pub(super) fn lower_binary_comprehension_expr(
    ctx: &mut LowerCtx,
    b: &mut FunctionBuilder,
    mut block: IrBlock,
    compr: &BinaryComprehension,
) -> (IrBlock, IrValue) {
    let inner =
        |ctx: &mut LowerCtx, b: &mut FunctionBuilder, mut block: IrBlock, bin_ref: IrValue| {
            let val = map_block!(block, lower_single(ctx, b, block, &compr.body));
            let spec = BinaryEntrySpecifier::Bytes { unit: 1 };
            let (ok_cont, err_cont) =
                BinaryConstructPush::build(b, block, bin_ref, val, spec, None);

            b.op_unreachable(compr.span, err_cont);

            let bin_ref = b.block_args(ok_cont)[0];
            (ok_cont, bin_ref)
        };

    block = BinaryConstructStart::build(b, block);
    let mut bin_ref = b.block_args(block)[0];

    bin_ref = map_block!(
        block,
        lower_qual(ctx, b, &inner, &compr.qualifiers, block, bin_ref)
    );

    block = BinaryConstructFinish::build(b, block, bin_ref);
    let res = b.block_args(block)[0];

    (block, res)
}
