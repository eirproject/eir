pub use libeir_ir::binary::{BinaryEntrySpecifier, Endianness};
use libeir_ir::{
    operation::binary_construct::{
        BinaryConstructFinish, BinaryConstructPush, BinaryConstructStart,
    },
    Block as IrBlock, FunctionBuilder, Value as IrValue,
};

use libeir_util_binary::BitVec;

use crate::parser::binary::{TypeName, specifier_to_typename, specifier_can_have_size, default_specifier};
use crate::parser::ast::{Binary, BinaryElement, Expr, Literal};

use crate::lower::{LowerCtx, LowerError};
use crate::lower::expr::lower_single_same_scope;

use crate::util::string_tokenizer::StringTokenizer;

fn make_size(
    ctx: &mut LowerCtx,
    b: &mut FunctionBuilder,
    block: &mut IrBlock,
    bit_size: &Option<Expr>,
    spec: BinaryEntrySpecifier,
) -> Option<IrValue> {
    let spec_typ = specifier_to_typename(&spec);

    if let Some(size_expr) = bit_size {
        if !specifier_can_have_size(&spec) {
            ctx.error(LowerError::BinaryInvalidSize {
                span: size_expr.span(),
                typ: spec_typ,
            });
            None
        } else {
            Some(map_block!(
                *block,
                lower_single_same_scope(ctx, b, *block, size_expr)
            ))
        }
    } else {
        match spec_typ {
            TypeName::Integer => Some(b.value(8)),
            TypeName::Float => Some(b.value(64)),
            _ => None,
        }
    }
}

/// Lowers a single entry of a binary construction element.
///
/// A couple of gotchas with the syntax:
/// - `<<"abc">>.`: obviously valid, creates a binary of the ascii characters
/// - `<<"™">>.`: also valid, encodes each CODEPOINT in the string as a byte,
///   truncated. This evaluates to `<<34>>`.
/// - `<<[97, 98, 99]>>.` not valid, while a string binary element is handled
///   as a special case, this does not apply to char lists.
/// - `<<"abc":10/integer>>` valid! a string literal is desugared to a list of
///   individual integers in the binary entry, each with the specifier on it.
pub(crate) fn lower_binary_elem(
    ctx: &mut LowerCtx,
    b: &mut FunctionBuilder,
    mut block: IrBlock,
    bin_ref: &mut IrValue,
    elem: &BinaryElement,
) -> IrBlock {
    let spec = elem.specifier.unwrap_or(default_specifier());

    let size_val = make_size(ctx, b, &mut block, &elem.bit_size, spec);

    let elem_val = match &elem.bit_expr {
        Expr::Literal(Literal::String(_id, string)) if !spec.is_native_endian() => {

            let tokenizer = StringTokenizer::new(*string);
            for elem in tokenizer {
                match elem {
                    Ok((cp, span)) => {
                        let elem_val = b.value(cp);
                        let (ok_cont, err_cont) = BinaryConstructPush::build(
                            b, block, *bin_ref, elem_val, spec, size_val,
                        );
                        *bin_ref = b.block_args(ok_cont)[0];
                        block = ok_cont;

                        // TODO: Proper error
                        b.op_unreachable(span, err_cont);
                    },
                    Err(err) => {
                        ctx.error(err.into());
                    },
                }
            }

            None
        }
        _ => Some(map_block!(
            block,
            lower_single_same_scope(ctx, b, block, &elem.bit_expr)
        )),
    };

    if let Some(elem_val) = elem_val {
        let (ok_cont, err_cont) =
            BinaryConstructPush::build(b, block, *bin_ref, elem_val, spec, size_val);
        *bin_ref = b.block_args(ok_cont)[0];
        block = ok_cont;

        // TODO: Proper error
        b.op_unreachable(elem.span, err_cont);
    }

    block
}

pub(super) fn lower_binary_expr(
    ctx: &mut LowerCtx,
    b: &mut FunctionBuilder,
    mut block: IrBlock,
    bin: Option<IrValue>,
    binary: &Binary,
) -> (IrBlock, IrValue) {
    block = BinaryConstructStart::build(b, block);
    let mut bin_ref = b.block_args(block)[0];

    if let Some(bin) = bin {
        let spec = BinaryEntrySpecifier::Bytes { unit: 1 };
        let (ok_cont, err_cont) = BinaryConstructPush::build(b, block, bin_ref, bin, spec, None);
        block = ok_cont;
        b.op_unreachable(binary.span, err_cont);

        bin_ref = b.block_args(ok_cont)[0];
    }

    let mut bitvec = BitVec::new();

    for elem in binary.elements.iter() {
        let resolve_rec_idx = |name, field| ctx.resolve_rec_idx(name, field);
        match crate::util::binary::append_static_binary_element(elem, &mut bitvec, Some(&resolve_rec_idx)) {
            // This means the binary element was fully static, and the data has
            // been appended to the BitVec.
            Ok(()) => (),
            // Element was dynamic, we need to generate binary construction
            // operations.
            Err(_) => {
                // If we have static data before this dynamic element, we
                // need to append that first.
                if !bitvec.empty() {
                    let bin = b.value(bitvec);
                    let spec = BinaryEntrySpecifier::Bits { unit: 1 };

                    let (ok_cont, err_cont) = BinaryConstructPush::build(b, block, bin_ref, bin, spec, None);
                    bin_ref = b.block_args(ok_cont)[0];
                    block = ok_cont;

                    // We know the value we appended is a BitVec, this can never
                    // fail.
                    b.op_unreachable(binary.span, err_cont);

                    bitvec = BitVec::new();
                }

                block = lower_binary_elem(ctx, b, block, &mut bin_ref, elem);
            },
        }
    }

    if !bitvec.empty() {
        let bin = b.value(bitvec);
        let spec = BinaryEntrySpecifier::Bits { unit: 1 };

        let (ok_cont, err_cont) = BinaryConstructPush::build(b, block, bin_ref, bin, spec, None);
        bin_ref = b.block_args(ok_cont)[0];
        block = ok_cont;

        // We know the value we appended is a BitVec, this can never
        // fail.
        b.op_unreachable(binary.span, err_cont);
    }

    block = BinaryConstructFinish::build(b, block, bin_ref);
    let res_arg = b.block_args(block)[0];

    (block, res_arg)
}
