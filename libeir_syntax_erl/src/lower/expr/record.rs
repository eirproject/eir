use libeir_diagnostics::SourceSpan;
use libeir_ir::{BinOp as IrBinOp, Block as IrBlock, FunctionBuilder, Value as IrValue};

use libeir_intern::Symbol;

use crate::parser::ast::{Record, RecordAccess, RecordIndex, RecordUpdate};

use crate::lower::expr::lower_single;
use crate::lower::{LowerCtx, LowerError};

fn make_rec_fail(
    ctx: &mut LowerCtx,
    b: &mut FunctionBuilder,
    span: SourceSpan,
    recname_val: IrValue,
) -> IrBlock {
    let fail_block = b.block_insert();
    let block = fail_block;

    let fail_type = b.value(Symbol::intern("error")); // TODO double check correct type

    let badrecord_val = b.value(Symbol::intern("badrecord"));
    let fail_error = b.prim_tuple(span, &[badrecord_val, recname_val]);

    ctx.exc_stack
        .make_error_jump(b, span, block, fail_type, fail_error);

    fail_block
}

pub(super) fn lower_record_access_expr(
    ctx: &mut LowerCtx,
    b: &mut FunctionBuilder,
    mut block: IrBlock,
    rec: &RecordAccess,
) -> (IrBlock, IrValue) {
    let span = rec.span;
    let rec_def = &ctx.module.records[&rec.name.name];
    let recname_val = b.value(rec.name);

    let idx = rec_def.field_idx_map[&rec.field];

    let fail_block = make_rec_fail(ctx, b, span, recname_val);

    let record_val = map_block!(block, lower_single(ctx, b, block, &rec.record));

    let mut match_builder = b.op_match_build(span);
    let unpack_ok_block = match_builder.push_tuple(rec_def.record.fields.len() + 1, b);
    let unpack_fail_block = match_builder.push_wildcard(span, b);
    match_builder.finish(block, record_val, b);
    block = unpack_ok_block;

    b.op_call_flow(unpack_fail_block, fail_block, &[]);

    let recname_test_val = b.block_args(block)[0];
    let rec_field_val = b.block_args(block)[idx + 1];

    let eq_cond = b.prim_binop(span, IrBinOp::Equal, recname_test_val, recname_val);
    let eq_fail_block = map_block!(block, b.op_if_bool_strict(span, block, eq_cond));
    b.op_call_flow(eq_fail_block, fail_block, &[]);

    (block, rec_field_val)
}

pub(super) fn lower_record_update_expr(
    ctx: &mut LowerCtx,
    b: &mut FunctionBuilder,
    mut block: IrBlock,
    rec: &RecordUpdate,
) -> (IrBlock, IrValue) {
    let span = rec.span;
    // TODO Warn/error when updates overlap?
    let rec_def = &ctx.module.records[&rec.name.name];
    let recname_val = b.value(rec.name);

    let num_fields = rec_def.record.fields.len();

    let fail_block = make_rec_fail(ctx, b, span, recname_val);

    // Unpack tuple
    let record_val = map_block!(block, lower_single(ctx, b, block, &rec.record));

    let mut match_builder = b.op_match_build(span);
    let unpack_ok_block = match_builder.push_tuple(num_fields + 1, b);
    let unpack_fail_block = match_builder.push_wildcard(span, b);
    match_builder.finish(block, record_val, b);
    block = unpack_ok_block;

    b.op_call_flow(unpack_fail_block, fail_block, &[]);

    // Make a vector with all the values in the unpacked tuple
    let mut elems = Vec::with_capacity(num_fields);
    for idx in 0..num_fields {
        elems.push(b.block_args(block)[idx + 1]);
    }

    // Check first tuple element
    let recname_test_val = b.block_args(block)[0];
    let eq_cond = b.prim_binop(span, IrBinOp::Equal, recname_test_val, recname_val);
    let eq_fail_block = map_block!(block, b.op_if_bool_strict(span, block, eq_cond));
    b.op_call_flow(eq_fail_block, fail_block, &[]);

    // Update fields
    for update in rec.updates.iter() {
        let idx = rec_def.field_idx_map[&update.name];
        let new_val = map_block!(
            block,
            lower_single(ctx, b, block, update.value.as_ref().unwrap())
        );
        elems[idx] = new_val;
    }

    // Create new tuple
    let mut tup_values = Vec::new();
    tup_values.push(recname_val);
    tup_values.extend(elems.iter().cloned());
    let tup = b.prim_tuple(span, &tup_values);

    (block, tup)
}

pub(super) fn lower_record_expr(
    ctx: &mut LowerCtx,
    b: &mut FunctionBuilder,
    mut block: IrBlock,
    rec: &Record,
) -> (IrBlock, IrValue) {
    let span = rec.span;
    let rec_def = &ctx.module.records[&rec.name.name];
    let recname_val = b.value(rec.name);

    let num_fields = rec_def.record.fields.len();

    let mut elems = vec![None; num_fields];

    // Populate values from expression
    for field in rec.fields.iter() {
        let idx = rec_def.field_idx_map[&field.name];

        if elems[idx].is_some() {
            ctx.error(LowerError::DuplicateRecordField {
                new: field.name.span,
                old: rec
                    .fields
                    .iter()
                    .find(|f| f.name == field.name)
                    .unwrap()
                    .name
                    .span,
            });
        }

        let new_val = map_block!(
            block,
            lower_single(ctx, b, block, field.value.as_ref().unwrap())
        );
        elems[idx] = Some(new_val);
    }

    // Fill with defaults
    for (idx, field) in rec_def.record.fields.iter().enumerate() {
        if elems[idx].is_none() {
            let new_val = if let Some(const_expr) = field.value.as_ref() {
                // TODO: Allow only constants. This should be a separate lowering method!
                map_block!(block, lower_single(ctx, b, block, const_expr))
            } else {
                b.value(Symbol::intern("undefined"))
            };
            elems[idx] = Some(new_val);
        }
    }

    let mut tup_values = Vec::new();
    tup_values.push(recname_val);
    tup_values.extend(elems.iter().map(|e| e.unwrap()));
    let tup = b.prim_tuple(span, &tup_values);

    (block, tup)
}

pub(super) fn lower_record_index(
    ctx: &mut LowerCtx,
    b: &mut FunctionBuilder,
    block: IrBlock,
    rec: &RecordIndex,
) -> (IrBlock, IrValue) {
    let rec_def = &ctx.module.records[&rec.name.name];
    let index = rec_def.field_idx_map[&rec.field];
    let val = b.value(index);
    (block, val)
}
