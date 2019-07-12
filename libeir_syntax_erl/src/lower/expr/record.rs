use std::collections::HashMap;

use libeir_ir::{
    Module as IrModule,
    FunctionBuilder,
    Value as IrValue,
    Block as IrBlock,
    BinOp as IrBinOp,
};
use libeir_ir::constant::NilTerm;

use libeir_intern::Symbol;

use crate::parser::ast::{ Record, RecordAccess, RecordUpdate };

use crate::lower::LowerCtx;
use crate::lower::expr::lower_single;

fn make_rec_fail(ctx: &mut LowerCtx, b: &mut FunctionBuilder, recname_val: IrValue) -> IrBlock {
    let fail_block = b.block_insert();
    let mut block = fail_block;

    let fail_type = b.value(Symbol::intern("error")); // TODO double check correct type

    // TODO: support constant tuples, intern constant directly
    let badrecord_val = b.value(Symbol::intern("badrecord"));
    block = b.op_make_tuple(block, &[badrecord_val, recname_val]);
    let fail_error = b.block_args(block)[0];

    // TODO: Trace
    let fail_trace = b.value(NilTerm);

    ctx.exc_stack.make_error_jump(b, block, fail_type, fail_error, fail_trace);

    fail_block
}

pub(super) fn lower_record_access_expr(ctx: &mut LowerCtx, b: &mut FunctionBuilder, mut block: IrBlock,
                                       rec: &RecordAccess) -> (IrBlock, IrValue) {
    let rec_def = &ctx.module.records[&rec.name.name];
    let recname_val = b.value(rec.name);

    let idx = rec_def.fields.iter().enumerate()
        .find(|(_idx, field)| field.name.symbol() == rec.field.name)
        .map(|(idx, _field)| idx)
        .unwrap();

    let fail_block = make_rec_fail(ctx, b, recname_val);

    let record_val = map_block!(block, lower_single(ctx, b, block, &rec.record));
    let unpack_fail_block = map_block!(block, b.op_unpack_tuple(block, record_val, rec_def.fields.len()+1));
    b.op_call(unpack_fail_block, fail_block, &[]);

    let recname_test_val = b.block_args(block)[0];
    let rec_field_val = b.block_args(block)[idx + 1];

    let eq_fail_block = map_block!(block, b.op_binop(block, IrBinOp::Equal, recname_test_val, recname_val));
    b.op_call(eq_fail_block, fail_block, &[]);

    (block, rec_field_val)
}

pub(super) fn lower_record_update_expr(ctx: &mut LowerCtx, b: &mut FunctionBuilder, mut block: IrBlock,
                                       rec: &RecordUpdate) -> (IrBlock, IrValue) {
    let rec_def = &ctx.module.records[&rec.name.name];
    let recname_val = b.value(rec.name);

    // Make a map of field_name => elem_idx
    let mut field_idxs = HashMap::new();
    for (idx, field) in rec_def.fields.iter().enumerate() {
        assert!(!field_idxs.contains_key(&field.name.symbol()));
        field_idxs.insert(field.name.symbol(), idx);
    }

    let fail_block = make_rec_fail(ctx, b, recname_val);

    // Unpack tuple
    let record_val = map_block!(block, lower_single(ctx, b, block, &rec.record));
    let unpack_fail_block = map_block!(block, b.op_unpack_tuple(block, record_val, rec_def.fields.len()+1));
    b.op_call(unpack_fail_block, fail_block, &[]);

    // Make a vector with all the values in the unpacked tuple
    let mut elems = Vec::with_capacity(rec_def.fields.len());
    for (idx, _field) in rec_def.fields.iter().enumerate() {
        elems.push(b.block_args(block)[idx]);
    }

    // Check first tuple element
    let recname_test_val = b.block_args(block)[0];
    let eq_fail_block = map_block!(block, b.op_binop(block, IrBinOp::Equal, recname_test_val, recname_val));
    b.op_call(eq_fail_block, fail_block, &[]);

    // Update fields
    for update in rec.updates.iter() {
        let idx = field_idxs[&update.name.symbol()];
        let new_val = map_block!(block, lower_single(ctx, b, block, update.value.as_ref().unwrap()));
        elems[idx] = new_val;
    }

    // Create new tuple
    let mut tup_b = b.op_make_tuple_build();

    tup_b.push_value(recname_val, b);
    for elem in elems.iter() {
        tup_b.push_value(*elem, b);
    }

    tup_b.block = Some(block);
    block = tup_b.finish(b);

    (block, b.block_args(block)[0])
}

pub(super) fn lower_record_expr(ctx: &mut LowerCtx, b: &mut FunctionBuilder, mut block: IrBlock,
                                rec: &Record) -> (IrBlock, IrValue) {
    let rec_def = &ctx.module.records[&rec.name.name];
    let recname_val = b.value(rec.name);

    // Make a map of field_name => elem_idx
    let mut field_idxs = HashMap::new();
    for (idx, field) in rec_def.fields.iter().enumerate() {
        assert!(!field_idxs.contains_key(&field.name.symbol()));
        field_idxs.insert(field.name.symbol(), idx);
    }

    let mut elems = vec![None; rec_def.fields.len()];

    // Populate values from expression
    for field in rec.fields.iter() {
        let idx = field_idxs[&field.name.symbol()];
        let new_val = map_block!(block, lower_single(ctx, b, block, field.value.as_ref().unwrap()));
        elems[idx] = Some(new_val);
    }

    // Fill with defaults
    for (idx, field) in rec_def.fields.iter().enumerate() {
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

    let mut tup_b = b.op_make_tuple_build();

    tup_b.push_value(recname_val, b);
    for elem in elems.iter() {
        tup_b.push_value(elem.unwrap(), b);
    }

    tup_b.block = Some(block);
    block = tup_b.finish(b);

    (block, b.block_args(block)[0])
}


