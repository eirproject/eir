use libeir_ir::{
    constant::EmptyMap, Block as IrBlock, FunctionBuilder, MapPutUpdate, Value as IrValue,
};

use libeir_diagnostics::SourceSpan;
use libeir_intern::Symbol;

use crate::lower::{lower_single, LowerCtx, LowerError};
use crate::parser::ast::{Map, MapField, MapUpdate};

pub(super) fn lower_map_update_expr(
    ctx: &mut LowerCtx,
    b: &mut FunctionBuilder,
    mut block: IrBlock,
    map: &MapUpdate,
) -> (IrBlock, IrValue) {
    let entry_map_val = map_block!(block, lower_single(ctx, b, block, &map.map));

    if let Some(cons) = b.fun().value_const(entry_map_val) {
        if b.cons().const_kind(cons).is_map() {
            // TODO: More warnings
        } else {
            ctx.warn(LowerError::MapUpdateOnNonMap { map: map.span });
        }
    }

    lower(ctx, b, block, entry_map_val, map.span, &map.updates)
}

pub(super) fn lower_map_expr(
    ctx: &mut LowerCtx,
    b: &mut FunctionBuilder,
    block: IrBlock,
    map: &Map,
) -> (IrBlock, IrValue) {
    for field in map.fields.iter() {
        match field {
            MapField::Exact { .. } => {
                ctx.error(LowerError::MapUpdateInConstruction {
                    map: map.span,
                    field: field.span(),
                });
                // No need to generate more than one error here,
                // the user will probably have gotten the point.
                break;
            }
            _ => (),
        }
    }

    let empty_map = b.value(EmptyMap);
    lower(ctx, b, block, empty_map, map.span, &map.fields)
}

fn lower(
    ctx: &mut LowerCtx,
    b: &mut FunctionBuilder,
    mut block: IrBlock,
    old_map: IrValue,
    span: SourceSpan,
    fields: &[MapField],
) -> (IrBlock, IrValue) {
    let mut map_builder = b.op_map_put_build(span, old_map);

    for field in fields.iter() {
        let (key, value, action) = match field {
            MapField::Assoc { key, value, .. } => (key, value, MapPutUpdate::Put),
            MapField::Exact { key, value, .. } => (key, value, MapPutUpdate::Update),
        };

        let key_val = map_block!(block, lower_single(ctx, b, block, key));
        let value_val = map_block!(block, lower_single(ctx, b, block, value));
        map_builder.push_kv(key_val, value_val, action, b);
    }

    let loc = ctx.current_location(b, span);
    b.block_set_location(block, loc);
    let (ok, fail) = map_builder.finish(block, b);

    b.block_set_location(fail, loc);

    let typ_val = b.value(Symbol::intern("error"));
    let badmatch_val = b.value(Symbol::intern("badkey"));
    let failed_key = b.block_args(fail)[0];
    let err_val = b.prim_tuple(span, &[badmatch_val, failed_key]);
    ctx.exc_stack
        .make_error_jump(b, span, fail, typ_val, err_val);

    (ok, b.block_args(ok)[0])
}
