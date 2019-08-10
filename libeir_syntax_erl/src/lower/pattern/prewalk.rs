use std::collections::HashMap;

use libeir_ir::{
    FunctionBuilder,
    Const,
};

use super::{ LowerCtx, LowerError };
use crate::parser::ast::{ NodeId, Expr, Literal, BinaryExpr, BinaryOp };

pub enum PrewalkFail {
    Disjoint,
    Fail,
}

fn prewalk_append(
    ctx: &mut LowerCtx,
    b: &mut FunctionBuilder,
    constants: &mut HashMap<NodeId, Const>,
    pattern: &Expr,
    tail: Const,
) -> Result<Option<Const>, PrewalkFail> {
    match pattern {
        Expr::Nil(_) => Ok(Some(tail)),
        Expr::BinaryExpr(BinaryExpr { op: BinaryOp::Append, lhs, rhs, .. }) => {
            if let Some(tail) = prewalk_append(ctx, b, constants, rhs, tail)? {
                if let Some(tail) = prewalk_append(ctx, b, constants, lhs, tail)? {
                    Ok(Some(tail))
                } else {
                    Ok(None)
                }
            } else {
                Ok(None)
            }
        }
        _ => unimplemented!(),
    }
}

pub(super) fn prewalk_pattern(
    ctx: &mut LowerCtx,
    b: &mut FunctionBuilder,
    constants: &mut HashMap<NodeId, Const>,
    pattern: &Expr,
) -> Result<Option<Const>, PrewalkFail> {
    let res = match pattern {
        Expr::Nil(nil) => {
            Some(b.cons_mut().nil())
        }
        Expr::Literal(lit) => match lit {
            Literal::Atom(_id, ident) =>
                Some(b.cons_mut().from(*ident)),
            Literal::Char(_span, _id, c) =>
                Some(b.cons_mut().from(*c)),
            Literal::Integer(_span, id, num) =>
                Some(b.cons_mut().from(*num)),
            Literal::BigInteger(_span, id, int) =>
                Some(b.cons_mut().from(int.clone())),
            Literal::Float(_span, id, num) =>
                Some(b.cons_mut().from(*num)),
            Literal::String(id, ident) => {
                match crate::lower::expr::literal::intern_string_const(
                    *ident, b.cons_mut())
                {
                    Ok(cons) => Some(cons),
                    Err(err) => {
                        ctx.error(err);
                        None
                    },
                }
            },
        }
        Expr::Tuple(tup) => {
            let mut all_consts = true;
            let mut tup_b = b.cons_mut().tuple_builder();
            for field in tup.elements.iter() {
                let c = prewalk_pattern(ctx, b, constants, field)?;
                if let Some(child) = c {
                    tup_b.push(child, b.cons_mut());
                } else {
                    all_consts = false;
                }
            }

            if all_consts {
                Some(tup_b.finish(b.cons_mut()))
            } else {
                tup_b.clear(b.cons_mut());
                None
            }
        }
        Expr::Cons(cons) => {
            let head_r = prewalk_pattern(ctx, b, constants, &cons.head)?;
            let tail_r = prewalk_pattern(ctx, b, constants, &cons.tail)?;
            if let (Some(head), Some(tail)) = (head_r, tail_r) {
                Some(b.cons_mut().list_cell(head, tail))
            } else {
                None
            }
        }
        // Since patterns are not matched by equality semantics, we can't
        // combine it as a constant here.
        Expr::Map(map) => {
            for field in map.fields.iter() {
                prewalk_pattern(ctx, b, constants, &field.key())?;
                prewalk_pattern(ctx, b, constants, &field.value())?;
            }
            None
        },
        // Since we don't allow merging of binaries, it is fine to not
        // combine these as constants.
        Expr::Binary(bin) => {
            for elem in &bin.elements {
                prewalk_pattern(ctx, b, constants, &elem.bit_expr)?;
                if let Some(e) = elem.bit_size.as_ref() {
                    prewalk_pattern(ctx, b, constants, &e)?;
                }
            }
            None
        },
        Expr::BinaryExpr(BinaryExpr { op: BinaryOp::Append, lhs, rhs, .. }) => {
            let tail = prewalk_pattern(ctx, b, constants, rhs)?;
            if let Some(tail) = tail {
                prewalk_append(ctx, b, constants, lhs, tail)?
            } else {
                None
            }
        },
        Expr::Record(rec) => {
            let rec_def = &ctx.module.records[&rec.name.name];

            let fields_len = rec_def.record.fields.len();

            let mut all_consts = true;
            let mut elems = vec![None; fields_len];
            for field in rec.fields.iter() {
                let idx = rec_def.field_idx_map[&field.name];
                if elems[idx].is_some() {
                    ctx.error(LowerError::DuplicateRecordField {
                        new: field.name.span,
                        old: rec.fields.iter()
                            .find(|f| f.name == field.name)
                            .unwrap()
                            .name.span,
                    });
                    return Err(PrewalkFail::Fail);
                }

                let value = prewalk_pattern(
                    ctx, b, constants, &field.value.as_ref().unwrap())?;
                if value.is_none() {
                    all_consts = false;
                }
                elems[idx] = Some(value);
            }

            if all_consts {
                let mut tup = b.cons_mut().tuple_builder();

                let recname_val = b.cons_mut().from(rec.name);
                tup.push(recname_val, b.cons_mut());

                for elem in elems.iter() {
                    tup.push(elem.unwrap().unwrap(), b.cons_mut());
                }

                Some(tup.finish(&mut b.cons_mut()))
            } else {
                None
            }
        },
        Expr::RecordAccess(_) => unimplemented!(),
        Expr::RecordIndex(_) => unimplemented!(),
        Expr::RecordUpdate(_) => unimplemented!(),
        Expr::Match(match_expr) => {
            let l_const = prewalk_pattern(ctx, b, constants, &match_expr.pattern)?;
            let r_const = prewalk_pattern(ctx, b, constants, &match_expr.expr)?;
            if l_const.is_some() & r_const.is_some() {
                if l_const != r_const {
                    ctx.error(LowerError::DisjointPatternUnionWarning {
                        left: match_expr.pattern.span(),
                        right: match_expr.expr.span(),
                    });
                    return Err(PrewalkFail::Disjoint);
                }
                l_const
            } else {
                None
            }
        }
        _ => None,
    };

    if let Some(cons) = res {
        constants.insert(pattern.id(), cons);
    }

    Ok(res)
}
