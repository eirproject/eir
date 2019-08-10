use std::collections::HashSet;

use libeir_ir::{
    FunctionBuilder,
};

use libeir_intern::{ Ident };

use crate::parser::ast::{ Expr, Var, BinaryExpr, BinaryOp };

use crate::lower::{ LowerCtx };

pub(super) fn collect(
    ctx: &LowerCtx,
    b: &FunctionBuilder,
    pattern: &Expr,
    binds: &mut HashSet<Ident>,
) {
    match pattern {
        Expr::Literal(_) => (),
        Expr::Nil(_) => (),
        Expr::Match(match_expr) => {
            collect(ctx, b, &match_expr.pattern, binds);
            collect(ctx, b, &match_expr.expr, binds);
        }
        Expr::Var(Var(_id, var)) => {
            if ctx.try_resolve(*var).is_err() {
                binds.insert(*var);
            }
        }
        Expr::Tuple(tup) => {
            for elem in tup.elements.iter() {
                collect(ctx, b, elem, binds);
            }
        }
        Expr::Cons(cons) => {
            collect(ctx, b, &cons.head, binds);
            collect(ctx, b, &cons.tail, binds);
        }
        Expr::Record(rec) => {
            for field in rec.fields.iter() {
                if let Some(expr) = field.value.as_ref() {
                    collect(ctx, b, expr, binds);
                }
            }
        }
        Expr::BinaryExpr(BinaryExpr { op: BinaryOp::Append, lhs, rhs, .. }) => {
            collect(ctx, b, lhs, binds);
            collect(ctx, b, rhs, binds);
        }
        Expr::Binary(bin) => {
            for elem in bin.elements.iter() {
                collect(ctx, b, &elem.bit_expr, binds);
            }
        }
        Expr::Map(map) => {
            for field in map.fields.iter() {
                collect(ctx, b, &field.value(), binds);
            }
        }
        _ => (),
    }
}
