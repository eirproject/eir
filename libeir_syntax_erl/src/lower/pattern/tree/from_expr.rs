use either::Either;
use std::convert::TryInto;

use cranelift_entity::EntityList;

use libeir_ir::{
    AtomicTerm, BigIntTerm, BinaryTerm, Block, FloatTerm, FunctionBuilder, IntTerm, NilTerm,
};

use libeir_diagnostics::SourceSpan;
use libeir_intern::Ident;

use libeir_util_number::bigint::BigInt;

use crate::evaluator::{eval_expr, ResolveRecordIndexError, Term};
use crate::lower::{lower_single, LowerCtx, LowerError};
use crate::parser::ast::{Binary, BinaryExpr, BinaryOp, Expr, Literal, UnaryExpr, UnaryOp, Var};

use super::{Tree, TreeNode, TreeNodeKind};

fn pattern_to_tree_node_append_tail(
    ctx: &mut LowerCtx,
    b: &mut FunctionBuilder,
    pre_block: &mut Block,
    t: &mut Tree,
    expr: &Expr,
    tail: TreeNode,
    span: SourceSpan,
) -> TreeNode {
    match expr {
        Expr::Literal(Literal::String(_id, ident)) => {
            let mut tokens = Vec::new();
            let ret = crate::lower::strings::tokenize_string(*ident, &mut |cp, _si| {
                tokens.push(cp);
                Ok(())
            });
            match ret {
                Ok(()) => (),
                Err(err) => {
                    ctx.error(err);
                    return t.nodes.push(TreeNodeKind::Wildcard(span));
                }
            };

            let mut node = tail;
            for c in tokens.iter().rev() {
                let head = t
                    .nodes
                    .push(TreeNodeKind::Atomic(span, b.cons_mut().from(*c)));
                node = t.nodes.push(TreeNodeKind::Cons {
                    span: span,
                    head,
                    tail: node,
                });
            }

            node
        }
        Expr::Nil(_) => tail,
        Expr::Cons(cons) => {
            let tail =
                pattern_to_tree_node_append_tail(ctx, b, pre_block, t, &cons.tail, tail, span);
            let head = pattern_to_tree_node(ctx, b, pre_block, t, &cons.head);

            t.nodes.push(TreeNodeKind::Cons {
                span: span,
                head: head,
                tail: tail,
            })
        }
        _ => unimplemented!("{:?}", expr),
    }
}

//fn eval_const_expr(
//    ctx: &mut LowerCtx,
//    b: &mut FunctionBuilder,
//    pre_block: &mut Block,
//    expr: &Expr,
//) -> Option<AtomicTerm> {
//    use libeir_util_number::Integer;
//    macro_rules! toi {
//        ($int:expr) => {{
//            let r: Integer = ($int).into();
//            r
//        }};
//    }
//
//    let res = match expr {
//        Expr::Literal(lit) => match lit {
//            Literal::Integer(_span, _id, int) => int.clone().into(),
//            Literal::Float(_span, _id, float) => (*float).into(),
//
//            _ => {
//                ctx.error(LowerError::InvalidPatternConstExpression {
//                    span: bin_expr.span,
//                });
//                return None;
//            }
//        },
//        Expr::UnaryExpr(
//            unary @ UnaryExpr {
//                op: UnaryOp::Minus, ..
//            },
//        ) => match eval_const_expr(ctx, b, pre_block, &unary.operand)? {
//            AtomicTerm::Int(IntTerm(int)) => AtomicTerm::Int(IntTerm(-int)),
//            val => unimplemented!("{:?}", val),
//        },
//        Expr::BinaryExpr(bin_expr) => {
//            use AtomicTerm as A;
//            use BinaryOp as B;
//
//            let lhs = eval_const_expr(ctx, b, pre_block, &bin_expr.lhs)?;
//            let rhs = eval_const_expr(ctx, b, pre_block, &bin_expr.rhs)?;
//
//            match (bin_expr.op, lhs, rhs) {
//                (B::Add, A::Int(IntTerm(l)), A::Int(IntTerm(r))) => (toi!(l) + &toi!(r)).into(),
//                (B::Add, A::Int(IntTerm(l)), A::Float(FloatTerm(r))) => (l as f64 + r).into(),
//                (B::Add, A::Float(FloatTerm(l)), A::Int(IntTerm(r))) => (l + r as f64).into(),
//                (B::Add, A::Float(FloatTerm(l)), A::Float(FloatTerm(r))) => (l + r).into(),
//
//                (B::Sub, A::Int(IntTerm(l)), A::Int(IntTerm(r))) => (toi!(l) - &toi!(r)).into(),
//                (B::Sub, A::Int(IntTerm(l)), A::Float(FloatTerm(r))) => (l as f64 - r).into(),
//                (B::Sub, A::Float(FloatTerm(l)), A::Int(IntTerm(r))) => (l - r as f64).into(),
//                (B::Sub, A::Float(FloatTerm(l)), A::Float(FloatTerm(r))) => (l - r).into(),
//
//                _ => {
//                    ctx.error(LowerError::InvalidPatternConstExpression {
//                        span: bin_expr.span,
//                    });
//                    return None;
//                }
//            }
//        }
//        Expr::BinaryExpr(
//            binary @ BinaryExpr {
//                op: BinaryOp::Add, ..
//            },
//        ) => {
//            let lhs = eval_const_expr(ctx, b, pre_block, &binary.lhs)?;
//            let rhs = eval_const_expr(ctx, b, pre_block, &binary.rhs)?;
//            match (lhs, rhs) {
//                (AtomicTerm::Int(IntTerm(l)), AtomicTerm::Int(IntTerm(r))) => {
//                    AtomicTerm::Int(IntTerm(l + r))
//                }
//                val => unimplemented!("{:?}", val),
//            }
//        }
//        Expr::BinaryExpr(
//            binary @ BinaryExpr {
//                op: BinaryOp::Bsl, ..
//            },
//        ) => {
//            let lhs = eval_const_expr(ctx, b, pre_block, &binary.lhs)?;
//            let rhs = eval_const_expr(ctx, b, pre_block, &binary.rhs)?;
//            match (lhs, rhs) {
//                (AtomicTerm::Int(IntTerm(l)), AtomicTerm::Int(IntTerm(r))) => {
//                    AtomicTerm::BigInt(BigIntTerm(BigInt::from(l) << r.try_into().unwrap()))
//                }
//                val => unimplemented!("{:?}", val),
//            }
//        }
//        _ => unimplemented!("{:?}", expr),
//    };
//    Some(res)
//}

impl Tree {
    pub(crate) fn add_root(
        &mut self,
        ctx: &mut LowerCtx,
        b: &mut FunctionBuilder,
        pre_block: &mut Block,
        expr: &Expr,
    ) {
        let root = pattern_to_tree_node(ctx, b, pre_block, self, expr);
        self.roots.push(root);
    }
}

fn pattern_to_tree_node(
    ctx: &mut LowerCtx,
    b: &mut FunctionBuilder,
    pre_block: &mut Block,
    t: &mut Tree,
    expr: &Expr,
) -> TreeNode {
    match expr {
        Expr::Nil(nil) => {
            let cons = b.cons_mut().from(NilTerm);
            t.nodes.push(TreeNodeKind::Atomic(nil.0, cons))
        }
        Expr::Literal(lit) => {
            let cons = match lit {
                Literal::Atom(_id, ident) => b.cons_mut().from(*ident).into(),
                Literal::Char(_span, _id, c) => b.cons_mut().from(*c).into(),
                Literal::Integer(_span, _id, num) => b.cons_mut().from(num.clone()).into(),
                Literal::Float(_span, _id, num) => b.cons_mut().from(*num).into(),
                Literal::String(_id, ident) => {
                    let mut chars = Vec::new();
                    let ret = crate::lower::strings::tokenize_string(*ident, &mut |cp, _si| {
                        chars.push(cp);
                        Ok(())
                    });
                    match ret {
                        Ok(()) => {
                            let nil_const = b.cons_mut().from(NilTerm);
                            let mut node =
                                t.nodes.push(TreeNodeKind::Atomic(ident.span, nil_const));

                            for c in chars.iter().rev() {
                                let char_const = b.cons_mut().from(*c);
                                let head =
                                    t.nodes.push(TreeNodeKind::Atomic(ident.span, char_const));

                                node = t.nodes.push(TreeNodeKind::Cons {
                                    span: ident.span,
                                    head: head,
                                    tail: node,
                                });
                            }

                            return node;
                        }
                        Err(err) => {
                            ctx.error(err);
                            return t.nodes.push(TreeNodeKind::Wildcard(lit.span()));
                        }
                    }
                }
            };
            t.nodes.push(TreeNodeKind::Atomic(lit.span(), cons))
        }
        Expr::Match(match_expr) => {
            let left = pattern_to_tree_node(ctx, b, pre_block, t, &match_expr.pattern);
            let right = pattern_to_tree_node(ctx, b, pre_block, t, &match_expr.expr);
            t.nodes.push(TreeNodeKind::And { left, right })
        }
        Expr::Var(Var(_id, var)) => {
            let node = t.nodes.push(TreeNodeKind::Wildcard(expr.span()));
            if !crate::lower::scope::is_wildcard(*var) {
                t.binds[node].push(*var);
            }
            node
        }
        Expr::Tuple(tup) => {
            let mut elems = EntityList::new();
            for elem in tup.elements.iter() {
                let node = pattern_to_tree_node(ctx, b, pre_block, t, elem);
                elems.push(node, &mut t.node_pool);
            }
            t.nodes.push(TreeNodeKind::Tuple {
                span: tup.span,
                elems,
            })
        }
        Expr::Cons(cons) => {
            let head = pattern_to_tree_node(ctx, b, pre_block, t, &cons.head);
            let tail = pattern_to_tree_node(ctx, b, pre_block, t, &cons.tail);
            t.nodes.push(TreeNodeKind::Cons {
                span: cons.span,
                head,
                tail,
            })
        }
        Expr::Record(rec) => {
            let span = expr.span();
            if !ctx.module.records.contains_key(&rec.name.name) {
                ctx.error(LowerError::UndefinedRecord {
                    span: rec.name.span,
                });
                return t.nodes.push(TreeNodeKind::Wildcard(span));
            }

            let rec_def = &ctx.module.records[&rec.name.name];

            let name = b.cons_mut().from(rec.name);
            let name_node = t.nodes.push(TreeNodeKind::Atomic(rec.name.span, name));

            let mut pat_fields = vec![];
            for _ in 0..rec_def.record.fields.len() {
                pat_fields.push(t.nodes.push(TreeNodeKind::Wildcard(span)));
            }

            for field in rec.fields.iter() {
                let idx = rec_def.field_idx_map[&field.name];

                let node =
                    pattern_to_tree_node(ctx, b, pre_block, t, &field.value.as_ref().unwrap());

                let prev = pat_fields[idx];
                pat_fields[idx] = t.nodes.push(TreeNodeKind::And {
                    left: prev,
                    right: node,
                });
            }

            let mut elems = EntityList::new();
            elems.push(name_node, &mut t.node_pool);
            for field in pat_fields {
                elems.push(field, &mut t.node_pool);
            }
            t.nodes.push(TreeNodeKind::Tuple {
                span: rec.span,
                elems,
            })
        }
        Expr::BinaryExpr(BinaryExpr {
            op: BinaryOp::Append,
            lhs,
            rhs,
            span,
            ..
        }) => {
            let tail = pattern_to_tree_node(ctx, b, pre_block, t, rhs);
            pattern_to_tree_node_append_tail(ctx, b, pre_block, t, lhs, tail, *span)
        }
        Expr::Binary(Binary { span, elements, .. }) => {
            use crate::lower::expr::binary::{
                default_specifier, specifier_can_have_size, specifier_from_parsed,
                specifier_to_typename, TypeName,
            };

            // Desugar <<"binary string">>
            if elements.len() == 1 {
                let elem = &elements[0];
                if elem.bit_size.is_none()
                    && elem.bit_type.as_ref().map(|v| v.len() == 0).unwrap_or(true)
                {
                    if let Expr::Literal(Literal::String(_id, string)) = &elem.bit_expr {
                        let mut chars = Vec::new();
                        let tokenized =
                            crate::lower::strings::tokenize_string(*string, &mut |cp, _si| {
                                chars.push(cp);
                                Ok(())
                            });
                        return match tokenized {
                            Ok(()) => {
                                let bin =
                                    chars.iter().map(|ch| (ch & 0xff) as u8).collect::<Vec<_>>();
                                let cons = b.cons_mut().from(bin);
                                t.nodes.push(TreeNodeKind::Atomic(*span, cons))
                            }
                            Err(err) => {
                                ctx.error(err);
                                t.nodes.push(TreeNodeKind::Wildcard(*span))
                            }
                        };
                    }
                }
            }

            let mut bin_node = t.nodes.push(TreeNodeKind::Atomic(
                span.clone(),
                b.cons_mut().from(Vec::<u8>::new()),
            ));

            for (_idx, elem) in elements.iter().enumerate().rev() {
                let spec = elem
                    .bit_type
                    .as_ref()
                    .map(|b| specifier_from_parsed(&*b))
                    .unwrap_or(Ok(default_specifier()));

                let spec = match spec {
                    Ok(inner) => inner,
                    Err(err) => {
                        ctx.error(err);
                        continue;
                    }
                };
                let spec_typ = specifier_to_typename(&spec);

                let bit_val = pattern_to_tree_node(ctx, b, pre_block, t, &elem.bit_expr);

                let size_val = if let Some(size_expr) = &elem.bit_size {
                    if !specifier_can_have_size(&spec) {
                        ctx.error(LowerError::BinaryInvalidSize {
                            span: size_expr.span(),
                            typ: spec_typ,
                        });
                        None
                    } else {
                        let ret = match size_expr {
                            Expr::Var(Var(_id, var)) => Either::Left(*var),
                            _ => {
                                let (block, val) = lower_single(ctx, b, *pre_block, size_expr);
                                *pre_block = block;
                                Either::Right(val)
                            }
                        };
                        Some(ret)
                    }
                } else {
                    match spec_typ {
                        TypeName::Integer => Some(b.value(8)),
                        TypeName::Float => Some(b.value(64)),
                        _ => None,
                    }
                    .map(Either::Right)
                };

                bin_node = t.nodes.push(TreeNodeKind::Binary {
                    span: *span,
                    specifier: spec,
                    size: size_val,
                    size_resolved: None,
                    value: bit_val,
                    tail: bin_node,
                });
            }

            bin_node
        }
        Expr::Map(map) => {
            let mut entries = Vec::new();
            for field in map.fields.iter() {
                let (block, key) = lower_single(ctx, b, *pre_block, &field.key());
                *pre_block = block;

                let val = pattern_to_tree_node(ctx, b, pre_block, t, &field.value());

                entries.push((key, val));
            }
            t.nodes.push(TreeNodeKind::Map {
                span: map.span,
                entries,
            })
        }
        expr => {
            let resolve_rec_idx = |name: Ident, field: Ident| {
                let rec = ctx
                    .module
                    .records
                    .get(&name.name)
                    .ok_or(ResolveRecordIndexError::NoRecord)?;
                let idx = rec
                    .field_idx_map
                    .get(&field)
                    .ok_or(ResolveRecordIndexError::NoField)?;
                Ok(*idx)
            };
            match eval_expr(expr, Some(&resolve_rec_idx)) {
                Ok(term) => {
                    let constant = match term {
                        Term::Number(num) => b.cons_mut().from(num),
                        _ => unreachable!(),
                    };
                    t.nodes.push(TreeNodeKind::Atomic(expr.span(), constant))
                }
                Err(error) => {
                    ctx.error(LowerError::PatternConst {
                        source: error,
                        span: expr.span(),
                    });

                    // In this path compilation has failed.
                    // Add a wildcard to don't affect other parts of the compilation.
                    t.nodes.push(TreeNodeKind::Wildcard(expr.span()))
                }
            }
        }
    }
}
