use std::collections::HashMap;

use libeir_ir::{
    FunctionBuilder,
    Value as IrValue,
    Block as IrBlock,
    Const,
};
use libeir_ir::pattern::{
    PatternClause,
    PatternNode,
    PatternValue,
    PatternMergeFail,
};
use libeir_ir::constant::{ NilTerm, BinaryTerm };

use libeir_diagnostics::ByteSpan;
use libeir_intern::{ Ident, Symbol };

use crate::parser::ast::{ Expr, NodeId, Var, Guard };
use crate::parser::ast::{ Literal, BinaryExpr, BinaryOp, UnaryExpr, UnaryOp, Binary };

use crate::lower::{ LowerCtx, lower_block, lower_single, ScopeToken };
use crate::lower::errors::LowerError;

use super::{ ClauseLowerCtx, EqGuard };

use either::Either;

#[derive(Debug, Copy, Clone)]
pub(super) enum PatternRes {
    Node(PatternNode),
    Const(Const),
}
impl From<PatternNode> for PatternRes {
    fn from(n: PatternNode) -> PatternRes {
        PatternRes::Node(n)
    }
}
impl From<Const> for PatternRes {
    fn from(n: Const) -> PatternRes {
        PatternRes::Const(n)
    }
}

pub(super) enum LowerFail {
    PatternMergeFail(PatternMergeFail),
    Disjoint {
        left: ByteSpan,
        right: ByteSpan,
    },
}
impl From<PatternMergeFail> for LowerFail {
    fn from(e: PatternMergeFail) -> Self {
        LowerFail::PatternMergeFail(e)
    }
}

fn normalize(
    b: &mut FunctionBuilder,
    cl_ctx: &mut ClauseLowerCtx,
    elems: &[PatternRes],
) -> Either<Vec<PatternNode>, Vec<Const>>
{
    if elems.iter().all(
        |a| if let PatternRes::Const(_) = a { true } else { false })
    {
        let mut vec = Vec::new();
        for elem in elems.iter() {
            if let PatternRes::Const(cons) = elem {
                vec.push(*cons);
            } else {
                unreachable!();
            }
        }
        Either::Right(vec)
    } else {
        let mut vec = Vec::new();
        for elem in elems.iter() {
            match elem {
                PatternRes::Const(cons) => {
                    let new_node = b.pat_mut().node_empty();
                    b.pat_mut().constant(new_node, *cons);

                    vec.push(new_node);
                }
                PatternRes::Node(node) => {
                    vec.push(*node)
                }
            }
        }
        Either::Left(vec)
    }
}

pub(super) fn to_node(
    b: &mut FunctionBuilder,
    cl_ctx: &mut ClauseLowerCtx,
    elem: PatternRes,
) -> PatternNode {
    match elem {
        PatternRes::Const(cons) => {
            let new_node = b.pat_mut().node_empty();
            b.pat_mut().constant(new_node, cons);

            new_node
        }
        PatternRes::Node(node) => {
            node
        }
    }
}

fn lower_list_head_pattern(
    ctx: &mut LowerCtx,
    b: &mut FunctionBuilder,
    cl_ctx: &mut ClauseLowerCtx,
    pattern: &Expr,
    tail: PatternRes,
) -> Result<PatternRes, LowerFail>
{
    match pattern {
        Expr::Literal(Literal::String(_id, ident)) => {
            let tokens = match crate::lower::expr::literal::tokenize_string(*ident) {
                Ok(tok) => tok,
                Err(err) => {
                    ctx.error(err);
                    let wildcard = b.pat_mut().node_empty();
                    b.pat_mut().wildcard(wildcard);
                    return Ok(wildcard.into());
                },
            };

            match tail {
                PatternRes::Const(tail) => {
                    let mut cons = tail;
                    for c in tokens.iter().rev() {
                        let head = b.cons_mut().from(*c);
                        cons = b.cons_mut().list_cell(head, cons);
                    }
                    Ok(cons.into())
                }
                PatternRes::Node(tail) => {
                    let mut node = tail;
                    for c in tokens.iter().rev() {
                        let char_val = b.value(*c);
                        let char_pat_val = cl_ctx.clause_value(b, char_val);

                        let char_pat = b.pat_mut().node_empty();
                        b.pat_mut().value(char_pat, char_pat_val);

                        let new_node = b.pat_mut().node_empty();
                        b.pat_mut().list(new_node, char_pat, node);
                        node = new_node;
                    }
                    Ok(node.into())
                }
            }
        }
        Expr::Nil(_) => {
            Ok(tail)
        }
        Expr::Cons(cons) => {
            let tail = lower_list_head_pattern(
                ctx, b, cl_ctx, &cons.tail, tail)?;
            let head = lower_pattern(
                ctx, b, cl_ctx, &cons.head)?;

            match (&head, &tail) {
                (PatternRes::Const(c1), PatternRes::Const(c2)) => {
                    Ok(b.cons_mut().list_cell(*c1, *c2).into())
                }
                _ => {
                    let hn = to_node(b, cl_ctx, head);
                    let tn = to_node(b, cl_ctx, tail);

                    let new_node = b.pat_mut().node_empty();
                    b.pat_mut().list(new_node, hn, tn);
                    b.pat_mut().node_set_span(new_node, cons.span);
                    Ok(new_node.into())
                }
            }
        }
        _ => unimplemented!("{:?}", pattern),
    }
}

pub(super) fn lower_pattern(
    ctx: &mut LowerCtx,
    b: &mut FunctionBuilder,
    cl_ctx: &mut ClauseLowerCtx,
    pattern: &Expr,
) -> Result<PatternRes, LowerFail>
{
    //if let Some(cons) = constants.get(&pattern.id()) {
    //    let val = b.value(*cons);
    //    let pat_val = cl_ctx.clause_value(b, val);

    //    let new_node = b.pat_mut().node_empty();
    //    b.pat_mut().value(new_node, pat_val);

    //    return Ok(new_node);
    //}

    let node = match pattern {
        Expr::Nil(_nil) =>
            b.cons_mut().nil().into(),
        Expr::Literal(lit) => match lit {
            Literal::Atom(_id, ident) =>
                b.cons_mut().from(*ident).into(),
            Literal::Char(_span, _id, c) =>
                b.cons_mut().from(*c).into(),
            Literal::Integer(_span, _id, num) =>
                b.cons_mut().from(*num).into(),
            Literal::BigInteger(_span, _id, int) =>
                b.cons_mut().from(int.clone()).into(),
            Literal::Float(_span, _id, num) =>
                b.cons_mut().from(*num).into(),
            Literal::String(_id, ident) => {
                match crate::lower::expr::literal::intern_string_const(
                    *ident, b.cons_mut())
                {
                    Ok(cons) => cons.into(),
                    Err(err) => {
                        ctx.error(err);
                        let node = b.pat_mut().node_empty();
                        b.pat_mut().wildcard(node);
                        node.into()
                    },
                }
            },
        }
        // <pattern> = <expr>
        Expr::Match(match_expr) => {
            // Lower the two sides to patterns and merge them.
            // This will return an error if the two patterns:
            // 1. Are fully disjoint. At this point we know for sure that the
            //    pattern can never succeed, we emit a warning and continue
            //    compilation.
            // 2. We try to merge two unsupported patterns. An example of this
            //    is binary patterns. While we could try harder to merge the
            //    two, it is not supported by the erlang compiler, along with
            //    being generally useless from an end user perspective, so we
            //    don't.
            let p1 = lower_pattern(ctx, b, cl_ctx, &match_expr.pattern)?;
            let p2 = lower_pattern(ctx, b, cl_ctx, &match_expr.expr)?;

            match (p1, p2) {
                (PatternRes::Node(n1), PatternRes::Node(n2)) =>
                    PatternRes::Node(
                        b.merge_patterns(&mut cl_ctx.node_renames, n1, n2)?),
                (PatternRes::Const(c1), PatternRes::Const(c2)) if c1 == c2 =>
                    PatternRes::Const(c1),
                (PatternRes::Const(_), PatternRes::Const(_)) =>
                    Err(LowerFail::Disjoint {
                        left: match_expr.pattern.span(),
                        right: match_expr.expr.span(),
                    })?,
                (PatternRes::Const(cons), PatternRes::Node(node)) =>
                    b.merge_pattern_constant(
                        &mut cl_ctx.node_renames, node, cons)?.into(),
                (PatternRes::Node(node), PatternRes::Const(cons)) =>
                    b.merge_pattern_constant(
                        &mut cl_ctx.node_renames, node, cons)?.into(),
            }
        }
        Expr::Var(Var(_id, var)) => {
            let node = b.pat_mut().node_empty();
            b.pat_mut().wildcard(node);
            b.pat_mut().node_set_span(node, var.span);

            // Bind the node to a new variable
            let new_bind_idx = cl_ctx.binds.len();
            let mut do_add = true;

            if let Ok(prev_bound) = ctx.scope.resolve(*var) {
                // If the variable is already bound in the scope,
                // we need to insert a new guard for equality.
                cl_ctx.binds.push(None);
                cl_ctx.eq_guards.push(EqGuard::EqValue(new_bind_idx, prev_bound));
            } else {
                if let Some(bind_num) = cl_ctx.binds_map.get(var) {
                    // If we encountered the variable previously in the clause,
                    // insert a guard for that.
                    cl_ctx.binds.push(None);
                    cl_ctx.eq_guards.push(EqGuard::EqBind(new_bind_idx, *bind_num));
                } else {
                    // Disregard wildcards
                    if crate::lower::scope::is_wildcard(*var) {
                        do_add = false;
                    } else {
                        // If it is not already bound, we bind it in the scope.
                        cl_ctx.binds.push(Some(*var));
                        cl_ctx.binds_map.insert(*var, new_bind_idx);
                    }
                }
            }

            if do_add {
                b.pat_mut().clause_bind_push(cl_ctx.pat_clause, node);
            }

            node.into()
        }
        Expr::Tuple(tup) => {
            let mut elems = Vec::new();
            for elem in tup.elements.iter() {
                let elem_node = lower_pattern(ctx, b, cl_ctx, elem)?;
                elems.push(elem_node);
            }

            match normalize(b, cl_ctx, &elems) {
                Either::Left(nodes) => {
                    let node = b.pat_mut().node_empty();
                    b.pat_mut().tuple(node);

                    for n_node in nodes {
                        b.pat_mut().tuple_elem_push(node, n_node);
                    }

                    b.pat_mut().node_finish(node);
                    b.pat_mut().node_set_span(node, tup.span);

                    node.into()
                }
                Either::Right(consts) => {
                    let mut tup_b = b.cons_mut().tuple_builder();
                    for cons in consts {
                        tup_b.push(cons, b.cons_mut());
                    }
                    tup_b.finish(b.cons_mut()).into()
                }
            }
        }
        Expr::Cons(cons) => {
            let head = lower_pattern(ctx, b, cl_ctx, &cons.head)?;
            let tail = lower_pattern(ctx, b, cl_ctx, &cons.tail)?;

            match normalize(b, cl_ctx, &[head, tail]) {
                Either::Left(nodes) => {
                    assert!(nodes.len() == 2);

                    let node = b.pat_mut().node_empty();
                    b.pat_mut().list(node, nodes[0], nodes[1]);
                    b.pat_mut().node_set_span(node, pattern.span());
                    node.into()
                }
                Either::Right(consts) => {
                    assert!(consts.len() == 2);

                    b.cons_mut().list_cell(consts[0], consts[1]).into()
                }
            }
        }
        Expr::Record(rec) => {
            let rec_def = &ctx.module.records[&rec.name.name];
            // TODO: Proper error handling

            let num_fields = rec_def.record.fields.len();

            let mut pat_fields = vec![None; num_fields];
            for field in rec.fields.iter() {
                let idx = rec_def.field_idx_map[&field.name];
                let pat = lower_pattern(ctx, b, cl_ctx,
                                        field.value.as_ref().unwrap())?;
                assert!(pat_fields[idx].is_none());
                pat_fields[idx] = Some(pat);
            }

            if pat_fields.iter().all(
                |f| if let Some(PatternRes::Const(_)) = f
                { true } else { false })
            {
                let mut tup_b = b.cons_mut().tuple_builder();

                let name_const = b.cons_mut().from(rec.name);
                tup_b.push(name_const, b.cons_mut());

                for field in pat_fields {
                    if let Some(PatternRes::Const(cons)) = field {
                        tup_b.push(cons, b.cons_mut());
                    } else {
                        unreachable!()
                    }
                }

                tup_b.finish(b.cons_mut()).into()
            } else {
                let node = b.pat_mut().node_empty();
                b.pat_mut().tuple(node);

                let name_val = b.value(rec.name);
                let name_pat_val = cl_ctx.clause_value(b, name_val);
                let name_node = b.pat_mut().node_empty();
                b.pat_mut().value(name_node, name_pat_val);
                b.pat_mut().tuple_elem_push(node, name_node);

                for field in pat_fields.iter() {
                    match field {
                        Some(res) => {
                            let c_node = to_node(b, cl_ctx, *res);
                            b.pat_mut().tuple_elem_push(node, c_node);
                        },
                        None => {
                            let wc = b.pat_mut().node_empty();
                            b.pat_mut().wildcard(wc);
                            b.pat_mut().tuple_elem_push(node, wc);
                        }
                    }
                }

                b.pat_mut().node_set_span(node, rec.span);
                b.pat_mut().node_finish(node);

                node.into()
            }
        }
        Expr::BinaryExpr(BinaryExpr { op: BinaryOp::Append, lhs, rhs, .. }) => {
            let tail = lower_pattern(ctx, b, cl_ctx, rhs)?;
            lower_list_head_pattern(ctx, b, cl_ctx, lhs, tail)?
        }
        Expr::Binary(Binary { span, elements, .. }) => {
            use crate::lower::expr::binary::{ TypeName, specifier_from_parsed, default_specifier,
                                              specifier_to_typename, specifier_can_have_size };

            // Desugar <<"binary string">>
            if elements.len() == 1 {
                let elem = &elements[0];
                if elem.bit_size.is_none()
                    && elem.bit_type.as_ref().map(|v| v.len() == 0).unwrap_or(true)
                {
                    if let Expr::Literal(Literal::String(_id, string)) = &elem.bit_expr {
                        let tokenized = crate::lower::expr::literal::tokenize_string(*string);
                        return match tokenized {
                            Ok(chars) => {
                                let bin = chars.iter()
                                    .map(|ch| (ch & 0xff) as u8)
                                    .collect::<Vec<_>>();
                                let cons = b.cons_mut().from(bin);
                                Ok(PatternRes::Const(cons))
                            }
                            Err(err) => {
                                ctx.error(err);
                                let cons = b.cons_mut().from(vec![]);
                                Ok(PatternRes::Const(cons))
                            }
                        };
                    }
                }
            }

            let mut map: HashMap<Ident, PatternValue> = HashMap::new();

            let empty_bin_const = b.value(Vec::<u8>::new());
            let empty_bin_pat_val = cl_ctx.clause_value(b, empty_bin_const);

            let mut bin_node = b.pat_mut().node_empty();
            b.pat_mut().value(bin_node, empty_bin_pat_val);

            let mut nodes = Vec::new();
            for elem in elements.iter() {
                let res = lower_pattern(ctx, b, cl_ctx, &elem.bit_expr)?;
                let node = to_node(b, cl_ctx, res);
                match elem.bit_expr {
                    Expr::Var(Var(_id, var)) => {
                        let val = b.pat_mut().clause_node_value(
                            cl_ctx.pat_clause, node);
                        map.insert(var, val);
                    }
                    _ => (),
                }
                nodes.push(node);
            }

            for (idx, elem) in elements.iter().enumerate().rev() {
                let spec = elem.bit_type.as_ref()
                    .map(|b| specifier_from_parsed(&*b))
                    .unwrap_or(Ok(default_specifier()));

                let spec = match spec {
                    Ok(inner) => inner,
                    Err(err) => {
                        ctx.error(err);
                        continue;
                    },
                };
                let spec_typ = specifier_to_typename(&spec);

                let size_val = if let Some(size_expr) = &elem.bit_size {
                    if !specifier_can_have_size(&spec) {
                        ctx.error(LowerError::BinaryInvalidSize {
                            span: size_expr.span(),
                            typ: spec_typ,
                        });
                        None
                    } else {
                        let ret = match size_expr {
                            Expr::Var(Var(_id, var)) => {

                                // use that
                                match ctx.try_resolve(*var) {
                                    Ok(value) => cl_ctx.clause_value(b, value),
                                    Err(err) => {
                                        // Also try to resolve from the binary match
                                        if let Some(r) = map.get(var) {
                                            *r
                                        } else {
                                            // Else, error and return dummy value
                                            ctx.error(err);
                                            let nil_val = b.value(NilTerm);
                                            cl_ctx.clause_value(b, nil_val)
                                        }
                                    }
                                }
                            }
                            _ => {
                                let (block, val) = lower_single(ctx, b, cl_ctx.pre_case, size_expr);
                                cl_ctx.pre_case = block;
                                cl_ctx.clause_value(b, val)
                            }
                        };
                        Some(ret)
                    }
                } else {
                    match spec_typ {
                        TypeName::Integer => Some(b.value(8)),
                        TypeName::Float => Some(b.value(64)),
                        _ => None,
                    }.map(|v| cl_ctx.clause_value(b, v))
                };

                let bit_node = nodes[idx];

                let node = b.pat_mut().node_empty();
                b.pat_mut().binary(node, spec, bit_node, size_val, bin_node);
                bin_node = node;
            }

            b.pat_mut().node_finish(bin_node);
            b.pat_mut().node_set_span(bin_node, *span);
            bin_node.into()
        },
        Expr::Map(map) => {
            let node = b.pat_mut().node_empty();
            b.pat_mut().map(node);

            for field in map.fields.iter() {
                let (block, val) = lower_single(
                    ctx, b, cl_ctx.pre_case, &field.key());
                cl_ctx.pre_case = block;
                let key_val = cl_ctx.clause_value(b, val);

                let val_res = lower_pattern(
                    ctx, b, cl_ctx, &field.value())?;
                let val_pat = to_node(b, cl_ctx, val_res);

                b.pat_mut().map_push(node, key_val, val_pat);
            }

            b.pat_mut().node_finish(node);
            b.pat_mut().node_set_span(node, map.span);
            node.into()
        }
        Expr::MapUpdate(_map) => unimplemented!(),
        Expr::MapProjection(_map) => unimplemented!(),
        _ => {
            // We don't bother trying to evaluate the expressions here, we
            // instead lower to expressions and rely on later compiler passes to
            // make sure the constant expressions get optimized down to
            // constants.

            // Create wildcard node and bind to value
            let node = b.pat_mut().node_empty();
            b.pat_mut().wildcard(node);
            b.pat_mut().node_set_span(node, pattern.span());
            b.pat_mut().clause_bind_push(cl_ctx.pat_clause, node);
            let bind_num = cl_ctx.binds.len();
            cl_ctx.binds.push(None);

            // Insert pre var
            let (pre_cont, pre_var) = lower_block(ctx, b, cl_ctx.pre_case,
                                                  [pattern].iter().map(|v| *v));
            cl_ctx.pre_case = pre_cont;

            // Eq guard
            cl_ctx.eq_guards.push(EqGuard::EqValue(bind_num, pre_var));

            // Although we support all expressions in patterns internally, only
            // a subset is actually supported by the erlang language. Validate
            // that the expression is actually part of that subset. This will
            // update the fail state in the context if it fails.
            check_valid_pattern_expr(ctx, pattern);

            node.into()
        }
    };

    Ok(node)
}

fn check_valid_pattern_expr(ctx: &mut LowerCtx, expr: &Expr) {
    // http://erlang.org/doc/reference_manual/expressions.html#expressions-in-patterns
    // > An arithmetic expression can be used within a pattern if it meets both
    // > of the following two conditions:
    // > - It uses only numeric or bitwise operators.
    // > - Its value can be evaluated to a constant when complied.

    let valid = match expr {
        Expr::Literal(Literal::Atom(_, _)) => true,
        Expr::Literal(Literal::Integer(_, _, _)) => true,
        Expr::Literal(Literal::BigInteger(_, _, _)) => true,
        Expr::BinaryExpr(BinaryExpr { op, lhs, rhs, .. }) => {
            let valid = match op {
                // Integer
                BinaryOp::Add => true,
                BinaryOp::Sub => true,
                BinaryOp::Divide => true,
                BinaryOp::Multiply => true,
                BinaryOp::Div => true,
                BinaryOp::Rem => true,

                // Bitwise
                BinaryOp::Band => true,
                BinaryOp::Bor => true,
                BinaryOp::Bxor => true,
                BinaryOp::Bsl => true,
                BinaryOp::Bsr => true,

                // List
                BinaryOp::Append => true,

                _ => false,
            };
            if valid {
                check_valid_pattern_expr(ctx, lhs);
                check_valid_pattern_expr(ctx, rhs);
            }
            valid
        },
        Expr::UnaryExpr(UnaryExpr { op, operand, .. }) => {
            // All unary expressions valid, include match for readability
            let valid = match op {
                UnaryOp::Plus => true,
                UnaryOp::Minus => true,
                UnaryOp::Bnot => true,
                UnaryOp::Not => true,
            };
            if valid {
                check_valid_pattern_expr(ctx, operand);
            }
            valid
        },
        Expr::Nil(_nil) => {
            true
        }
        Expr::Cons(cons) => {
            check_valid_pattern_expr(ctx, &cons.head);
            check_valid_pattern_expr(ctx, &cons.tail);
            true
        },
        _ => false,
    };

    if !valid {
        ctx.errors.push(LowerError::NotAllowedInPattern {
            span: expr.span(),
        });
        ctx.failed = true;
    }
}
