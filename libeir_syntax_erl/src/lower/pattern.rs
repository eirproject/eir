use std::collections::HashMap;

use libeir_ir::{
    FunctionBuilder,
    Value as IrValue,
    Block as IrBlock,
    BinOp as IrBinOp,
};
use libeir_ir::pattern::{
    PatternClause,
    PatternNode,
    PatternMergeFail,
};

use crate::parser::ast::{ Module, ResolvedFunctionName, NamedFunction, Expr, Guard };
use crate::parser::ast::{ Literal, BinaryExpr, BinaryOp, UnaryExpr, UnaryOp };

use super::{ LowerCtx, lower_block, ScopeToken };
use super::errors::LowerError;

use cranelift_entity::{ ListPool, EntityList };

use libeir_intern::{ Ident, Symbol };

enum EqGuard {
    EqValue(usize, IrValue),
    EqBind(usize, usize),
}

struct ClauseLowerCtx {

    // The clause we are constructing
    pat_clause: PatternClause,

    /// Patterns can contain a (limited) amount of expressions.
    /// We construct these values before the case structure starts.
    /// This contains the current last block in the control flow
    /// chain of constructed values.
    pre_case: IrBlock,

    node_renames: HashMap<PatternNode, PatternNode>,

    /// When values are bound when lowering patterns, they are added
    /// here in the same order as they are referenced in the pattern.
    binds: Vec<Option<Ident>>,
    /// Used for checking binds in pattern
    binds_map:HashMap<Ident, usize>,

    /// Corresponds to PatternValues in the clause
    values: Vec<IrValue>,

    // Auxillary equality guards
    // The first value represents the position in the bind list
    eq_guards: Vec<EqGuard>,

}

pub struct LoweredClause {
    pub clause: PatternClause,
    pub body: IrBlock,
    pub guard: IrValue,
    pub scope_token: ScopeToken,
}

/// When this returns Some:
/// * A scope will be pushed with the bound variables in the body block
/// * The body is empty
pub(super) fn lower_clause<'a, P>(
    ctx: &mut LowerCtx, b: &mut FunctionBuilder, pre_case: &mut IrBlock,
    patterns: P, guard: Option<&Vec<Guard>>,
) -> Option<LoweredClause> where P: Iterator<Item = &'a Expr>
{

    let pat_clause = b.pat_mut().clause_start();

    let mut fail = false;

    let mut clause_ctx = ClauseLowerCtx {
        pat_clause,
        pre_case: *pre_case,
        binds: Vec::new(),
        binds_map: HashMap::new(),
        values: Vec::new(),
        eq_guards: Vec::new(),
        node_renames: HashMap::new(),
    };

    // Construct patterns
    for pattern in patterns {
        let pat_node = match lower_pattern(ctx, b, &mut clause_ctx, pattern) {
            Ok(clause) => clause,
            Err(PatternMergeFail::Disjoint { left, right }) => {
                // In this case we only push a warning and fail the lowering of
                // the clause. This will cause the clause to not be added to the
                // case, and compilation can still succeed.
                ctx.errors.push(LowerError::DisjointPatternUnionWarning {
                    left: b.pat().node_span(left),
                    right: b.pat().node_span(right),
                });
                fail = true;
                continue;
            }
            Err(PatternMergeFail::Failure { left, right }) => {
                ctx.errors.push(LowerError::UnsupportedPatternUnion {
                    left: b.pat().node_span(left),
                    right: b.pat().node_span(right),
                });
                ctx.failed = true;
                fail = true;
                continue;
            }
        };
        b.pat_mut().clause_node_push(pat_clause, pat_node);
    }

    // Since we are merging nodes, we might have binds that no longer exist in the actual pattern.
    // Update these according to the rename map.
    // TODO TODO TODO FIXME: Multilevel
    b.pat_mut().update_binds(pat_clause, &clause_ctx.node_renames);

    // Construct guard lambda
    let guard_lambda_block = {

        let guard_lambda_block = b.block_insert();

        let ret_cont = b.block_arg_insert(guard_lambda_block);

        let scope_tok = ctx.scope.push();
        {
            let fail_handler_block = b.block_insert();
            b.block_arg_insert(fail_handler_block);
            let false_val = b.value(false);
            b.op_call(fail_handler_block, ret_cont, &[false_val]);
            ctx.exc_stack.push_handler(b.value(fail_handler_block));
        }

        // Binds
        for bind in clause_ctx.binds.iter() {
            let val = b.block_arg_insert(guard_lambda_block);
            if let Some(name) = bind {
                ctx.bind(*name, val);
            }
        }

        // Body
        let mut block = guard_lambda_block;

        let (cond_block, cond_block_val) = b.block_insert_get_val();
        let cond_res = b.block_arg_insert(cond_block);

        let mut top_and = b.op_intrinsic_build(Symbol::intern("bool_and"));
        top_and.push_value(cond_block_val, b);

        // Aux guards
        for eq_guard in clause_ctx.eq_guards.iter() {
            let (next_block, next_block_val) = b.block_insert_get_val();
            let res_val = b.block_arg_insert(next_block);

            let (lhs, rhs) = match eq_guard {
                EqGuard::EqValue(lhs_idx, rhs) => {
                    let lhs = b.block_args(guard_lambda_block)[lhs_idx + 1];
                    (lhs, *rhs)
                }
                EqGuard::EqBind(lhs_idx, rhs_idx) => {
                    let lhs = b.block_args(guard_lambda_block)[lhs_idx + 1];
                    let rhs = b.block_args(guard_lambda_block)[rhs_idx + 1];
                    (lhs, rhs)
                }
            };

            b.op_intrinsic(block, Symbol::intern("eq"), &[
                next_block_val,
                lhs,
                rhs,
            ]);
            block = next_block;

            top_and.push_value(res_val, b);
        }

        // Clause guards
        if let Some(guard_seq) = guard {
            let (or_block, or_block_val) = b.block_insert_get_val();
            top_and.push_value(b.block_arg_insert(or_block), b);

            let mut or = b.op_intrinsic_build(Symbol::intern("bool_or"));
            or.push_value(or_block_val, b);

            for guard in guard_seq {
                let (and_block, and_block_val) = b.block_insert_get_val();
                or.push_value(b.block_arg_insert(and_block), b);

                let mut and = b.op_intrinsic_build(Symbol::intern("bool_and"));
                and.push_value(and_block_val, b);

                for condition in guard.conditions.iter() {
                    let (block_new, val) = lower_block(
                        ctx, b, block, [condition].iter().map(|v| *v));
                    and.push_value(val, b);
                    block = block_new;
                }

                and.block = Some(block);
                and.finish(b);
                block = and_block;
            }

            or.block = Some(block);
            or.finish(b);
            block = or_block;
        }

        top_and.block = Some(block);
        top_and.finish(b);
        block = cond_block;

        b.op_call(block, ret_cont, &[cond_res]);

        ctx.exc_stack.pop_handler();
        ctx.scope.pop(scope_tok);

        guard_lambda_block
    };

    *pre_case = clause_ctx.pre_case;

    if fail {
        None
    } else {

        // Construct body
        let scope_token = ctx.scope.push();

        // Binds
        let body_block = b.block_insert();

        for bind in clause_ctx.binds.iter() {
            let val = b.block_arg_insert(body_block);
            if let Some(name) = bind {
                ctx.bind(*name, val);
            }
        }

        Some(LoweredClause {
            clause: pat_clause,
            body: body_block,
            guard: b.value(guard_lambda_block),
            scope_token
        })
    }
}

fn lower_pattern(
    ctx: &mut LowerCtx,
    b: &mut FunctionBuilder,
    cl_ctx: &mut ClauseLowerCtx,
    pattern: &Expr,
) -> Result<PatternNode, PatternMergeFail>
{

    let node = match pattern {
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
            b.pat_mut().merge(&mut cl_ctx.node_renames, p1, p2)?
        }
        Expr::Var(var) => {
            let node = b.pat_mut().wildcard();

            // Bind the node to a new variable
            b.pat_mut().clause_bind_push(cl_ctx.pat_clause, node);
            let new_bind_idx = cl_ctx.binds.len();

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
                    // If it is not already bound, we bind it in the scope.
                    cl_ctx.binds.push(Some(*var));
                    cl_ctx.binds_map.insert(*var, new_bind_idx);
                }
            }

            node
        }
        Expr::Tuple(tup) => {
            let node = b.pat_mut().tuple();
            for elem in tup.elements.iter() {
                let elem_node = lower_pattern(ctx, b, cl_ctx, elem)?;
                b.pat_mut().tuple_elem_push(node, elem_node);
            }
            b.pat_mut().node_finish(node);
            node
        }
        Expr::Record(rec) => {
            let rec_def = &ctx.module.records[&rec.name.name];
            // TODO: Proper error handling

            let mut field_idxs = HashMap::new();
            for (idx, field) in rec_def.fields.iter().enumerate() {
                assert!(!field_idxs.contains_key(&field.name.symbol()));
                field_idxs.insert(field.name.symbol(), idx);
            }

            let mut pat_fields = vec![None; rec_def.fields.len()];
            for field in rec.fields.iter() {
                let idx = field_idxs[&field.name.symbol()];
                let pat = lower_pattern(ctx, b, cl_ctx, field.value.as_ref().unwrap())?;
                assert!(pat_fields[idx].is_none());
                pat_fields[idx] = Some(pat);
            }

            let node = b.pat_mut().tuple();

            let name_const = b.cons_mut().from(rec.name);
            let name_node = b.pat_mut().atomic(name_const);
            b.pat_mut().tuple_elem_push(node, name_node);

            for field in pat_fields.iter() {
                if let Some(child_node) = field {
                    b.pat_mut().tuple_elem_push(node, *child_node);
                } else {
                    let wc = b.pat_mut().wildcard();
                    b.pat_mut().tuple_elem_push(node, wc);
                }
            }

            b.pat_mut().node_finish(node);

            node
        }
        _ => {
            // We don't bother trying to evaluate the expressions here, we
            // instead lower to expressions and rely on later compiler passes to
            // make sure the constant expressions get optimized down to
            // constants.

            // Create wildcard node and bind to value
            let node = b.pat_mut().wildcard();
            b.pat_mut().clause_bind_push(cl_ctx.pat_clause, node);
            let bind_num = cl_ctx.binds.len();
            cl_ctx.binds.push(None);

            // Insert pre var
            let (pre_cont, pre_var) = lower_block(ctx, b, cl_ctx.pre_case, [pattern].iter().map(|v| *v));
            cl_ctx.pre_case = pre_cont;

            // Eq guard
            cl_ctx.eq_guards.push(EqGuard::EqValue(bind_num, pre_var));

            // Although we support all expressions in patterns internally, only
            // a subset is actually supported by the erlang language. Validate
            // that the expression is actually part of that subset. This will
            // update the fail state in the context if it fails.
            check_valid_pattern_expr(ctx, pattern);

            node
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
        Expr::Literal(Literal::Integer(_, _)) => true,
        Expr::Literal(Literal::BigInteger(_, _)) => true,
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
        _ => false,
    };

    if !valid {
        ctx.errors.push(LowerError::NotAllowedInPattern {
            span: expr.span(),
        });
        ctx.failed = true;
    }
}
