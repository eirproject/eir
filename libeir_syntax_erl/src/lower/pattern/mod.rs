use std::collections::HashMap;

use libeir_diagnostics::SourceSpan;
use libeir_ir::pattern::{PatternClause, PatternContainer, PatternValue};
use libeir_ir::{Block as IrBlock, FunctionBuilder, Location, LogicOp, Value as IrValue};

use crate::parser::ast::{Expr, Guard};

use super::{lower_block, LowerCtx, ScopeToken};

use libeir_intern::Ident;

mod tree;
use tree::Tree;

//use prewalk::{ prewalk_pattern, PrewalkFail };
//use lower::{ lower_pattern, to_node, PatternRes, LowerFail };

enum EqGuard {
    EqValue(usize, IrValue),
    EqBind(usize, usize),
}

struct ClauseLowerCtx {
    span: SourceSpan,
    loc: Location,

    // The clause we are constructing
    pat_clause: PatternClause,

    /// Patterns can contain a (limited) amount of expressions.
    /// We construct these values before the case structure starts.
    /// This contains the current last block in the control flow
    /// chain of constructed values.
    pre_case: IrBlock,

    /// When values are bound when lowering patterns, they are added
    /// here in the same order as they are referenced in the pattern.
    binds: Vec<Option<Ident>>,

    /// Corresponds to PatternValues in the clause
    values: Vec<IrValue>,

    // Auxillary equality guards
    // The first value represents the position in the bind list
    eq_guards: Vec<EqGuard>,

    value_dedup: HashMap<IrValue, PatternValue>,
}

impl ClauseLowerCtx {
    pub fn clause_value(&mut self, pat: &mut PatternContainer, val: IrValue) -> PatternValue {
        if let Some(pat_val) = self.value_dedup.get(&val) {
            *pat_val
        } else {
            self.values.push(val);
            pat.clause_value(self.pat_clause)
        }
    }
}

pub(crate) struct LoweredClause {
    pub clause: PatternClause,
    pub guard: IrValue,
    pub values: Vec<IrValue>,

    pub shadow: bool,
    pub binds: Vec<Option<Ident>>,
}
impl LoweredClause {
    pub fn make_body(&self, ctx: &mut LowerCtx, b: &mut FunctionBuilder) -> (ScopeToken, IrBlock) {
        let scope_token = ctx.scope.push();
        let body_block = b.block_insert();

        for bind in self.binds.iter() {
            let val = b.block_arg_insert(body_block);
            if let Some(name) = bind {
                if self.shadow {
                    ctx.bind_shadow(*name, val);
                } else {
                    ctx.bind(*name, val);
                }
            }
        }

        (scope_token, body_block)
    }
}

pub(crate) struct UnreachableClause {
    pub shadow: bool,
    pub binds: Vec<Ident>,
}
impl UnreachableClause {
    pub fn make_body(&self, ctx: &mut LowerCtx, b: &mut FunctionBuilder) -> (ScopeToken, IrBlock) {
        let scope_token = ctx.scope.push();
        let body_block = b.block_insert();

        let sentinel = ctx.sentinel();
        for bind in self.binds.iter() {
            if self.shadow {
                ctx.bind_shadow(*bind, sentinel);
            } else {
                ctx.bind(*bind, sentinel);
            }
        }

        (scope_token, body_block)
    }
}

/// When this returns Some:
/// * A scope will be pushed with the bound variables in the body block
/// * The body is empty
pub(super) fn lower_clause<'a, P>(
    ctx: &mut LowerCtx,
    pat: &mut PatternContainer,
    b: &mut FunctionBuilder,
    // Once all clauses have been lowered, this is the block where the case
    // operation itself will be lowered into. The lowering logic for a clause
    // can use this to prepend anything it needs done before the pattern
    // starts, like creating needed values.
    // This block should always be empty.
    pre_case: &mut IrBlock,
    shadow: bool,
    span: SourceSpan,
    patterns: P,
    guard: Option<&Vec<Guard>>,
) -> Result<LoweredClause, UnreachableClause>
where
    P: Iterator<Item = &'a Expr>,
{
    assert!(b.fun().block_kind(*pre_case).is_none());
    let loc = ctx.current_location(b, span);

    let pat_clause = pat.clause_start(span);

    let mut clause_ctx = ClauseLowerCtx {
        span,
        loc,

        pat_clause,
        pre_case: *pre_case,

        binds: Vec::new(),

        values: Vec::new(),
        eq_guards: Vec::new(),

        value_dedup: HashMap::new(),
    };

    let mut tree = Tree::new();
    for pattern in patterns {
        tree.add_root(ctx, b, &mut clause_ctx.pre_case, pattern);
    }
    tree.process(ctx, b, shadow);

    if tree.unmatchable {
        return Err(UnreachableClause {
            shadow,
            binds: tree.pseudo_binds(),
        });
    }

    tree.lower(b, pat, &mut clause_ctx);

    // Construct guard lambda
    let guard_lambda_block = clause_ctx.lower_guard(ctx, b, shadow, guard);

    *pre_case = clause_ctx.pre_case;

    // Construct body
    //let scope_token = ctx.scope.push();

    // Binds
    //let body_block = b.block_insert();

    //for bind in clause_ctx.binds.iter() {
    //    let val = b.block_arg_insert(body_block);
    //    if let Some(name) = bind {
    //        ctx.bind(*name, val);
    //    }
    //}

    Ok(LoweredClause {
        clause: pat_clause,
        guard: b.value(guard_lambda_block),
        values: clause_ctx.values,

        shadow,
        binds: clause_ctx.binds,
    })
}

impl ClauseLowerCtx {
    fn lower_guard(
        &self,
        ctx: &mut LowerCtx,
        b: &mut FunctionBuilder,
        shadow: bool,
        guard: Option<&Vec<Guard>>,
    ) -> IrBlock {
        let guard_lambda_block = b.block_insert();
        b.block_set_location(guard_lambda_block, self.loc);

        let ret_cont = b.block_arg_insert(guard_lambda_block);
        let _throw_cont = b.block_arg_insert(guard_lambda_block);

        let scope_tok = ctx.scope.push();
        {
            let fail_handler_block = b.block_insert();
            b.block_set_location(fail_handler_block, self.loc);
            b.block_arg_insert(fail_handler_block);
            b.block_arg_insert(fail_handler_block);
            b.block_arg_insert(fail_handler_block);
            let false_val = b.value(false);
            b.op_call_flow(fail_handler_block, ret_cont, &[false_val]);
            ctx.exc_stack.push_handler(b.value(fail_handler_block));
        }

        // Binds
        for bind in self.binds.iter() {
            let val = b.block_arg_insert(guard_lambda_block);
            if let Some(name) = bind {
                if shadow {
                    let _ = ctx.scope.bind_shadow(*name, val);
                } else {
                    ctx.bind(*name, val);
                }
            }
        }

        // Body
        let mut block = guard_lambda_block;

        let mut top_and = Vec::new();

        let erlang_atom = b.value(Ident::from_str("erlang"));
        let exact_eq_atom = b.value(Ident::from_str("=:="));
        let two_atom = b.value(2);

        // Aux guards
        for eq_guard in self.eq_guards.iter() {
            let (lhs, rhs) = match eq_guard {
                EqGuard::EqValue(lhs_idx, rhs) => {
                    let lhs = b.block_args(guard_lambda_block)[lhs_idx + 2];
                    (lhs, *rhs)
                }
                EqGuard::EqBind(lhs_idx, rhs_idx) => {
                    let lhs = b.block_args(guard_lambda_block)[lhs_idx + 2];
                    let rhs = b.block_args(guard_lambda_block)[rhs_idx + 2];
                    (lhs, rhs)
                }
            };

            let fun_val = b.prim_capture_function(self.span, erlang_atom, exact_eq_atom, two_atom);

            let (ok_block, err_block) = b.op_call_function(self.span, block, fun_val, &[lhs, rhs]);
            b.block_set_location(block, self.loc);
            let res_val = b.block_args(ok_block)[0];
            block = ok_block;

            b.op_unreachable(self.span, err_block);
            b.block_set_location(err_block, self.loc);

            top_and.push(res_val);
        }

        let mut or = Vec::new();
        let mut and = Vec::new();

        // Clause guards
        if let Some(guard_seq) = guard {
            for guard in guard_seq {
                for condition in guard.conditions.iter() {
                    let (block_new, val) =
                        lower_block(ctx, b, block, [condition].iter().map(|v| *v));

                    and.push(val);
                    block = block_new;
                }

                let val = b.prim_logic_op(guard.span, LogicOp::And, &and);
                and.clear();
                or.push(val);
            }

            let val = b.prim_logic_op(self.span, LogicOp::Or, &or);
            or.clear();
            top_and.push(val);
        }

        let result_bool = b.prim_logic_op(self.span, LogicOp::And, &top_and);
        b.op_call_flow(block, ret_cont, &[result_bool]);
        b.block_set_location(block, self.loc);

        ctx.exc_stack.pop_handler();
        ctx.scope.pop(scope_tok);

        guard_lambda_block
    }
}
