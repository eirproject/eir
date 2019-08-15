use std::collections::{ HashMap };

use libeir_ir::{
    FunctionBuilder,
    Value as IrValue,
    Block as IrBlock,
};
use libeir_ir::pattern::{
    PatternClause,
    PatternNode,
    PatternValue,
    PatternMergeFail,
};


use crate::parser::ast::{ Expr, Guard };


use super::{ LowerCtx, lower_block, ScopeToken };


use libeir_intern::{ Ident, Symbol };

//mod prewalk;
//mod lower;
//mod collect_binds;

mod tree;
use tree::Tree;

//use prewalk::{ prewalk_pattern, PrewalkFail };
//use lower::{ lower_pattern, to_node, PatternRes, LowerFail };

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
    pub fn clause_value(&mut self, b: &mut FunctionBuilder, val: IrValue
    ) -> PatternValue {
        if let Some(pat_val) = self.value_dedup.get(&val) {
            *pat_val
        } else {
            self.values.push(val);
            b.pat_mut().clause_value(self.pat_clause)
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
    pub fn make_body(&self, ctx: &mut LowerCtx, b: &mut FunctionBuilder
    ) -> (ScopeToken, IrBlock) {
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
    pub fn make_body(&self, ctx: &mut LowerCtx, b: &mut FunctionBuilder
    ) -> (ScopeToken, IrBlock) {
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
    ctx: &mut LowerCtx, b: &mut FunctionBuilder, pre_case: &mut IrBlock,
    shadow: bool, patterns: P, guard: Option<&Vec<Guard>>,
) -> Result<LoweredClause, UnreachableClause>
where
    P: Iterator<Item = &'a Expr>,
{
    assert!(b.fun().block_kind(*pre_case).is_none());

    let pat_clause = b.pat_mut().clause_start();

    let mut clause_ctx = ClauseLowerCtx {
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

    tree.lower(b, &mut clause_ctx);

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

    fn lower_guard(&self, ctx: &mut LowerCtx, b: &mut FunctionBuilder, shadow: bool, guard: Option<&Vec<Guard>>) -> IrBlock {
        let guard_lambda_block = b.block_insert();

        let ret_cont = b.block_arg_insert(guard_lambda_block);
        let _throw_cont = b.block_arg_insert(guard_lambda_block);

        let scope_tok = ctx.scope.push();
        {
            let fail_handler_block = b.block_insert();
            b.block_arg_insert(fail_handler_block);
            b.block_arg_insert(fail_handler_block);
            b.block_arg_insert(fail_handler_block);
            let false_val = b.value(false);
            b.op_call(fail_handler_block, ret_cont, &[false_val]);
            ctx.exc_stack.push_handler(b.value(fail_handler_block));
        }

        // Binds
        for bind in self.binds.iter() {
            let val = b.block_arg_insert(guard_lambda_block);
            if let Some(name) = bind {
                if shadow {
                    ctx.scope.bind_shadow(*name, val);
                } else {
                    ctx.bind(*name, val);
                }
            }
        }

        b.block_annotate_is_fun(guard_lambda_block);

        // Body
        let mut block = guard_lambda_block;

        let (cond_block, cond_block_val) = b.block_insert_get_val();
        let cond_res = b.block_arg_insert(cond_block);

        let mut top_and = b.op_intrinsic_build(Symbol::intern("bool_and"));
        top_and.push_value(cond_block_val, b);

        let erlang_atom = b.value(Ident::from_str("erlang"));
        let exact_eq_atom = b.value(Ident::from_str("=:="));
        let two_atom = b.value(2);

        // Aux guards
        for eq_guard in self.eq_guards.iter() {
            let (next_block, next_block_val) = b.block_insert_get_val();
            let res_val = b.block_arg_insert(next_block);

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

            block = b.op_capture_function(block, erlang_atom, exact_eq_atom, two_atom);
            let fun_val = b.block_args(block)[0];

            let (unreachable_err, unreachable_err_val) = b.block_insert_get_val();
            b.block_arg_insert(unreachable_err);
            b.block_arg_insert(unreachable_err);
            b.block_arg_insert(unreachable_err);
            b.op_unreachable(unreachable_err);

            b.op_call(block, fun_val, &[next_block_val, unreachable_err_val, lhs, rhs]);
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
    }

}
