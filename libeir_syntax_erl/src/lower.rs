use std::collections::HashMap;

use libeir_hir::HirModule;
use libeir_hir::{
    Expr as HirExpr,
    Variable as HirVariable,
    Function as HirFunction,
    Clause as HirClause,
    PatternClause,
    PatternNode,
    PatternMergeFail,
};

use libeir_intern::Symbol;

use libeir_util::hashmap_stack::HashMapStack;

use cranelift_entity::{ ListPool, EntityList };

use crate::parser::ast::{ Module, ResolvedFunctionName, NamedFunction, Expr, Guard };
use crate::parser::ast::{ BinaryOp };

mod errors;
use errors::LowerError;

// Actual error info is located in context, this is only applicable when
// we encounter something we don't know how to proceed from and need to
// bail.
type LowerResult<T> = Result<T, ()>;

struct LowerCtx {
    hir: HirModule,

    scope: HashMapStack<Symbol, HirVariable>,

    errors: Vec<LowerError>,

    expr_pool: ListPool<HirExpr>,
    variable_pool: ListPool<HirVariable>,
    clause_pool: ListPool<HirClause>,
}

pub fn lower_module(module: &Module) -> HirModule {

    // TODO sort functions for more deterministic compilation

    let mut ctx = LowerCtx {
        hir: HirModule::new(module.name),

        scope: HashMapStack::new(),

        errors: Vec::new(),

        expr_pool: ListPool::new(),
        variable_pool: ListPool::new(),
        clause_pool: ListPool::new(),
    };

    for (ident, function) in module.functions.iter() {
        assert!(ctx.scope.height() == 0);
        let hir_fun = lower_function(&mut ctx, ident, function);
    }

    ctx.hir
}

fn lower_function(ctx: &mut LowerCtx, ident: &ResolvedFunctionName, function: &NamedFunction) -> HirFunction {

    // Construct function
    let hir_fun = ctx.hir.function_start();
    ctx.hir.function_set_name(hir_fun, Some(ident.function));

    // Construct the HIR variables that represent the function
    // parameters, and construct a value list for pattern matching.
    let params_val_list = ctx.hir.expr_value_list_start();
    for _ in 0..function.arity {
        let param_var = ctx.hir.variable_anon();
        ctx.hir.function_push_arg(hir_fun, param_var);

        let param_expr = ctx.hir.expr_variable(param_var);
        ctx.hir.expr_value_list_push(params_val_list, param_expr);
    }
    ctx.hir.expr_finish(params_val_list);

    // Top level function case expression
    let func_case = ctx.hir.expr_case_start(params_val_list);
    for clause in function.clauses.iter() {
        let hir_clause = match lower_clause(ctx, &clause.params, &clause.guard, &clause.body) {
            Some(clause) => clause,
            None => continue,
        };
        ctx.hir.expr_case_push_clause(func_case, hir_clause);
    }
    ctx.hir.expr_finish(func_case);

    ctx.hir.function_finish(hir_fun);
    hir_fun
}

fn lower_clause(ctx: &mut LowerCtx, patterns: &[Expr],
                guard: &Option<Vec<Guard>>, body: &[Expr]) -> Option<HirClause>
{

    let pat_clause = ctx.hir.pattern_container.clause_start();
    let hir_clause = ctx.hir.clause_start(pat_clause);

    // There can be scope local assignments from a pattern, push a new scope
    ctx.scope.push();

    let mut fail = false;
    let mut node_merge_map = HashMap::new();

    for pattern in patterns.iter() {
        let pat_node = match lower_pattern(ctx, &mut node_merge_map, pat_clause, hir_clause, pattern) {
            Ok(clause) => clause,
            Err(PatternMergeFail::Disjoint { left, right }) => {
                // In this case we only push a warning and fail the lowering of the clause.
                // This will cause the clause to not be added to the case, and compilation
                // can still succeed.
                ctx.errors.push(LowerError::DisjointPatternUnionWarning {
                    left: ctx.hir.pattern_container.node_span(left),
                    right: ctx.hir.pattern_container.node_span(right),
                });
                fail = true;
                continue;
            }
            Err(PatternMergeFail::Failure { left, right }) => {
                ctx.errors.push(LowerError::UnsupportedPatternUnion {
                    left: ctx.hir.pattern_container.node_span(left),
                    right: ctx.hir.pattern_container.node_span(right),
                });
                fail = true;
                continue;
            }
        };
        ctx.hir.pattern_container.clause_node_push(pat_clause, pat_node);
    }

    // TODO apply node merge map to pattern binds

    let body_expr = lower_exprs(ctx, body);

    ctx.scope.pop();
    unimplemented!()
}

fn lower_exprs(ctx: &mut LowerCtx, exprs: &[Expr]) -> HirExpr {
}

fn lower_pattern(ctx: &mut LowerCtx, map: &mut HashMap<PatternNode, PatternNode>,
                 pat_clause: PatternClause, hir_clause: HirClause,
                 pattern: &Expr) -> Result<PatternNode, PatternMergeFail>
{

    let node = match pattern {
        Expr::BinaryExpr(bin_expr) => {
            // Only assignment/equality is allowed in patterns
            match bin_expr.op {
                BinaryOp::Equal => {
                    let p1 = lower_pattern(ctx, map, pat_clause, hir_clause, &bin_expr.lhs)?;
                    let p2 = lower_pattern(ctx, map, pat_clause, hir_clause, &bin_expr.rhs)?;
                    ctx.hir.pattern_container.merge(map, p1, p2)?
                }
                _ => {
                    // Not allowed, push an error.
                    // Return a dummy pattern so that we can go on and potentially find
                    // more errors.
                    ctx.errors.push(LowerError::NotAllowedInPattern { span: bin_expr.span });
                    ctx.hir.pattern_container.wildcard()
                }
            }

        }
        Expr::Var(var) => {

            if let Some(prev_bound) = ctx.scope.get(&var.name) {
                // If the variable is already bound in the scope,
                // we need to insert a new guard for equality.
            } else {
                // If it is not already bound, we bind it.
            }

            unimplemented!()
        }
        _ => unimplemented!(),
    };

    Ok(node)
}








