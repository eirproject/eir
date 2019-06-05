mod resolve_scope;
use self::resolve_scope::{ ResolvedVar, Node as ResNode };

mod exception_handler_stack;
use self::exception_handler_stack::ExceptionHandlerStack;

use crate::hir::{ HirModule, Function as HirFun, Clause, Expr, Variable };

use std::collections::HashMap;

use libeir_ir::{ Atom, Function, FunctionBuilder, Block, Value };
use libeir_ir::{ AtomicTerm };
use libeir_ir::{ TestOperation };
use libeir_ir::FunctionIdent;

use cranelift_entity::{ EntityList, ListPool };

pub fn lower(hir: &HirModule, fun: HirFun) -> libeir_ir::Function {

    let ident = FunctionIdent {
        module: hir.name.clone(),
        name: hir.function_name(fun).clone(),
        arity: hir.function_args(fun).len(),
    };

    let mut resolver = resolve_scope::ScopeResolver::new();
    let map = resolver.resolve(hir, fun);

    let mut eir = Function::new(ident);

    {

        let mut ctx = LowerCtx {
            hir: hir,
            map: &map,
            res: HashMap::new(),
            exc_stack: ExceptionHandlerStack::new(),
            b: FunctionBuilder::new(&mut eir),

            val_buf: Vec::new(),
            block_pool: ListPool::new(),
        };

        let block = lower_fun(&mut ctx, fun);
        ctx.b.fun_mut().block_set_entry(block);
    }

    eir
}

struct LowerCtx<'a> {
    hir: &'a HirModule,
    map: &'a resolve_scope::ScopeMap,
    res: HashMap<ResolvedVar, Value>,
    exc_stack: ExceptionHandlerStack,
    b: FunctionBuilder<'a>,

    val_buf: Vec<Value>,
    block_pool: ListPool<Block>,
}

impl<'a> LowerCtx<'a> {

    fn get_res(&self, node: ResNode) -> Value {
        let res = self.map.get(node).unwrap();
        self.res[&res]
    }

    fn put_res(&mut self, node: ResNode, val: Value) {
        let res = self.map.get(node).unwrap();
        assert!(self.res.get(&res).map(|v| *v == val).unwrap_or(true));
        self.res.insert(res, val);
    }

}

fn lower_fun<'a>(ctx: &mut LowerCtx<'a>, fun: HirFun) -> Block {
    let block = ctx.b.block_insert();

    // Ok continuation
    let ok_cont = ctx.b.block_arg_insert(block);

    // Error continuation
    let err_cont = ctx.b.block_arg_insert(block);
    ctx.exc_stack.push_handler(err_cont);

    // Arguments
    for hir_arg in ctx.hir.function_args(fun) {
        let arg = ctx.b.block_arg_insert(block);
        ctx.put_res(ResNode::Variable(*hir_arg), arg);
    }

    // Binding
    let fun_block_val = ctx.b.value_block(block);
    ctx.put_res(ResNode::Function(fun), fun_block_val);

    // Body
    let expr = ctx.hir.function_expr(fun);
    let ret_block = lower_expr(ctx, block, expr);

    // Return call
    let ret_arg = ctx.b.block_args(block)[0];
    ctx.b.op_call(ret_block, ok_cont, &[ret_arg]);

    block
}

fn unpack_value_list_variables<'a>(ctx: &mut LowerCtx<'a>, block: Block, value: Value,
                                   vars: &EntityList<Variable>) -> Block {
    let mut block = block;

    // Unpack value list OP
    let val_list_len = vars.len(&ctx.hir.variable_pool);
    block = ctx.b.op_unpack_value_list(block, value, val_list_len);

    // Insert newly bound
    for (idx, assign) in vars.as_slice(&ctx.hir.variable_pool).iter().enumerate() {
        let val = ctx.b.block_args(block)[idx];
        ctx.put_res(ResNode::Variable(*assign), val);
    }

    block
}

fn lower_match<'a>(ctx: &mut LowerCtx<'a>, block: Block, match_on: Value,
                   clauses: &EntityList<Clause>, no_match: Value,
                   body_hook: &Fn(&mut LowerCtx<'a>, Block) -> Block) -> (Block, Value)
{
    let mut block = block;

    let join_block = ctx.b.block_insert();
    let join_block_val = ctx.b.value_block(join_block);
    let join_arg = ctx.b.block_arg_insert(join_block);

    // TODO Validate execution order is correct according to erlang semantics

    // Lower all pattern values
    for clause in clauses.as_slice(&ctx.hir.clause_pool) {
        for val_expr in ctx.hir.clause_values(*clause) {
            block = lower_expr(ctx, block, *val_expr);
        }
    }

    let mut guards_buf = EntityList::<Block>::new();
    let mut bodies_buf = EntityList::<Block>::new();

    // Lower guards and bodies
    for clause in clauses.as_slice(&ctx.hir.clause_pool) {
        let pat_clause = ctx.hir.clause_pattern_clause(*clause);

        // Guard
        // Each guard lowers into a lambda with two continuations
        // as its first two arguments, a guard ok continuation and
        // a guard fail continuation.
        // The rest of the arguments are the pattern binds of the
        // clause.
        {
            let guard_expr = ctx.hir.clause_guard_expr(*clause);

            let guard_lambda = ctx.b.block_insert();

            let guard_ok_cont = ctx.b.block_arg_insert(guard_lambda);
            let guard_fail_cont = ctx.b.block_arg_insert(guard_lambda);

            // Bind clause binds to lambda arguments
            let clause_binds_len = ctx.b.fun().pattern_container().clause_binds(pat_clause).len();
            for idx in 0..clause_binds_len {
                let node = ctx.b.fun().pattern_container().clause_binds(pat_clause)[idx];
                let var = ctx.b.block_arg_insert(guard_lambda);
                ctx.put_res(ResNode::PatternBind(node), var);
            }

            // Lower actual guard
            ctx.exc_stack.push_anon_handler(guard_fail_cont);
            let ret_block = lower_expr(ctx, guard_lambda, guard_expr);
            ctx.exc_stack.pop_handler();

            // Return value truthiness test
            let ret_val = ctx.get_res(ResNode::Expr(guard_expr));
            let (ok, err) = ctx.b.op_test(ret_block, TestOperation::IsTruthy, ret_val);
            ctx.b.op_call(ok, guard_ok_cont, &[]);
            ctx.b.op_call(err, guard_fail_cont, &[]);

            guards_buf.push(guard_lambda, &mut ctx.block_pool);
        }

        // Body
        {
            let body_expr = ctx.hir.clause_body_expr(*clause);

            let body_block = ctx.b.block_insert();
            let mut block = body_block;

            block = body_hook(ctx, block);

            let clause_binds_len = ctx.b.fun().pattern_container().clause_binds(pat_clause).len();
            for idx in 0..clause_binds_len {
                let node = ctx.b.fun().pattern_container().clause_binds(pat_clause)[idx];
                let var = ctx.b.block_arg_insert(body_block);
                ctx.put_res(ResNode::PatternBind(node), var);
            }

            // Lower body
            block = lower_expr(ctx, block, body_expr);

            // Call to join block
            let ret_val = ctx.get_res(ResNode::Expr(body_expr));
            ctx.b.op_call(block, join_block_val, &[ret_val]);

            bodies_buf.push(body_block, &mut ctx.block_pool);
        }

    }

    // Construct match operation
    {
        ctx.b.op_start_case(block, no_match, match_on);

        for (idx, clause) in clauses.as_slice(&ctx.hir.clause_pool).iter().enumerate() {
            let guard = guards_buf.get(idx, &ctx.block_pool).unwrap();
            let guard_value = ctx.b.value_block(guard);
            let body = bodies_buf.get(idx, &ctx.block_pool).unwrap();
            let body_value = ctx.b.value_block(body);

            // Values
            for val_expr in ctx.hir.clause_values(*clause) {
                let val = ctx.get_res(ResNode::Expr(*val_expr));
                ctx.b.case_value(val);
            }

            // Copy clause to the new pattern container
            let pat_clause = ctx.hir.clause_pattern_clause(*clause);
            let clause_copied = ctx.b.fun_mut().pattern_container_mut()
                .copy_from(pat_clause, &ctx.hir.pattern_container);

            ctx.b.case_clause(clause_copied, guard_value, body_value);

        }

        ctx.b.case_finish();
    }

    guards_buf.clear(&mut ctx.block_pool);
    bodies_buf.clear(&mut ctx.block_pool);

    (join_block, join_arg)
}

fn lower_expr<'a>(ctx: &mut LowerCtx<'a>, block: Block, expr: Expr) -> Block {
    use crate::hir::ExprKind as EK;
    match ctx.hir.expr_kind(expr) {
        EK::Variable(var) => {
            // This should already be bound, assert that
            let res = ctx.map.get(ResNode::Variable(*var)).unwrap();
            assert!(ctx.res.contains_key(&res));

            block
        }
        EK::Function(fun) => {
            let mut block = block;
            let res = ctx.map.get(ResNode::FunctionRef(*fun)).unwrap();
            if let Some(fun) = ctx.map.is_ext_fun(res) {

                let module_atom = ctx.hir.function_ref_module(fun).cloned()
                    .unwrap_or_else(|| ctx.hir.name.clone());
                let module = ctx.b.value_atomic(AtomicTerm::Atom(module_atom));

                let name = ctx.b.value_atomic(AtomicTerm::Atom(
                    ctx.hir.function_ref_name(fun).clone()));

                let arity = ctx.b.value_atomic(AtomicTerm::Integer(
                    ctx.hir.function_ref_arity(fun).into()));

                block = ctx.b.op_capture_function(block, module, name, arity);

                let fun_val = ctx.b.block_args(block)[0];
                ctx.put_res(ResNode::Expr(expr), fun_val);

                block
            } else {
                assert!(ctx.res.contains_key(&res));
                block
            }
        }
        EK::LetRec { closures, body } => {
            for fun in closures.as_slice(&ctx.hir.function_pool) {
                lower_fun(ctx, *fun);
            }
            lower_expr(ctx, block, *body)
        }
        EK::Atomic(atomic) => {
            let val = ctx.b.value_atomic(atomic.clone());
            ctx.put_res(ResNode::Expr(expr), val);

            block
        }
        EK::Tuple { elems } => {
            let mut block = block;
            for expr in elems.as_slice(&ctx.hir.expr_pool) {
                block = lower_expr(ctx, block, *expr);
            }

            ctx.val_buf.clear();
            for elem in elems.as_slice(&ctx.hir.expr_pool) {
                let val = ctx.get_res(ResNode::Expr(*elem));
                ctx.val_buf.push(val);
            }

            block = ctx.b.op_make_tuple(block, &ctx.val_buf);
            let tup_val = ctx.b.block_args(block)[0];
            ctx.put_res(ResNode::Expr(expr), tup_val);

            block
        }
        /// TODO: List, Map, Binary
        EK::ValueList { elems } => {
            let mut block = block;
            for expr in elems.as_slice(&ctx.hir.expr_pool) {
                block = lower_expr(ctx, block, *expr);
            }

            ctx.val_buf.clear();
            for elem in elems.as_slice(&ctx.hir.expr_pool) {
                let val = ctx.get_res(ResNode::Expr(*elem));
                ctx.val_buf.push(val);
            }

            block = ctx.b.op_pack_value_list(block, &ctx.val_buf);
            let vallist_val = ctx.b.block_args(block)[0];
            ctx.put_res(ResNode::Expr(expr), vallist_val);

            block
        }
        /// TODO: PrimOp
        EK::Apply { fun, args } => {
            let mut block = block;

            // Lower subexpressions
            block = lower_expr(ctx, block, *fun);
            for expr in args.as_slice(&ctx.hir.expr_pool) {
                block = lower_expr(ctx, block, *expr);
            }

            // Ok continuation
            let ok_block = ctx.b.block_insert();
            let ok_arg = ctx.b.block_arg_insert(ok_block);
            ctx.put_res(ResNode::Expr(expr), ok_arg);

            // Err continuation
            let err_block = ctx.b.block_insert();
            let err_arg = ctx.b.block_arg_insert(err_block);
            ctx.exc_stack.make_error_jump(&mut ctx.b, err_block, err_arg);

            // Function value
            let fun_val = ctx.get_res(ResNode::Expr(*fun));

            // Arguments
            ctx.val_buf.clear();

            ctx.val_buf.push(ctx.b.value_block(ok_block));
            ctx.val_buf.push(ctx.b.value_block(err_block));

            for elem in args.as_slice(&ctx.hir.expr_pool) {
                let val = ctx.get_res(ResNode::Expr(*elem));
                ctx.val_buf.push(val);
            }

            // Actual call
            ctx.b.op_call(block, fun_val, &ctx.val_buf);
            ctx.put_res(ResNode::Expr(expr), ok_arg);

            ok_block
        }
        EK::Let { assigns, value, body } => {
            let mut block = block;

            block = lower_expr(ctx, block, *value);

            // Unpack value list OP
            let value_val = ctx.get_res(ResNode::Expr(expr));
            block = unpack_value_list_variables(ctx, block, value_val, assigns);

            block = lower_expr(ctx, block, *body);

            block
        }
        EK::Do { bodies } => {
            let mut block = block;

            for expr in bodies.as_slice(&ctx.hir.expr_pool) {
                block = lower_expr(ctx, block, *expr);
            }

            block
        }
        EK::Try { body, then_vars, then, catch_vars, catch } => {
            let mut block = block;

            // Control flow join block
            let join_block = ctx.b.block_insert();
            let join_arg = ctx.b.block_arg_insert(join_block);
            ctx.put_res(ResNode::Expr(expr), join_arg);

            let join_block_val = ctx.b.value_block(join_block);

            // Setup exception handler block
            let handler_block = ctx.b.block_insert();
            let handler_arg = ctx.b.block_arg_insert(handler_block);
            ctx.exc_stack.push_handler(ctx.b.value_block(handler_block));

            // Lower body
            block = lower_expr(ctx, block, *body);

            ctx.exc_stack.pop_handler();

            // Ok branch
            {
                let mut ok_block = block;

                let body_val = ctx.get_res(ResNode::Expr(*body));
                ok_block = unpack_value_list_variables(ctx, ok_block, body_val, then_vars);

                ok_block = lower_expr(ctx, ok_block, *then);

                let then_res = ctx.get_res(ResNode::Expr(*then));
                ctx.b.op_call(ok_block, join_block_val, &[then_res]);
            }

            // Err branch
            {
                let mut err_block = handler_block;

                err_block = unpack_value_list_variables(ctx, err_block, handler_arg, catch_vars);

                err_block = lower_expr(ctx, err_block, *catch);

                let catch_res = ctx.get_res(ResNode::Expr(*catch));
                ctx.b.op_call(err_block, join_block_val, &[catch_res]);
            }

            join_block
        }
        EK::Case { match_on, clauses } => {
            let mut block = block;

            // Lower match value
            block = lower_expr(ctx, block, *match_on);
            let match_on_val = ctx.get_res(ResNode::Expr(*match_on));

            // No match block
            // Since it is expected that the case is complete, this
            // lowers to an unreachable op.
             let no_match_block = ctx.b.block_insert();
             let no_match_block_val = ctx.b.value_block(no_match_block);
             ctx.b.op_unreachable(no_match_block);

            let (join_block, join_arg) = lower_match(ctx, block, match_on_val, clauses,
                                                     no_match_block_val, &|_ctx, bl| bl);

            ctx.put_res(ResNode::Expr(expr), join_arg);
            join_block
        }
        EK::Receive { clauses, timeout_time, timeout_body } => {
            let mut block = block;

            let wait_block = ctx.b.block_insert();
            let wait_block_val = ctx.b.value_block(wait_block);

            let match_entry_block = ctx.b.block_insert();
            let match_entry_block_val = ctx.b.value_block(wait_block);
            let match_msg = ctx.b.block_arg_insert(match_entry_block);

            let timeout_block = ctx.b.block_insert();
            let timeout_block_val = ctx.b.value_block(timeout_block);

            block = lower_expr(ctx, block, *timeout_time);
            let timeout_time_val = ctx.get_res(ResNode::Expr(*timeout_time));

            ctx.b.op_intrinsic(block, Atom::from_str("receive_start"), &[wait_block_val, timeout_time_val]);
            ctx.b.op_intrinsic(wait_block, Atom::from_str("receive_wait"), &[match_entry_block_val, timeout_block_val]);

            let (mut join_block, join_arg) = lower_match(
                ctx, match_entry_block, match_msg,
                clauses, wait_block_val, &|ctx, bl| {
                    let fin_block = ctx.b.block_insert();
                    let fin_block_val = ctx.b.block_arg_insert(fin_block);
                    ctx.b.op_intrinsic(bl, Atom::from_str("receive_finish"), &[fin_block_val]);
                    fin_block
                });
            let join_block_val = ctx.b.value_block(timeout_block);

            let tim_block = lower_expr(ctx, timeout_block, *timeout_body);
            let tim_val = ctx.get_res(ResNode::Expr(*timeout_body));
            ctx.b.op_call(tim_block, join_block_val, &[tim_val]);

            ctx.put_res(ResNode::Expr(expr), join_arg);
            join_block
        }
        _ => unimplemented!(),
    }
}









