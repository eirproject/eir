use std::collections::HashMap;

pub mod hir;
use ::ir::hir::scope_tracker::{ ScopeTracker, ScopeDefinition };
pub use ::eir::{ LambdaEnv, LambdaEnvIdx };
use ::eir::FunctionIdent;
pub mod lir;
mod doc;
mod fmt;

use ::{ Atom, Variable };
use ::parser;

use ::eir::ssa::{ SSAVariable, INVALID_SSA };

pub struct Module {
    pub name: Atom,
    pub attributes: Vec<(Atom, parser::Constant)>,
    pub functions: Vec<FunctionDefinition>,
    pub lambda_envs: Option<HashMap<LambdaEnvIdx, LambdaEnv>>,
}
impl Module {
    pub fn get_env<'a>(&'a self, env_idx: LambdaEnvIdx) -> &'a LambdaEnv {
        &self.lambda_envs.as_ref().unwrap()[&env_idx]
    }
}

#[derive(Debug)]
pub struct FunctionDefinition {
    pub ident: FunctionIdent,
    pub visibility: FunctionVisibility,
    pub hir_fun: hir::Function,
    pub lir_function: Option<lir::FunctionCfg>,
    pub lambda_env_idx: Option<LambdaEnvIdx>,
}
#[derive(Debug)]
pub enum FunctionVisibility {
    Public,
    Private,
    Lambda,
}

#[derive(Debug, Clone)]
pub struct AVariable {
    pub var: Variable,
    pub ssa: SSAVariable,
}
impl AVariable {
    fn new(var: Variable) -> Self {
        AVariable {
            var: var,
            ssa: INVALID_SSA,
        }
    }
}

#[derive(Debug, Clone)]
pub struct AFunctionName {
    var: FunctionIdent,
    ssa: SSAVariable,
}
impl AFunctionName {
    fn new(var: FunctionIdent) -> Self {
        AFunctionName {
            var: var,
            ssa: INVALID_SSA,
        }
    }
}

pub fn from_parsed(parsed: &parser::Module) -> Module {
    println!("STAGE: From parsed");
    let mut module = ::ir::hir::from_parsed::from_parsed(parsed);

    let mut env = ScopeTracker::new();

    println!("STAGE: Assign SSA");
    // Assign SSA variables
    for func in &mut module.functions {
        println!("Fun: {:?}", func.ident);
        let mut scope = HashMap::new();
        for arg in &mut func.hir_fun.args {
            arg.ssa = env.new_ssa();
            scope.insert(ScopeDefinition::Variable(
                arg.var.clone()), arg.ssa);
        }
        env.push_scope(scope);
        ::ir::hir::pass::ssa::assign_ssa_single_expression(
            &mut env, &mut func.hir_fun.body);
        env.pop_scope();
    }

    println!("STAGE: Extract lambdas");
    // Extract lambdas
    let mut lambda_collector = ::ir::hir::pass::extract_lambda::LambdaCollector::new();
    for fun in module.functions.iter_mut() {
        println!("Function: {}", fun.ident);
        ::ir::hir::pass::extract_lambda::extract_lambdas(
            &mut fun.hir_fun, &mut lambda_collector);
    }
    let mut lambdas = lambda_collector.finish();
    module.functions.extend(lambdas.drain(0..));

    // Compile patterns to decision tree
    //for fun in module.functions.iter_mut() {
    //    ::ir::hir::pass::pattern::pattern_to_cfg(fun);
    //}

    println!("STAGE: Lower to LIR");
    // Lower to LIR
    ::ir::lir::from_hir::do_lower(&mut module, &mut env);

    module.lambda_envs = Some(env.finish());

    // Validate CFG between each major pass.
    let hardass_validate = false;

    println!("STAGE: Functionwise");
    for function in module.functions.iter_mut() {
        let lir_mut = function.lir_function.as_mut().unwrap();
        println!("Function: {}", function.ident);

        if hardass_validate { ::ir::lir::pass::validate(&function.ident, lir_mut); }

        // Remove orphans in generated LIR
        ::ir::lir::pass::remove_orphan_blocks(lir_mut);
        if hardass_validate { ::ir::lir::pass::validate(&function.ident, lir_mut); }

        // Propagate atomics
        ::ir::lir::pass::propagate_atomics(lir_mut);
        if hardass_validate { ::ir::lir::pass::validate(&function.ident, lir_mut); }

        // Promote tail calls
        // AFTER propagate atomics
        ::ir::lir::pass::promote_tail_calls(lir_mut);
        // REQUIRED after promote tail calls
        ::ir::lir::pass::remove_orphan_blocks(lir_mut);

        ::ir::lir::pass::validate(&function.ident, lir_mut);

    }


    module
}
