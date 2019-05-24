use std::collections::HashMap;

//pub mod hir;
use ::hir::scope_tracker::{ ScopeTracker, ScopeDefinition };
pub use ::eir::{ ModuleEnvs, ClosureEnv };
pub use ::eir::FunctionIdent;
use ::eir::FunctionBuilder;
//pub mod lir;
//pub mod hir_new;

use ::{ Atom, Variable };

pub use ::ssa::{ SSAVariable, INVALID_SSA };

pub struct Module {
    pub name: Atom,
    pub attributes: Vec<(Atom, eir::ConstantTerm)>,
    pub functions: Vec<FunctionDefinition>,
    pub envs: Option<ModuleEnvs>,
    //pub lambda_envs: Option<HashMap<LambdaEnvIdx, LambdaEnv>>,
}
impl Module {
    //pub fn get_env<'a>(&'a self, env_idx: Closure) -> &'a LambdaEnv {
    //    &self.lambda_envs.as_ref().unwrap()[&env_idx]
    //}
    pub fn to_eir(mut self) -> ::eir::Module {
        ::eir::Module {
            name: self.name,
            functions: self.functions.drain(..)
                .map(|f| {
                    (f.ident, f.eir_fun.unwrap())
                }).collect(),
            envs: self.envs.unwrap(),
        }
    }
}

#[derive(Debug)]
pub struct FunctionDefinition {
    pub ident: FunctionIdent,
    pub visibility: FunctionVisibility,
    pub hir_fun: crate::hir::Function,
    pub lambda_env_idx: Option<ClosureEnv>,
    pub eir_fun: Option<::eir::Function>,
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
    pub fn new(var: Variable) -> Self {
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

pub fn to_eir(module: &Module) -> ::eir::Module {
    let mut env = ScopeTracker::new();

    println!("STAGE: Assign SSA");
    // Assign SSA variables
    for func in &mut module.functions {
        //println!("Fun: {}", func.ident);
        let mut scope = HashMap::new();
        for arg in &mut func.hir_fun.args {
            arg.ssa = env.new_ssa();
            scope.insert(ScopeDefinition::Variable(
                arg.var.clone()), arg.ssa);
        }
        env.push_scope(scope);
        ::hir::pass::ssa::assign_ssa_single_expression(
            &mut env, &mut func.hir_fun.body);
        env.pop_scope();
    }

    println!("STAGE: Extract lambdas");
    // Extract lambdas
    let mut lambda_collector = ::hir::pass::extract_lambda::LambdaCollector::new();
    for fun in module.functions.iter_mut() {
        //println!("Function: {}", fun.ident);
        ::hir::pass::extract_lambda::extract_lambdas(
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
    ::lower::do_lower(&mut module, &mut env);
    let lambda_envs = env.finish();
    module.envs = Some(lambda_envs);

    module.to_eir()
}

