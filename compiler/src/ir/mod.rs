use std::collections::HashMap;

pub mod hir;
use ::ir::hir::scope_tracker::{ ScopeTracker, ScopeDefinition };
pub use ::eir::{ LambdaEnv, LambdaEnvIdx };
use ::eir::FunctionIdent;
use ::eir::FunctionBuilder;
pub mod lir;
mod doc;
mod fmt;

use ::{ Atom, Variable };
use ::parser;

use ::ssa::{ SSAVariable, INVALID_SSA };

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
    pub fn to_eir(mut self) -> ::eir::Module {
        ::eir::Module {
            name: self.name,
            functions: self.functions.drain(..)
                .map(|f| {
                    (f.ident, f.eir_fun.unwrap())
                }).collect(),
            lambda_envs: self.lambda_envs.unwrap(),
        }
    }
}

#[derive(Debug)]
pub struct FunctionDefinition {
    pub ident: FunctionIdent,
    pub visibility: FunctionVisibility,
    pub hir_fun: hir::Function,
    pub lambda_env_idx: Option<LambdaEnvIdx>,
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

pub fn from_parsed(parsed: &parser::Module) -> ::eir::Module {
    println!("STAGE: From parsed");
    let mut module = ::ir::hir::from_parsed::from_parsed(parsed);

    let mut env = ScopeTracker::new();

    println!("STAGE: Assign SSA");
    // Assign SSA variables
    for func in &mut module.functions {
        println!("Fun: {}", func.ident);
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

    let fun_idents: Vec<_> = module.functions.iter()
        .map(|f| f.ident.clone()).collect();
    let mut eir_module = module.to_eir();

    // Validate CFG between each major pass.
    let hardass_validate = true;

    println!("STAGE: Functionwise");
    for fun_ident in fun_idents.iter() {
        let mut function = eir_module.functions.get_mut(fun_ident).unwrap();
        println!("Function: {}", function.ident());

        let mut builder = FunctionBuilder::new(&mut function);

        //if hardass_validate { builder.function().validate() }

        // Remove orphans in generated LIR
        ::ir::lir::pass::remove_orphan_blocks(&mut builder);
        if hardass_validate { builder.function().validate() }

        // Propagate atomics
        //::ir::lir::pass::propagate_atomics(function);
        //if hardass_validate { function.validate() }

        // Promote tail calls
        // AFTER propagate atomics
        //::ir::lir::pass::promote_tail_calls(function);
        // REQUIRED after promote tail calls
        //::ir::lir::pass::remove_orphan_blocks(function);
        //if hardass_validate { function.validate() }

        //::ir::lir::pass::simplify_branches(function);
        //::ir::lir::pass::remove_orphan_blocks(function);
        //if hardass_validate { function.validate() }

        //let mut out = ::std::fs::File::create("immediate1.dot").unwrap();
        //::ir::lir::to_dot::function_to_dot(
        //    fun_ident, lir_mut, &mut out).unwrap();

        ::ir::lir::pass::compile_pattern(&mut builder);
        //::ir::lir::pass::propagate_atomics(function);
        ::ir::lir::pass::simplify_branches(&mut builder);
        ::ir::lir::pass::remove_orphan_blocks(&mut builder);
        //if hardass_validate { function.validate() }

        //lir_mut.compress_numbering();
        builder.function().validate();

        let live = builder.function().live_values();
        let entry = builder.function().ebb_entry();
        let at_entry = &live.ebb_live[&entry];
        for value in at_entry.iter(&live.pool) {
            println!("{:?}", value);
        }

        //println!("Calls: {:?}", lir_mut.get_all_calls());

    }

    eir_module
}
