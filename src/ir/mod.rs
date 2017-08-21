use std::collections::{ HashMap, HashSet };

pub mod hir;
use ::ir::hir::pass::ssa::ScopeTracker;
pub mod lir;

use ::intern::{ Atom, Variable };
use ::parser;

#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct SSAVariable(u32);
impl ::std::fmt::Debug for SSAVariable {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(f, "%{}", self.0)
    }
}

#[derive(Debug)]
pub struct Module {
    pub name: Atom,
    pub attributes: Vec<(Atom, parser::Constant)>,
    pub functions: Vec<FunctionDefinition>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionIdent {
    name: Atom,
    arity: u32,
    lambda: Option<u32>,
}
impl ::std::fmt::Display for FunctionIdent {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        if let Some(lambda_num) = self.lambda {
            write!(f, "{}@{}/{}", self.name, lambda_num, self.arity)
        } else {
            write!(f, "{}/{}", self.name, self.arity)
        }
    }
}

#[derive(Debug)]
pub struct FunctionDefinition {
    pub ident: FunctionIdent,
    pub visibility: FunctionVisibility,
    pub hir_fun: hir::Function,
    pub lir_function: Option<lir::FunctionCfg>,
}
#[derive(Debug)]
pub enum FunctionVisibility {
    Public,
    Private,
    Lambda,
}

#[derive(Debug, Clone)]
pub struct AVariable {
    var: Variable,
    ssa: SSAVariable,
}
impl AVariable {
    fn new(var: Variable) -> Self {
        AVariable {
            var: var,
            ssa: SSAVariable(0),
        }
    }
}

#[derive(Debug, Clone)]
pub struct AFunctionName {
    var: ::parser::FunctionName,
    ssa: SSAVariable,
}
impl AFunctionName {
    fn new(var: ::parser::FunctionName) -> Self {
        AFunctionName {
            var: var,
            ssa: SSAVariable(0),
        }
    }
}

pub fn from_parsed(parsed: &parser::Module) -> Module {
    let mut module = ::ir::hir::from_parsed::from_parsed(parsed);

    let mut env =  ScopeTracker::new();

    // Assign SSA variables
    for func in &mut module.functions {
        let mut scope = HashMap::new();
        for arg in &mut func.hir_fun.args {
            arg.ssa = env.new_ssa();
            scope.insert(::ir::hir::pass::ssa::ScopeDefinition::Variable(
                arg.var.clone()), arg.ssa);
        }
        env.push_scope(scope);
        ::ir::hir::pass::ssa::assign_ssa_single_expression(
            &mut env, &mut func.hir_fun.body);
        env.pop_scope();
    }

    // Extract lambdas
    let mut lambda_collector = ::ir::hir::pass::extract_lambda::LambdaCollector::new();
    for fun in module.functions.iter_mut() {
        lambda_collector.set_ident(fun.ident.clone());
        ::ir::hir::pass::extract_lambda::extract_lambdas(
            fun, &mut lambda_collector);
    }
    let mut lambdas = lambda_collector.finish();
    module.functions.extend(lambdas.drain(0..));

    ::ir::lir::from_hir::do_lower(&mut module);

    for function in module.functions.iter_mut() {
        let lir_mut = function.lir_function.as_mut().unwrap();
        println!("{}", function.ident);
        ::ir::lir::pass::compile_pattern(lir_mut);
        //::ir::lir::pass::propagate_atomics(lir_mut);
        //::ir::lir::pass::validate(lir_mut);
    }


    module
}
