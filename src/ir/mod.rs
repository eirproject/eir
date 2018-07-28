use std::collections::{ HashMap, HashSet };
use std::fmt::{ Debug, Formatter };
use ::pretty::{ Doc, BoxDoc };

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

pub struct Module {
    pub name: Atom,
    pub attributes: Vec<(Atom, parser::Constant)>,
    pub functions: Vec<FunctionDefinition>,
}

use ::ToDoc;

impl ::ToDoc for Module {
    fn to_doc<'a>(&'a self) -> Doc<'a, BoxDoc> {
        let head: Doc<BoxDoc> = Doc::text(format!("module {}:", self.name));
        let attrs: Doc<BoxDoc> = Doc::newline()
            .append(Doc::text(format!("attributes: {:?}", self.attributes)))
            .nest(2);

        let funs = self.functions.iter().map(|fun| {
            let args = Doc::intersperse(
                fun.hir_fun.args.iter().map(|arg| Doc::text(format!("{:?}", arg))),
                Doc::text(",").append(Doc::space()));
            let start_signature = Doc::concat(vec![
                Doc::newline(),
                Doc::text(format!("fun {:?} {}(", fun.visibility, fun.ident)),
            ]).group();
            let args_signature = Doc::concat(vec![
                args,
                Doc::text("):")
            ]).nest(4);
            let signature = Doc::concat(vec![
                start_signature, args_signature
            ]).group();

            let hir = Doc::newline().append(fun.hir_fun.body.to_doc());

            let lir = {
                let lir = fun.lir_function.as_ref().unwrap();
                let lir_blocks = lir.cfg.node_indices().map(|node_idx| {
                    let head = Doc::newline()
                        .append(Doc::text(format!("block #{}:", node_idx.index())));

                    let phis = lir.cfg[node_idx].phi_nodes.iter().map(|phi| {
                        Doc::newline().append(Doc::text(format!("{:?}", phi)))
                    });

                    let block_ops = lir.cfg[node_idx].ops.iter().map(|op| {
                        Doc::newline().append(Doc::text(format!("{:?}", op)))
                    });

                    let branches_vec = lir.cfg
                        .neighbors_directed(node_idx, ::petgraph::Direction::Outgoing)
                        .map(|branch| Doc::text(format!("{:?}", branch)));
                    let branches = Doc::newline()
                        .append(Doc::text("branch ["))
                        .append(Doc::intersperse(branches_vec, Doc::text(",").append(Doc::space()))
                                .nest(2).group())
                        .append(Doc::text("]"));

                    Doc::concat(vec![
                        head,
                        Doc::concat(phis).nest(2),
                        Doc::concat(block_ops).nest(2),
                        branches.nest(2),
                    ])
                });

                Doc::concat(lir_blocks)
            };

            Doc::concat(vec![
                signature,
                Doc::newline().append(Doc::text("hir:")).nest(2),
                hir.nest(4),
                Doc::newline().append(Doc::text("lir:")).nest(2),
                lir.nest(4),
            ])
        });
        let funs_doc = Doc::concat(funs).nest(2);

        Doc::concat(vec![
            head, attrs, funs_doc
        ])
    }
}

impl Debug for Module {
    fn fmt(&self, f: &mut Formatter) -> Result<(), ::std::fmt::Error> {
        self.to_doc().render_fmt(80, f)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct FunctionIdent {
    pub name: Atom,
    pub arity: u32,
    pub lambda: Option<u32>,
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
    pub var: Variable,
    pub ssa: SSAVariable,
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

    let mut env = ScopeTracker::new();

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
            &mut fun.hir_fun, &mut lambda_collector);
    }
    let mut lambdas = lambda_collector.finish();
    module.functions.extend(lambdas.drain(0..));

    // Compile patterns to decision tree
    //for fun in module.functions.iter_mut() {
    //    ::ir::hir::pass::pattern::pattern_to_cfg(fun);
    //}

    // Lower to LIR
    ::ir::lir::from_hir::do_lower(&mut module, &mut env);

    for function in module.functions.iter_mut() {
        let lir_mut = function.lir_function.as_mut().unwrap();
        println!("Function: {}", function.ident);
        ::ir::lir::pass::compile_pattern(lir_mut);
        //::ir::lir::pass::propagate_atomics(lir_mut);
        //::ir::lir::pass::validate(lir_mut);
    }


    module
}
