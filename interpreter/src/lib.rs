//! LIR interpreter with zero consideration of performance.
//! Made as an experiment to narrow down relevant implementation
//! details.

use std::str::FromStr;
use std::collections::{ HashMap, HashSet };
use std::cell::RefCell;
use std::rc::Rc;

extern crate core_erlang_compiler;
use core_erlang_compiler::intern::Atom;
use core_erlang_compiler::ir::{ Module, FunctionIdent, SSAVariable };
use core_erlang_compiler::ir::lir::{ BasicBlock, LabelN, OpKind, Source };
use core_erlang_compiler::parser::AtomicLiteral;

extern crate num_bigint;

mod term;
pub use term::{ TermType, Term, BoundLambdaEnv };
mod pattern;
use pattern::{ CaseContext, MatchState };

pub mod erl_lib;
#[cfg(test)] pub mod erl_tests;

pub struct NativeModule {
    name: String,
    functions: HashMap<(String, u32), Box<Fn(&[Term]) -> CallReturn>>,
}
impl NativeModule {

    fn new(name: String) -> Self {
        NativeModule {
            name: name,
            functions: HashMap::new(),
        }
    }

    fn add_fun(&mut self, name: String, arity: u32, fun: Box<Fn(&[Term]) -> CallReturn>) {
        self.functions.insert((name, arity), fun);
    }

}

enum ModuleType {
    Erlang(Module),
    Native(NativeModule),
}

#[derive(Debug, Clone)]
pub enum CallReturn {
    Return { term: Term },
    Throw,
}
impl CallReturn {

    pub fn unwrap_return<'a>(&'a self) -> &'a Term {
        match self {
            CallReturn::Return { ref term } => term,
            _ => panic!("Expected return"),
        }
    }

}

struct StackFrame {
    variables: HashMap<SSAVariable, Term>,
    tombstones: HashSet<SSAVariable>,
}
impl StackFrame {

    fn new() -> Self {
        StackFrame {
            variables: HashMap::new(),
            tombstones: HashSet::new(),
        }
    }

    fn write(&mut self, ssa: SSAVariable, term: Term) {
        self.tombstones.remove(&ssa);
        self.variables.insert(ssa, term);
    }

    fn read(&self, src: &Source) -> Term {
        match *src {
            Source::Variable(ref var) => {
                if let Some(term) = self.variables.get(var) {
                    term.clone()
                } else {
                    panic!("var {:?} not found in frame", var);
                }
            }
            Source::Constant(ref literal) => {
                match *literal {
                    AtomicLiteral::Atom(ref atom) => Term::Atom(atom.clone()),
                    _ => unimplemented!(),
                }
            }
        }
    }

    fn tombstone(&mut self, ssa: SSAVariable) {
        self.tombstones.insert(ssa);
    }

}

enum BlockResult {
    Branch { slot: usize },
    Return { term: Term },
    Throw,
}

pub struct ExecutionContext {
    modules: HashMap<String, ModuleType>,
}

impl ExecutionContext {

    pub fn new() -> Self {
        ExecutionContext {
            modules: HashMap::new(),
        }
    }

    pub fn add_erlang_module(&mut self, module: Module) {
        self.modules.insert(module.name.to_string(), ModuleType::Erlang(module));
    }

    pub fn add_native_module(&mut self, module: NativeModule) {
        self.modules.insert(module.name.clone(), ModuleType::Native(module));
    }

    fn exec_block(&self, module: &Module, block: &BasicBlock,
                  prev: Option<LabelN>, frame: &mut StackFrame) -> BlockResult {

        // Apply phi nodes
        for phi in &block.phi_nodes {
            let source = &phi.entries.iter()
                .find(|(label, _)| label == prev.as_ref().unwrap())
                .unwrap().1;
            let term = frame.read(source);
            frame.write(phi.ssa, term);
        }

        let mut block_ret: Option<BlockResult> = None;
        for op in &block.ops {
            assert!(block_ret.is_none());
            match op.kind {
                OpKind::Arguments => {
                    assert!(op.reads.len() == 0);
                    assert!(op.writes.iter().all(|w| frame.variables.contains_key(w)))
                }
                OpKind::Move => {
                    assert!(op.reads.len() == 1);
                    assert!(op.writes.len() == 1);
                    let res = frame.read(&op.reads[0]);
                    frame.write(op.writes[0], res);
                }
                OpKind::Call => {
                    assert!(op.reads.len() >= 2);
                    assert!(op.writes.len() == 1);

                    let module_term = frame.read(&op.reads[0]);
                    let fun_term = frame.read(&op.reads[1]);
                    let args: Vec<Term> = op.reads[2..].iter()
                        .map(|arg| frame.read(arg)).collect();

                    let ret = self.call(module_term.atom_str(), fun_term.atom_str(), &args);
                    match ret {
                        CallReturn::Return { term } => {
                            frame.write(op.writes[0], term);
                            block_ret = Some(BlockResult::Branch { slot: 0 });
                        }
                        CallReturn::Throw => unimplemented!(),
                    }
                }
                OpKind::ReturnOk => {
                    assert!(op.reads.len() == 1);
                    assert!(op.writes.len() == 0);
                    block_ret = Some(BlockResult::Return { term: frame.read(&op.reads[0]) });
                }
                OpKind::CaptureNamedFunction(ref ident) => {
                    assert!(op.reads.len() == 0);
                    assert!(op.writes.len() == 1);
                    assert!(ident.lambda.is_none());
                    let res = Term::CapturedFunction {
                        module: module.name.clone(),
                        fun_name: ident.name.clone(),
                        arity: ident.arity,
                    };
                    frame.write(op.writes[0], res);
                }
                OpKind::Apply => {
                    assert!(op.writes.len() == 1);
                    assert!(op.reads.len() >= 1);

                    let fun_var = frame.read(&op.reads[0]);
                    let args: Vec<Term> = op.reads[1..].iter()
                        .map(|arg| frame.read(arg)).collect();

                    match fun_var {
                        Term::CapturedFunction { module: ref module_a,
                                                 ref fun_name, ref arity } => {
                            assert!(args.len() == *arity as usize);

                            let module_str: &str = module_a.as_str();
                            let ret = self.call_base(module_str, fun_name,
                                                     &args, None);

                            match ret {
                                CallReturn::Return { term } => {
                                    frame.write(op.writes[0], term);
                                    block_ret = Some(BlockResult::Branch { slot: 0 });
                                }
                                CallReturn::Throw => unimplemented!(),
                            }
                        }
                        Term::BoundLambda {
                            module: ref module_a, ref fun_name, arity,
                            lambda, ref bound_env } => {

                            assert!(args.len() == (arity as usize));

                            let module_str: &str = module_a.as_str();
                            let ret = self.call_base(
                                module_str, fun_name, &args,
                                Some((lambda, bound_env.clone())));

                            match ret {
                                CallReturn::Return { term } => {
                                    frame.write(op.writes[0], term);
                                    block_ret = Some(BlockResult::Branch { slot: 0 });
                                },
                                CallReturn::Throw => unimplemented!(),
                            }
                        }
                        _ => panic!("Var is not callable"),
                    }
                }
                OpKind::MakeClosureEnv { ref env_idx } => {
                    assert!(op.reads.len() > 0);
                    assert!(op.writes.len() == 1);

                    let env_vars: Vec<Term> = op.reads.iter()
                        .map(|r| frame.read(r))
                        .collect();

                    frame.write(op.writes[0], Term::LambdaEnv(BoundLambdaEnv {
                        env: *env_idx,
                        vars: env_vars,
                    }));
                }
                OpKind::BindClosure { ref ident } => {
                    assert!(op.reads.len() == 1);
                    assert!(op.writes.len() == 1);

                    let lenv = if let Term::LambdaEnv(bound_env)
                        = frame.read(&op.reads[0]) {
                        bound_env
                    } else {
                        panic!();
                    };

                    frame.write(op.writes[0], Term::BoundLambda {
                        module: module.name.clone(),
                        fun_name: ident.name.clone(),
                        arity: ident.arity,
                        lambda: ident.lambda.unwrap(),
                        bound_env: lenv.clone(),
                    });
                }
                OpKind::CaseStart { ref vars, ref clauses, ref value_vars } => {
                    let case_ctx = CaseContext::new(op.reads.clone(), clauses.clone());

                    frame.write(op.writes[0], Term::CaseContext(
                        Rc::new(RefCell::new(case_ctx))));
                }
                OpKind::Case(_) => {
                    let to_leaf = {
                        let curr = frame.read(&op.reads[0]);
                        let mut ctx = if let Term::CaseContext(ref ctx) = curr {
                            ctx.borrow_mut()
                        } else {
                            panic!("Case read not case context");
                        };

                        let args: Vec<_> = ctx.vars.iter()
                            .map(|s| frame.read(s))
                            .collect();
                        ctx.do_body(&args)
                    };
                    block_ret = Some(BlockResult::Branch { slot: to_leaf });
                }
                OpKind::CaseValues => {
                    let mut vals = {
                        let curr = frame.read(&op.reads[0]);
                        let mut ctx = if let Term::CaseContext(ref ctx) = curr {
                            ctx.borrow_mut()
                        } else {
                            panic!("case read not case context");
                        };

                        ctx.case_values()
                    };
                    for write in &op.writes {
                        frame.write(*write, vals.remove(write).unwrap());
                    }
                }
                OpKind::CaseGuardOk => {
                    let curr = frame.read(&op.reads[0]);
                    let mut ctx = if let Term::CaseContext(ref ctx) = curr {
                        ctx.borrow_mut()
                    } else {
                        panic!("case read not case context");
                    };
                    ctx.guard_ok();
                }
                OpKind::Jump => {
                    block_ret = Some(BlockResult::Branch { slot: 0 });
                }
                OpKind::IfTruthy => {
                    let false_atom = &*core_erlang_compiler::intern::FALSE;
                    let matched = match frame.read(&op.reads[0]) {
                        Term::Atom(ref atom) if atom == false_atom => false,
                        _ => true,
                    };

                    if matched {
                        block_ret = Some(BlockResult::Branch { slot: 0 });
                    } else {
                        block_ret = Some(BlockResult::Branch { slot: 1 });
                    }
                }
                OpKind::TombstoneSSA(ssa) => {
                    frame.tombstone(ssa);
                }
                OpKind::MakeTuple => {
                    let term = Term::Tuple(
                        op.reads.iter().map(|r| frame.read(r)).collect()
                    );
                    frame.write(op.writes[0], term);
                }
                _ => {
                    println!("Unimpl: {:?}", op);
                    println!("Variables: {:?}", frame.variables);
                    unimplemented!()
                }
            }
        }

        return block_ret.unwrap();
    }

    fn call_erlang_module(&self, module: &Module, fun_ident: &FunctionIdent,
                          args: &[Term],
                          env: Option<BoundLambdaEnv>)
                          -> CallReturn {

        let fun_opt = module.functions.iter().find(|fun| &fun.ident == fun_ident);
        if fun_opt.is_none() {
            panic!("function {} not found in module {:?}", fun_ident, module.name);
        }
        let fun = fun_opt.unwrap();

        let lir = fun.lir_function.as_ref().unwrap();

        let mut frame = StackFrame::new();

        // Insert arguments into frame
        if let Some(ref ienv) = env {
            assert!(ienv.env == fun.lambda_env_idx.unwrap());
            let env_def = module.get_env(fun.lambda_env_idx.unwrap());

            println!("Insert lambda env into frame {:?}, {:?}", env_def, ienv.vars);

            for ((_, _, ssa), term) in env_def.captures.iter().zip(ienv.vars.iter()) {
                frame.write(*ssa, term.clone());
            }
        } else {
            assert!(fun_ident.lambda.is_none());
        }

        for (var_def, term) in fun.hir_fun.args.iter().zip(args) {
            frame.write(var_def.ssa, term.clone());
        }

        // Execute blocks
        let mut prev_block_id: Option<LabelN> = None;
        let mut curr_block_id = lir.entry();
        loop {
            println!("Executing {}", curr_block_id);

            let block = lir.block(curr_block_id);
            let slots = lir.branch_slots(curr_block_id);
            //for slot in &slots {
            //    let edge = lir.cfg.find_edge(curr_block_id.0, slot.0).unwrap();
            //    println!("{:?}", lir.cfg.edge_weight(edge));
            //}
            let ret = self.exec_block(module, block, prev_block_id, &mut frame);
            match ret {
                BlockResult::Branch { slot } => {
                    prev_block_id = Some(curr_block_id);
                    curr_block_id = slots[slot];
                    println!("Branching to slot {}", slot);
                }
                BlockResult::Return { term } => {
                    return CallReturn::Return { term };
                }
                BlockResult::Throw => {
                    return CallReturn::Throw;
                }
            }
        }
    }

    fn call_native_module(&self, module: &NativeModule, fun_ident: &FunctionIdent,
                          args: &[Term]) -> CallReturn {
        assert!(args.len() == fun_ident.arity as usize);
        // bad
        let fun_name_str: &str = fun_ident.name.as_str();
        module.functions[&(fun_name_str.to_string(), fun_ident.arity)](args)
    }

    pub fn call(&self, module_name: &str, fun_name: &str, args: &[Term])
                -> CallReturn {
        self.call_base(module_name, &Atom::from_str(fun_name), args, None)
    }

    pub fn call_base(&self, module_name: &str, fun_name: &Atom, args: &[Term],
                     lambda: Option<(u32, BoundLambdaEnv)>) -> CallReturn {

        let fun_ident = FunctionIdent {
            name: fun_name.clone(),
            arity: args.len() as u32,
            lambda: lambda.as_ref().map(|l| l.0),
        };

        if !self.modules.contains_key(module_name) {
            panic!("module {:?} not found", module_name);
        }
        let module = &self.modules[module_name];

        match module {
            ModuleType::Erlang(ref erl_module) => {
                self.call_erlang_module(erl_module, &fun_ident, args,
                                        lambda.map(|l| l.1))
            }
            ModuleType::Native(ref native_module) => {
                assert!(lambda.is_none()); // No lambdas in native modules
                self.call_native_module(native_module, &fun_ident, args)
            }
        }
    }

}
