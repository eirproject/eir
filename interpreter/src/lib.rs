//! LIR interpreter with zero consideration of performance.
//! Made as an experiment to narrow down relevant implementation
//! details.

use std::str::FromStr;
use std::collections::HashMap;

extern crate core_erlang_compiler;
use core_erlang_compiler::intern::Atom;
use core_erlang_compiler::ir::{ Module, FunctionIdent, SSAVariable };
use core_erlang_compiler::ir::lir::{ BasicBlock, LabelN, OpKind, Source };
use core_erlang_compiler::parser::{ Constant, AtomicLiteral };

extern crate num_bigint;
use num_bigint::BigInt;

mod term;
pub use term::{ TermType, Term };

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

struct StackFrame {
    variables: HashMap<SSAVariable, Term>,
}
impl StackFrame {

    fn new() -> Self {
        StackFrame {
            variables: HashMap::new(),
        }
    }

    fn read(&self, src: &Source) -> Term {
        match *src {
            Source::Variable(ref var) => self.variables[var].clone(),
            Source::Constant(ref literal) => {
                match *literal {
                    AtomicLiteral::Atom(ref atom) => Term::Atom(atom.clone()),
                    _ => unimplemented!(),
                }
            }
        }
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
        assert!(block.phi_nodes.len() == 0);

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
                    frame.variables.insert(op.writes[0], res);
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
                            frame.variables.insert(op.writes[0], term);
                            block_ret = Some(BlockResult::Branch { slot: 1 });
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
                    frame.variables.insert(op.writes[0], res);
                }
                OpKind::Apply => {
                    assert!(op.writes.len() == 1);
                    assert!(op.reads.len() >= 1);

                    let fun_var = frame.read(&op.reads[0]);
                    let args: Vec<Term> = op.reads[1..].iter()
                        .map(|arg| frame.read(arg)).collect();

                    if let Term::CapturedFunction { module: ref module_a, ref fun_name,
                                                    ref arity } = fun_var {
                        assert!(args.len() == *arity as usize);

                        let module_str: &str = &*module_a;
                        let fun_str: &str = &*fun_name;

                        let ret = self.call(module_str, fun_str, &args);
                        match ret {
                            CallReturn::Return { term } => {
                                frame.variables.insert(op.writes[0], term);
                                block_ret = Some(BlockResult::Branch { slot: 1 });
                            }
                            CallReturn::Throw => unimplemented!(),
                        }
                    } else {
                        panic!("Var is not CapturedFunction");
                    }
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
                          args: &[Term]) -> CallReturn {
        let fun_opt = module.functions.iter().find(|fun| &fun.ident == fun_ident);
        if fun_opt.is_none() {
            panic!("function {} not found in module {:?}", fun_ident, module.name);
        }
        let fun = fun_opt.unwrap();

        let lir = fun.lir_function.as_ref().unwrap();

        let mut frame = StackFrame::new();

        // Insert arguments into frame
        for (var_def, term) in fun.hir_fun.args.iter().zip(args) {
            frame.variables.insert(var_def.ssa, term.clone());
        }

        // Execute blocks
        let mut prev_block_id: Option<LabelN> = None;
        let mut curr_block_id = lir.entry();
        loop {
            let block = lir.block(curr_block_id);
            let slots = lir.branch_slots(curr_block_id);
            for slot in &slots {
                let edge = lir.cfg.find_edge(curr_block_id.0, slot.0).unwrap();
                println!("{:?}", lir.cfg.edge_weight(edge));
            }
            let ret = self.exec_block(module, block, prev_block_id, &mut frame);
            match ret {
                BlockResult::Branch { slot } => {
                    prev_block_id = Some(curr_block_id);
                    curr_block_id = slots[slot];
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
        let fun_name_str: &str = &fun_ident.name;
        module.functions[&(fun_name_str.to_string(), fun_ident.arity)](args)
    }

    pub fn call(&self, module_name: &str, fun_name: &str, args: &[Term]) -> CallReturn {
        let fun_ident = FunctionIdent {
            name: Atom::from_str(fun_name).unwrap(),
            arity: args.len() as u32,
            lambda: None,
        };

        if !self.modules.contains_key(module_name) {
            panic!("module {:?} not found", module_name);
        }
        let module = &self.modules[module_name];

        match module {
            &ModuleType::Erlang(ref erl_module) => 
                self.call_erlang_module(erl_module, &fun_ident, args),
            &ModuleType::Native(ref native_module) => 
                self.call_native_module(native_module, &fun_ident, args),
        }
    }

}
