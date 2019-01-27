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
use core_erlang_compiler::ir::hir::scope_tracker::LambdaEnvIdx;
use core_erlang_compiler::parser::AtomicLiteral;

extern crate num_bigint;
use num_bigint::BigInt;
extern crate num_traits;

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

    fn has_fun(&self, ident: &FunctionIdent) -> bool {
        if ident.lambda.is_some() {
            false
        } else {
            self.functions.contains_key(&(ident.name.to_string(), ident.arity))
        }
    }

}

enum ModuleType {
    Erlang(Module, Option<NativeModule>),
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
    //tombstones: HashSet<SSAVariable>,
}
impl StackFrame {

    fn new() -> Self {
        StackFrame {
            variables: HashMap::new(),
            //tombstones: HashSet::new(),
        }
    }

    fn write(&mut self, ssa: SSAVariable, term: Term) {
        //self.tombstones.remove(&ssa);
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
                    AtomicLiteral::Integer(ref inner) => {
                        let mut bi = BigInt::parse_bytes(inner.digits.as_bytes(), 10)
                            .unwrap();
                        if !inner.sign {
                            bi *= -1;
                        }
                        Term::Integer(bi)
                    },
                    AtomicLiteral::Nil => Term::Nil,
                    _ => unimplemented!(),
                }
            }
        }
    }

    fn tombstone(&mut self, ssa: SSAVariable) {
        //self.tombstones.insert(ssa);
        self.variables.remove(&ssa);
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

fn unpack_value_list(val_list: Term) -> Vec<Term> {
    if val_list.get_type() == TermType::ValueList {
        if let Term::ValueList(list) = val_list {
            return list.clone();
        }
        unreachable!()
    } else {
        return vec![val_list];
    }
}

impl ExecutionContext {

    pub fn new() -> Self {
        ExecutionContext {
            modules: HashMap::new(),
        }
    }

    pub fn add_erlang_module(&mut self, module: Module) {
        self.modules.insert(module.name.to_string(), ModuleType::Erlang(module, None));
    }

    pub fn add_native_module(&mut self, module: NativeModule) {
        self.modules.insert(module.name.clone(), ModuleType::Native(module));
    }

    pub fn add_nif_overlay(&mut self, module: NativeModule) {
        let existing = self.modules.get_mut(&module.name).unwrap();
        if let ModuleType::Erlang(_, ref mut overlay) = existing {
            assert!(overlay.is_none());
            *overlay = Some(module);
        } else {
            panic!();
        }
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
                OpKind::UnpackValueList => {
                    assert!(op.reads.len() == 1);
                    let res = frame.read(&op.reads[0]);
                    let unpacked = unpack_value_list(res);
                    assert!(unpacked.len() == op.writes.len());
                    for (var, val) in op.writes.iter().zip(unpacked) {
                        frame.write(*var, val);
                    }
                }
                OpKind::UnpackEnv => {
                    assert!(op.reads.len() == 1);
                    let env = frame.read(&op.reads[0]);
                    assert!(env.get_type() == TermType::LambdaEnv);
                    if let Term::LambdaEnv(data) = env {
                        assert!(data.vars.len() == op.writes.len());
                        for (var, term) in op.writes.iter().zip(data.vars) {
                            frame.write(*var, term);
                        }
                    }
                }
                OpKind::PackValueList => {
                    assert!(op.writes.len() == 1);
                    let reads_val: Vec<_> = op.reads.iter()
                        .map(|r| frame.read(r))
                        .collect();
                    frame.write(op.writes[0], Term::ValueList(reads_val));
                }
                OpKind::Move => {
                    assert!(op.reads.len() == 1);
                    assert!(op.writes.len() == 1);
                    let res = frame.read(&op.reads[0]);
                    frame.write(op.writes[0], res);
                }
                OpKind::Call => {
                    assert!(op.reads.len() >= 2);
                    assert!(op.writes.len() == 2);

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
                        CallReturn::Throw => {
                            // TODO
                            frame.write(op.writes[1], Term::ValueList(
                                vec![Term::Nil, Term::Nil]));
                            block_ret = Some(BlockResult::Branch { slot: 1 });
                        },
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
                    assert!(op.writes.len() == 2);
                    assert!(op.reads.len() >= 1);

                    let fun_var = frame.read(&op.reads[0]);
                    let args: Vec<Term> = op.reads[1..].iter()
                        .map(|arg| frame.read(arg)).collect();

                    match fun_var {
                        Term::CapturedFunction { module: ref module_a,
                                                 ref fun_name, ref arity } => {
                            assert!(args.len() == *arity as usize);

                            let module_str: &str = module_a.as_str();
                            let ret = self.call_base(module_str, fun_name, None, &args);

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

                            let mut rargs = vec![Term::LambdaEnv(bound_env.clone())];
                            rargs.extend(args.iter().cloned());

                            assert!(rargs.len() == (arity as usize));

                            let module_str: &str = module_a.as_str();
                            let ret = self.call_base(
                                module_str, fun_name, Some(lambda), &rargs);

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
                    let term = frame.read(&op.reads[0]);
                    let vals = unpack_value_list(term);
                    let case_ctx = CaseContext::new(vals, clauses.clone());

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

                        //let args: Vec<_> = ctx.vars.iter()
                        //    .map(|s| frame.read(s))
                        //    .collect();
                        ctx.do_body()
                    };
                    block_ret = Some(BlockResult::Branch { slot: to_leaf });
                }
                OpKind::CaseGuardFail { clause_num } => {
                    let curr = frame.read(&op.reads[0]);
                    let mut ctx = if let Term::CaseContext(ref ctx) = curr {
                        ctx.borrow_mut()
                    } else {
                        panic!("Case read not case context");
                    };

                    ctx.guard_fail(clause_num);
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
                OpKind::MakeList => {
                    assert!(op.reads.len() >= 1);
                    assert!(op.writes.len() == 1);
                    let tail = frame.read(&op.reads[0]);
                    let mut front: Vec<_> = op.reads.iter()
                        .skip(1)
                        .map(|r| frame.read(r))
                        .collect();

                    // Just merge lists for prettyness
                    if let Term::List(i_head, i_tail) = tail {
                        front.extend(i_head);
                        frame.write(op.writes[0], Term::List(front, i_tail));
                    } else {
                        frame.write(op.writes[0], Term::List(front, Box::new(tail)));
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

        assert!(fun_ident.arity as usize == args.len());

        let fun_opt = module.functions.iter().find(|fun| &fun.ident == fun_ident);
        if fun_opt.is_none() {
            panic!("function {} not found in module {:?}", fun_ident, module.name);
        }
        let fun = fun_opt.unwrap();

        let lir = fun.lir_function.as_ref().unwrap();

        let mut frame = StackFrame::new();

        // Insert arguments into frame
        assert!(fun.lir_function.as_ref().unwrap().args.len() == args.len());
        for (var_def, term) in fun.lir_function.as_ref().unwrap().args.iter().zip(args) {
            frame.write(*var_def, term.clone());
        }

        // Execute blocks
        let mut prev_block_id: Option<LabelN> = None;
        let mut curr_block_id = lir.entry();
        loop {
            println!("Executing {}", curr_block_id);

            let block = lir.block(curr_block_id);
            let slots = lir.branch_slots(curr_block_id);

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
        println!("Call native: {}:{}", module.name, fun_ident);
        assert!(args.len() == fun_ident.arity as usize);
        // bad
        let fun_name_str: &str = fun_ident.name.as_str();
        module.functions[&(fun_name_str.to_string(), fun_ident.arity)](args)
    }

    pub fn call(&self, module_name: &str, fun_name: &str, args: &[Term])
                -> CallReturn {
        self.call_base(module_name, &Atom::from_str(fun_name), None, args)
    }

    pub fn call_base(&self, module_name: &str, fun_name: &Atom, lambda: Option<(LambdaEnvIdx, usize)>, args: &[Term]) -> CallReturn {

        let fun_ident = FunctionIdent {
            name: fun_name.clone(),
            arity: args.len() as u32,
            lambda: lambda,
        };

        println!("-> {}:{} {:?}", module_name, fun_ident, args);

        if !self.modules.contains_key(module_name) {
            panic!("module {:?} not found", module_name);
        }
        let module = &self.modules[module_name];

        let ret = match module {
            ModuleType::Erlang(ref erl_module, ref native_overlay) => {
                if let Some(ref overlay) = native_overlay {
                    if overlay.has_fun(&fun_ident) {
                        self.call_native_module(overlay, &fun_ident, args)
                    } else {
                        self.call_erlang_module(erl_module, &fun_ident, args)
                    }
                } else {
                    self.call_erlang_module(erl_module, &fun_ident, args)
                }
            }
            ModuleType::Native(ref native_module) => {
                assert!(lambda.is_none()); // No lambdas in native modules
                self.call_native_module(native_module, &fun_ident, args)
            }
        };

        println!("<- {}:{}", module_name, fun_ident);

        ret
    }

}
