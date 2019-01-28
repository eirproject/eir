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
use pattern::{ CaseContext };

pub mod erl_lib;
#[cfg(test)] pub mod erl_tests;

pub struct NativeModule {
    name: String,
    functions: HashMap<(String, u32), Box<Fn(&VMState, &[Term]) -> CallReturn>>,
}
impl NativeModule {

    fn new(name: String) -> Self {
        NativeModule {
            name: name,
            functions: HashMap::new(),
        }
    }

    fn add_fun(&mut self, name: String, arity: u32,
               fun: Box<Fn(&VMState, &[Term]) -> CallReturn>) {
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

pub enum ModuleType {
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

#[derive(Copy, Clone, PartialEq, Eq)]
enum StackFrameState {
    Normal,
    InCall(CallOutcomes),
}

#[derive(Copy, Clone, PartialEq, Eq, Debug)]
struct CallOutcomes {
    ret_ok_ssa: SSAVariable,
    ret_ok_slot: usize,
    ret_ok_label: Option<LabelN>,
    ret_throw_ssa: SSAVariable,
    ret_throw_slot: usize,
    ret_throw_label: Option<LabelN>,
}

struct NativeStackFrame {
    module: Atom,
    fun_ident: FunctionIdent,
    args: Vec<Term>,
}

enum StackFrameType {
    Erlang(StackFrame),
    Native(NativeStackFrame),
}

struct StackFrame {
    variables: HashMap<SSAVariable, Term>,
    state: StackFrameState,
    module: Atom,
    function: FunctionIdent,
    basic_block: LabelN,
    prev_basic_block: Option<LabelN>,
}
impl StackFrame {

    fn new(module: Atom, function: FunctionIdent, label: LabelN) -> Self {
        StackFrame {
            variables: HashMap::new(),
            state: StackFrameState::Normal,
            function: function,
            basic_block: label,
            module: module,
            prev_basic_block: None,
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

    fn exec_block(&mut self, module: &Module, block: &BasicBlock) -> BlockResult {

        // Apply phi nodes
        for phi in &block.phi_nodes {
            let source = &phi.entries.iter()
                .find(|(label, _)| label == self.prev_basic_block.as_ref().unwrap())
                .unwrap().1;
            let term = self.read(source);
            self.write(phi.ssa, term);
        }

        let mut block_ret: Option<BlockResult> = None;
        for op in &block.ops {
            assert!(block_ret.is_none());
            match op.kind {
                OpKind::Arguments => {
                    assert!(op.reads.len() == 0);
                    assert!(op.writes.iter().all(|w| self.variables.contains_key(w)))
                }
                OpKind::UnpackValueList => {
                    assert!(op.reads.len() == 1);
                    let res = self.read(&op.reads[0]);
                    let unpacked = unpack_value_list(res);
                    assert!(unpacked.len() == op.writes.len());
                    for (var, val) in op.writes.iter().zip(unpacked) {
                        self.write(*var, val);
                    }
                }
                OpKind::UnpackEnv => {
                    assert!(op.reads.len() == 1);
                    let env = self.read(&op.reads[0]);
                    assert!(env.get_type() == TermType::LambdaEnv);
                    if let Term::LambdaEnv(data) = env {
                        assert!(data.vars.len() == op.writes.len());
                        for (var, term) in op.writes.iter().zip(data.vars) {
                            self.write(*var, term);
                        }
                    }
                }
                OpKind::PackValueList => {
                    assert!(op.writes.len() == 1);
                    let reads_val: Vec<_> = op.reads.iter()
                        .map(|r| self.read(r))
                        .collect();
                    self.write(op.writes[0], Term::ValueList(reads_val));
                }
                OpKind::Move => {
                    assert!(op.reads.len() == 1);
                    assert!(op.writes.len() == 1);
                    let res = self.read(&op.reads[0]);
                    self.write(op.writes[0], res);
                }
                OpKind::Call => {
                    assert!(op.reads.len() >= 2);
                    assert!(op.writes.len() == 2);

                    let module_term = self.read(&op.reads[0]);
                    let fun_term = self.read(&op.reads[1]);
                    let args: Vec<Term> = op.reads[2..].iter()
                        .map(|arg| self.read(arg)).collect();

                    let module_atom = module_term.as_atom().unwrap();
                    let fun_atom = fun_term.as_atom().unwrap();

                    let outcomes = CallOutcomes {
                        ret_ok_slot: 0,
                        ret_ok_ssa: op.writes[0],
                        ret_ok_label: None,
                        ret_throw_slot: 1,
                        ret_throw_ssa: op.writes[1],
                        ret_throw_label: None,
                    };

                    block_ret = Some(BlockResult::Call {
                        module: module_atom,
                        fun: fun_atom,
                        lambda: None,
                        args: args,
                        outcomes: outcomes,
                    });
                }
                OpKind::ReturnOk => {
                    assert!(op.reads.len() == 1);
                    assert!(op.writes.len() == 0);
                    block_ret = Some(BlockResult::Return {
                        ret: CallReturn::Return { term: self.read(&op.reads[0]) },
                    });
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
                    self.write(op.writes[0], res);
                }
                OpKind::Apply => {
                    assert!(op.writes.len() == 2);
                    assert!(op.reads.len() >= 1);

                    let fun_var = self.read(&op.reads[0]);
                    let args: Vec<Term> = op.reads[1..].iter()
                        .map(|arg| self.read(arg)).collect();

                    let outcomes = CallOutcomes {
                        ret_ok_slot: 0,
                        ret_ok_ssa: op.writes[0],
                        ret_ok_label: None,
                        ret_throw_slot: 1,
                        ret_throw_ssa: op.writes[1],
                        ret_throw_label: None,
                    };

                    match fun_var {
                        Term::CapturedFunction { module: ref module_a,
                                                 ref fun_name, ref arity } => {
                            assert!(args.len() == *arity as usize);

                            block_ret = Some(BlockResult::Call {
                                module: module_a.clone(),
                                fun: fun_name.clone(),
                                lambda: None,
                                args: args,
                                outcomes: outcomes,
                            });
                        }
                        Term::BoundLambda {
                            module: ref module_a, ref fun_name, arity,
                            lambda, ref bound_env } => {

                            let mut rargs = vec![Term::LambdaEnv(bound_env.clone())];
                            rargs.extend(args.iter().cloned());

                            assert!(rargs.len() == (arity as usize));

                            block_ret = Some(BlockResult::Call {
                                module: module_a.clone(),
                                fun: fun_name.clone(),
                                lambda: Some(lambda),
                                args: rargs,
                                outcomes: outcomes,
                            });
                        }
                        _ => panic!("Var is not callable"),
                    }
                }
                OpKind::MakeClosureEnv { ref env_idx } => {
                    assert!(op.reads.len() > 0);
                    assert!(op.writes.len() == 1);

                    let env_vars: Vec<Term> = op.reads.iter()
                        .map(|r| self.read(r))
                        .collect();

                    self.write(op.writes[0], Term::LambdaEnv(BoundLambdaEnv {
                        env: *env_idx,
                        vars: env_vars,
                    }));
                }
                OpKind::BindClosure { ref ident } => {
                    assert!(op.reads.len() == 1);
                    assert!(op.writes.len() == 1);

                    let lenv = if let Term::LambdaEnv(bound_env)
                        = self.read(&op.reads[0]) {
                        bound_env
                    } else {
                        panic!();
                    };

                    self.write(op.writes[0], Term::BoundLambda {
                        module: module.name.clone(),
                        fun_name: ident.name.clone(),
                        arity: ident.arity,
                        lambda: ident.lambda.unwrap(),
                        bound_env: lenv.clone(),
                    });
                }
                OpKind::CaseStart { ref clauses, .. } => {
                    let term = self.read(&op.reads[0]);
                    let vals = unpack_value_list(term);
                    let case_ctx = CaseContext::new(vals, clauses.clone());

                    self.write(op.writes[0], Term::CaseContext(
                        Rc::new(RefCell::new(case_ctx))));
                }
                OpKind::Case(_) => {
                    let to_leaf = {
                        let curr = self.read(&op.reads[0]);
                        let mut ctx = if let Term::CaseContext(ref ctx) = curr {
                            ctx.borrow_mut()
                        } else {
                            panic!("Case read not case context");
                        };

                        ctx.do_body()
                    };
                    block_ret = Some(BlockResult::Branch { slot: to_leaf });
                }
                OpKind::CaseGuardFail { clause_num } => {
                    let curr = self.read(&op.reads[0]);
                    let mut ctx = if let Term::CaseContext(ref ctx) = curr {
                        ctx.borrow_mut()
                    } else {
                        panic!("Case read not case context");
                    };

                    ctx.guard_fail(clause_num);
                }
                OpKind::CaseValues => {
                    let mut vals = {
                        let curr = self.read(&op.reads[0]);
                        let mut ctx = if let Term::CaseContext(ref ctx) = curr {
                            ctx.borrow_mut()
                        } else {
                            panic!("case read not case context");
                        };

                        ctx.case_values()
                    };
                    for write in &op.writes {
                        self.write(*write, vals.remove(write).unwrap());
                    }
                }
                OpKind::CaseGuardOk => {
                    let curr = self.read(&op.reads[0]);
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
                    let matched = match self.read(&op.reads[0]) {
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
                    self.tombstone(ssa);
                }
                OpKind::MakeTuple => {
                    let term = Term::Tuple(
                        op.reads.iter().map(|r| self.read(r)).collect()
                    );
                    self.write(op.writes[0], term);
                }
                OpKind::MakeList => {
                    assert!(op.reads.len() >= 1);
                    assert!(op.writes.len() == 1);
                    let tail = self.read(&op.reads[0]);
                    let mut front: Vec<_> = op.reads.iter()
                        .skip(1)
                        .map(|r| self.read(r))
                        .collect();

                    // Just merge lists for prettyness
                    if let Term::List(i_head, i_tail) = tail {
                        front.extend(i_head);
                        self.write(op.writes[0], Term::List(front, i_tail));
                    } else {
                        self.write(op.writes[0], Term::List(front, Box::new(tail)));
                    }
                }
                _ => {
                    println!("Unimpl: {:?}", op);
                    println!("Variables: {:?}", self.variables);
                    unimplemented!()
                }
            }
        }

        return block_ret.unwrap();
    }

}

pub struct VMState {
    modules: HashMap<String, ModuleType>,
    processes: Vec<Rc<RefCell<ProcessContext>>>,
}

impl VMState {

    pub fn new() -> Self {
        VMState {
            modules: HashMap::new(),
            processes: Vec::new(),
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

    pub fn call(&mut self, module_name: &str, fun_name: &str, args: Vec<Term>)
                -> CallReturn {
        let fun_ident = FunctionIdent {
            name: Atom::from_str(fun_name),
            arity: args.len() as u32,
            lambda: None,
        };

        let mut process = ProcessContext::new();

        let frame = process.make_call_stackframe(
            self,
            Atom::from_str(module_name),
            fun_ident,
            args
        );

        let self_pid = self.processes.len();
        process.stack.push(frame);
        self.processes.push(Rc::new(RefCell::new(process)));

        loop {
            for process_num in 0..(self.processes.len()) {
                println!("=====================================");
                println!("======== SWITCH TO PROCESS {} ========", process_num);
                println!("=====================================");

                let process_rc = self.processes[process_num].clone();
                let mut process = process_rc.borrow_mut();
                process.run_reductions(self, 4000);
            }

            if self.processes[self_pid].borrow().stack.len() == 0 {
                break;
            }
        }

        let mut process = self.processes[self_pid].borrow_mut();
        process.return_val.take().unwrap()
    }

}

#[derive(Clone, Debug)]
enum BlockResult {
    Branch { slot: usize },
    Return { ret: CallReturn },
    Call {
        module: Atom,
        fun: Atom,
        args: Vec<Term>,
        lambda: Option<(LambdaEnvIdx, usize)>,
        outcomes: CallOutcomes,
    },
}

pub struct ProcessContext {
    stack: Vec<StackFrameType>,
    return_val: Option<CallReturn>,
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

impl ProcessContext {

    pub fn new() -> Self {
        ProcessContext {
            stack: Vec::new(),
            return_val: None,
        }
    }

    fn make_call_stackframe(&self, vm: &VMState,
                            module: Atom, fun_ident: FunctionIdent,
                            args: Vec<Term>) -> StackFrameType {

        assert!(vm.modules.contains_key(module.as_str()));
        let module_t = vm.modules.get(module.as_str()).unwrap();

        assert!(fun_ident.arity == args.len() as u32);

        println!("-> {}:{}", module, fun_ident);

        match module_t {
            ModuleType::Erlang(c_module, native_overlay_opt) => {
                if let Some(native_overlay) = native_overlay_opt {
                    if native_overlay.functions.contains_key(&(
                        fun_ident.name.as_str().to_string(),
                        fun_ident.arity as u32
                    )) {
                        let native_frame = NativeStackFrame {
                            module,
                            fun_ident,
                            args,
                        };
                        return StackFrameType::Native(native_frame);
                    }
                }

                let fun = c_module.functions.iter()
                    .find(|f| f.ident == fun_ident)
                    .unwrap();
                let c_lir = fun.lir_function.as_ref().unwrap();

                let mut call_frame = StackFrame::new(
                    module.clone(),
                    fun_ident.clone(),
                    c_lir.entry()
                );

                // Insert arguments into frame
                assert!(c_lir.args.len() == args.len());
                for (var_def, term) in c_lir.args.iter().zip(args) {
                    call_frame.write(*var_def, term.clone());
                }

                StackFrameType::Erlang(call_frame)
            }
            ModuleType::Native(native) => {
                if native.functions.contains_key(&(
                    fun_ident.name.as_str().to_string(),
                    fun_ident.arity as u32
                )) {
                    let native_frame = NativeStackFrame {
                        module,
                        fun_ident,
                        args,
                    };
                    StackFrameType::Native(native_frame)
                } else {
                    panic!()
                }
            }
        }
    }

    // In our case we count a single basic block as a reduction.
    // This should be a decent way to do things for a reference
    // implementation.
    pub fn do_reduction(&mut self, vm: &VMState) {
        let mut push_frame_parts: Option<(Atom, FunctionIdent, Vec<Term>)> = None;
        let mut pop_frame = false;

        if self.stack.len() == 0 {
            return;
        }

        {
            let frame = self.stack.last_mut().unwrap();
            match frame {
                StackFrameType::Erlang(frame) => {

                    // If we were in a call, handle result
                    if self.return_val.is_some() {
                        let ret = self.return_val.take().unwrap();
                        if let StackFrameState::InCall(outcomes) = frame.state {
                            match ret {
                                CallReturn::Return { term } => {
                                    frame.prev_basic_block = Some(frame.basic_block);
                                    frame.basic_block = outcomes.ret_ok_label.unwrap();
                                    frame.write(outcomes.ret_ok_ssa, term);
                                    println!("Branching to slot {}", outcomes.ret_ok_slot);
                                }
                                CallReturn::Throw => {
                                    frame.prev_basic_block = Some(frame.basic_block);
                                    frame.basic_block = outcomes.ret_throw_label.unwrap();
                                    // TODO TODO
                                    let replacement_term = Term::ValueList(
                                        vec![Term::Nil, Term::Nil]
                                    );
                                    frame.write(outcomes.ret_throw_ssa, replacement_term);
                                    println!("Branching to slot {}", outcomes.ret_throw_slot);
                                }
                            }
                            frame.state = StackFrameState::Normal;
                        } else {
                            panic!();
                        }
                    } else {
                        assert!(frame.state == StackFrameState::Normal);
                    }

                    let curr_block_id = frame.basic_block;

                    assert!(vm.modules.contains_key(frame.module.as_str()));
                    let module_t = vm.modules.get(frame.module.as_str()).unwrap();

                    if let ModuleType::Erlang(ref module, _native_overlay_opt) = module_t {

                        let fun = module.functions.iter()
                            .find(|fun| fun.ident == frame.function)
                            .unwrap();
                        let lir = fun.lir_function.as_ref().unwrap();
                        let block = lir.block(frame.basic_block);

                        match frame.exec_block(module, block) {
                            BlockResult::Branch { slot } => {
                                let slots = lir.branch_slots(curr_block_id);
                                frame.prev_basic_block = Some(curr_block_id);
                                frame.basic_block = slots[slot];
                                println!("Branching to slot {}", slot);
                            }
                            BlockResult::Call { module, fun, args, lambda, mut outcomes } => {
                                let slots = lir.branch_slots(curr_block_id);
                                outcomes.ret_ok_label = Some(slots[outcomes.ret_ok_slot]);
                                outcomes.ret_throw_label = Some(slots[outcomes.ret_throw_slot]);

                                let ident = FunctionIdent {
                                    name: fun,
                                    arity: args.len() as u32,
                                    lambda: lambda,
                                };
                                push_frame_parts = Some((module, ident, args));
                                frame.state = StackFrameState::InCall(outcomes);
                            }
                            BlockResult::Return { ret } => {
                                self.return_val = Some(ret);
                                pop_frame = true;
                                println!("<- {}:{}", module.name, frame.function);
                            }
                        }

                    } else {
                        unreachable!()
                    }

                }
                StackFrameType::Native(frame) => {
                    assert!(vm.modules.contains_key(frame.module.as_str()));
                    let module_t = vm.modules.get(frame.module.as_str()).unwrap();

                    match module_t {
                        ModuleType::Native(module) => {
                            let fun = &module.functions[&(
                                frame.fun_ident.name.as_str().to_string(),
                                frame.fun_ident.arity as u32
                            )];
                            self.return_val = Some((fun)(vm, &frame.args));
                            pop_frame = true;
                            println!("<- {}:{}", module.name, frame.fun_ident);
                        }
                        ModuleType::Erlang(_mod, Some(module)) => {
                            let fun = &module.functions[&(
                                frame.fun_ident.name.as_str().to_string(),
                                frame.fun_ident.arity as u32
                            )];
                            self.return_val = Some((fun)(vm, &frame.args));
                            pop_frame = true;
                            println!("<- {}:{}", module.name, frame.fun_ident);
                        }
                        _ => unreachable!(),
                    }
                }
            }
        }

        if push_frame_parts.is_some() {
            let (module, ident, args) = push_frame_parts.take().unwrap();
            let frame = self.make_call_stackframe(vm, module, ident, args);
            self.stack.push(frame);
        }
        if pop_frame {
            self.stack.pop();
        }

    }

    pub fn run_reductions(&mut self, vm: &mut VMState, reductions: u64) {
        let reduction_counter = 0;
        while reduction_counter < reductions && self.stack.len() > 0 {
            self.do_reduction(vm);
        }
    }

}
