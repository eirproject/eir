use std::collections::HashMap;
use std::cell::RefCell;
use std::rc::Rc;

use crate::module::{ NativeModule, ModuleType, ErlangModule };
use crate::process::{ ProcessContext, CallExecutor, Continuation, TermCall };
use crate::term::{ Term, Pid, Reference };

use libeir_ir::{ Module, FunctionIdent };
use libeir_intern::Symbol;

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub enum WatchType {
    Link,
    Monitor(Reference),
}

#[derive(Debug)]
pub struct ReferenceGenerator(Reference);
impl ReferenceGenerator {

    fn new() -> Self {
        ReferenceGenerator(Reference(0))
    }

    pub fn next(&mut self) -> Reference {
        let r = self.0;
        (self.0).0 += 1;
        r
    }

}

pub struct VMState {
    pub modules: HashMap<Symbol, ModuleType>,
    pub processes: RefCell<Vec<Rc<RefCell<ProcessContext>>>>,

    pub ref_gen: RefCell<ReferenceGenerator>,

    // Hashmap of all watches a process has placed on it.
    //pub watches: RefCell<HashMap<Pid, Vec<(Pid, WatchType)>>>,

    //pub mailboxes: RefCell<HashMap<Pid, ::mailbox::Mailbox>>,
}

impl VMState {

    pub fn new() -> Self {
        VMState {
            modules: HashMap::new(),
            processes: RefCell::new(Vec::new()),
            ref_gen: RefCell::new(ReferenceGenerator::new()),
            //watches: RefCell::new(HashMap::new()),
            //mailboxes: RefCell::new(HashMap::new()),
        }
    }

    pub fn add_erlang_module(&mut self, module: Module) {
        let erl_mod = ErlangModule::from_eir(module);
        self.modules.insert(erl_mod.name, ModuleType::Erlang(erl_mod, None));
    }

    pub fn add_native_module(&mut self, module: NativeModule) {
        self.modules.insert(module.name, ModuleType::Native(module));
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

    pub fn add_builtin_modules(&mut self) {
        self.add_native_module(crate::erl_lib::make_erlang());
        self.add_native_module(crate::erl_lib::make_lists());
        self.add_native_module(crate::erl_lib::make_math());
    }

    pub fn call(&mut self, fun: &FunctionIdent, args: &[Term]) -> Result<Rc<Term>, (Rc<Term>, Rc<Term>, Rc<Term>)> {
        let self_pid = {
            let processes = self.processes.borrow();
            Pid(processes.len())
        };

        let mut process = ProcessContext::new(self_pid);

        let fun_term = Term::CapturedFunction {
            ident: fun.clone(),
        };

        let mut n_args = Vec::new();
        n_args.push(Term::ReturnOk.into());
        n_args.push(Term::ReturnThrow.into());
        n_args.extend(args.iter().cloned().map(|v| v.into()));

        let mut continuation = TermCall {
            fun: fun_term.into(),
            args: n_args,
        };

        let mut executor = CallExecutor::new();
        loop {
            match executor.run(self, &mut process, continuation) {
                Continuation::Term(call) => continuation = call,
                Continuation::ReturnOk(ret) => return Ok(ret),
                Continuation::ReturnThrow(r1, r2, r3) => return Err((r1, r2, r3)),
            }
        }

    }

    //pub fn call(&mut self, module_name: &str, fun_name: &str, args: Vec<Term>)
    //            -> CallReturn {
    //    let fun_ident = FunctionIdent {
    //        module: Atom::from_str(module_name),
    //        name: Atom::from_str(fun_name),
    //        arity: args.len(),
    //        lambda: None,
    //    };

    //    let self_pid = {
    //        let processes = self.processes.borrow();
    //        Pid(processes.len())
    //    };
    //    ::trace::set_pid(self_pid);

    //    let process = ProcessContext::new(self_pid);

    //    let frame = process.make_call_stackframe(
    //        self,
    //        Atom::from_str(module_name),
    //        fun_ident,
    //        args
    //    );

    //    {
    //        let mut processes = self.processes.borrow_mut();
    //        {
    //            let mut stack = process.stack.borrow_mut();
    //            stack.push(frame);
    //        }
    //        processes.push(Rc::new(RefCell::new(process)));
    //    }
    //    {
    //        let mut mailboxes = self.mailboxes.borrow_mut();
    //        mailboxes.insert(self_pid, ::mailbox::Mailbox::new());
    //    }

    //    loop {
    //        let processes_len = self.processes.borrow().len();
    //        for process_num in 0..processes_len {
    //            println!("=====================================");
    //            println!("======== SWITCH TO PROCESS {} ========", process_num);
    //            println!("=====================================");
    //            ::trace::set_pid(Pid(process_num));

    //            let process_rc = self.processes.borrow()[process_num].clone();
    //            let mut process = process_rc.borrow_mut();
    //            process.run_reductions(self, 4000);
    //        }

    //        let processes_borrow = self.processes.borrow();
    //        let process_borrow = processes_borrow[self_pid.0].borrow();
    //        if process_borrow.stack.borrow().len() == 0 {
    //            break;
    //        }
    //    }

    //    let processes_borrow = self.processes.borrow();
    //    let mut process = processes_borrow[self_pid.0].borrow_mut();
    //    process.return_val.take().unwrap()
    //}

}
