use std::collections::HashMap;
use std::cell::RefCell;
use std::rc::Rc;

use ::{ Atom, Module, FunctionIdent };
use ::module::{ NativeModule, ModuleType };
use ::process::{ ProcessContext, CallReturn };
use ::term::{ Term, Pid, Reference };

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
    pub modules: HashMap<String, ModuleType>,
    pub processes: RefCell<Vec<Rc<RefCell<ProcessContext>>>>,

    pub ref_gen: RefCell<ReferenceGenerator>,

    /// Hashmap of all watches a process has placed on it.
    pub watches: RefCell<HashMap<Pid, Vec<(Pid, WatchType)>>>,
}

impl VMState {

    pub fn new() -> Self {
        VMState {
            modules: HashMap::new(),
            processes: RefCell::new(Vec::new()),
            ref_gen: RefCell::new(ReferenceGenerator::new()),
            watches: RefCell::new(HashMap::new()),
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

        let self_pid = {
            let processes = self.processes.borrow();
            Pid(processes.len())
        };

        let mut process = ProcessContext::new(self_pid);

        let frame = process.make_call_stackframe(
            self,
            Atom::from_str(module_name),
            fun_ident,
            args
        );

        {
            let mut processes = self.processes.borrow_mut();
            {
                let mut stack = process.stack.borrow_mut();
                stack.push(frame);
            }
            processes.push(Rc::new(RefCell::new(process)));
        }

        loop {
            let processes_len = self.processes.borrow().len();
            for process_num in 0..processes_len {
                println!("=====================================");
                println!("======== SWITCH TO PROCESS {} ========", process_num);
                println!("=====================================");

                let process_rc = self.processes.borrow()[process_num].clone();
                let mut process = process_rc.borrow_mut();
                process.run_reductions(self, 4000);
            }

            let processes_borrow = self.processes.borrow();
            let process_borrow = processes_borrow[self_pid.0].borrow();
            if process_borrow.stack.borrow().len() == 0 {
                break;
            }
        }

        let processes_borrow = self.processes.borrow();
        let mut process = processes_borrow[self_pid.0].borrow_mut();
        process.return_val.take().unwrap()
    }

}
