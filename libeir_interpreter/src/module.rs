use std::collections::HashMap;
use std::rc::Rc;

use crate::{ VMState, Term };
use crate::process::{ ProcessContext };

use libeir_intern::Symbol;
use libeir_ir::{ Module, Function, FunctionIdent, LiveValues };

pub enum NativeReturn {
    Return {
        term: Rc<Term>,
    },
    Throw {
        typ: Rc<Term>,
        reason: Rc<Term>,
    },
}

pub struct NativeModule {
    pub name: Symbol,
    pub functions: HashMap<(Symbol, usize),
                           Box<dyn Fn(&VMState, &mut ProcessContext, &[Rc<Term>]) -> NativeReturn>>,
}
impl NativeModule {

    pub fn new(name: Symbol) -> Self {
        NativeModule {
            name: name,
            functions: HashMap::new(),
        }
    }

    pub fn add_fun(&mut self, name: Symbol, arity: usize,
               fun: Box<dyn Fn(&VMState, &mut ProcessContext, &[Rc<Term>]) -> NativeReturn>) {
        self.functions.insert((name, arity), fun);
    }

    pub fn has_fun(&self, ident: &FunctionIdent) -> bool {
        self.functions.contains_key(&(ident.name.name, ident.arity))
    }

}

pub struct ErlangFunction {
    pub fun: Function,
    pub live: LiveValues,
}

pub struct ErlangModule {
    pub name: Symbol,
    pub functions: HashMap<FunctionIdent, ErlangFunction>,
}

impl ErlangModule {

    pub fn from_eir(module: Module) -> Self {
        let functions = module.functions.values().map(|fun| {
            let nfun = ErlangFunction {
                live: fun.live_values(),
                fun: fun.clone(),
            };
            (fun.ident().clone(), nfun)
        }).collect();
        ErlangModule {
            name: module.name.name,
            functions,
        }
    }

}

pub enum ModuleType {
    Erlang(ErlangModule, Option<NativeModule>),
    Native(NativeModule),
}
