use std::collections::HashMap;
use ::{ VMState, Term, FunctionIdent };
use ::process::{ ProcessContext, CallReturn };
use ::eir::Module;

pub struct NativeModule {
    pub name: String,
    pub functions: HashMap<(String, usize), Box<Fn(&VMState, &mut ProcessContext,
                                                   &[Term]) -> CallReturn>>,
}
impl NativeModule {

    pub fn new(name: String) -> Self {
        NativeModule {
            name: name,
            functions: HashMap::new(),
        }
    }

    pub fn add_fun(&mut self, name: String, arity: usize,
               fun: Box<Fn(&VMState, &mut ProcessContext, &[Term]) -> CallReturn>) {
        self.functions.insert((name, arity), fun);
    }

    pub fn has_fun(&self, ident: &FunctionIdent) -> bool {
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
