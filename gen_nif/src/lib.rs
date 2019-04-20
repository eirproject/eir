use std::collections::HashMap;

use eir::{ Module as EirModule };
use eir::FunctionIdent;
use eir::Function as EirFunction;
use eir::Dialect;

use inkwell::context::Context;
use inkwell::values::FunctionValue;
use inkwell::module::Module;

mod primitives;
mod emit;
mod nif_types;
pub mod target;

use ::std::path::Path;

#[derive(Debug, Clone)]
pub enum CallType {
    /// Takes an environment, arg*arity
    Function { arity: usize },
    /// Takes an environment, return term
    Continuation,
}

#[derive(Debug, Clone)]
pub struct CallSig {
    ident: FunctionIdent,
    typ: CallType,
}

pub struct Data {
    context: Context,
    module: Module,
    protos: HashMap<FunctionIdent, FunctionValue>,
    loc_id: u64,
}

pub struct CompilationContext<Target> {
    data: Data,
    target: Target,
}

impl<Target> CompilationContext<Target> where Target: target::Target {

    pub fn new(llvm_module_name: &str, target_options: Target::Options) -> Self {
        let context = Context::create();
        let module = context.create_module(llvm_module_name);
        let target = Target::new(context.clone(), &module, target_options);
        CompilationContext {
            data: Data {
                context: context,
                module: module,
                protos: HashMap::new(),
                loc_id: 1,
            },
            target: target,
        }
    }

    pub fn inner_mut<'a>(&'a mut self) -> (&'a mut Data, &'a mut Target) {
        (&mut self.data, &mut self.target)
    }

    pub fn gen_proto(&mut self, fun: &FunctionIdent) {
        if fun.lambda.is_none() && !self.data.protos.contains_key(fun) {
            let val = self.target.gen_prototype_pub(&mut self.data, fun);
            self.data.protos.insert(fun.clone(), val);
        }
    }

    pub fn get_fun_value(&mut self, fun: &EirFunction) -> FunctionValue {
        let ident = fun.ident();
        if let Some(val) = self.data.protos.get(ident) {
            *val
        } else {
            let fun = self.target.gen_prototype(&mut self.data, fun);
            self.data.protos.insert(ident.clone(), fun);
            fun
        }
    }

    pub fn build_function(
        &mut self,
        module: &EirModule,
        name: &str,
        arity: usize
    ) -> bool
    {
        println!("{}/{}", name, arity);

        let funs: Vec<_> = module.functions.keys()
            .filter(|i| i.name.as_str() == name && i.arity == arity)
            .collect();

        for ident in funs.iter() {
            println!("LF: {}", ident);
            let fun = &module.functions[ident];
            assert!(fun.dialect() == Dialect::CPS);
            self.get_fun_value(fun);
        }

        for ident in funs.iter() {
            let fun = &module.functions[ident];
            let fn_val = self.get_fun_value(fun);

            crate::emit::emit_fun(
                &mut self.target,
                &self.data.context,
                &self.data.module,
                &mut self.data.protos,
                &mut self.data.loc_id,
                &module,
                fun,
                fn_val,
            );
        }

        true
    }

    pub fn print_ir(&self) {
        self.data.module.print_to_stderr();
    }

    pub fn finalize(&mut self) {
        self.target.finalize(&mut self.data);
    }

    pub fn write_bitcode(&self, path: &Path) {
        self.print_ir();
        println!("{:?}", self.data.module.verify());
        self.data.module.write_bitcode_to_path(&path);
    }

}
