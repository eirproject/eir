mod beam_data_structures;

mod types;
pub use types::{ NifTypes };

mod export;

mod emit;

use eir::FunctionIdent;
use eir::Function as EirFunction;
use eir::AttributeKey;

use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::values::{ FunctionValue, BasicValueEnum };

use crate::{ Data };

pub struct NifTarget {
    types: NifTypes,
    env: Option<BasicValueEnum>,
}

impl super::Target for NifTarget {
    type Options = ();

    fn new(context: Context, module: &Module, options: ()) -> Self {
        NifTarget {
            types: NifTypes::new(&context, &module),
            env: None,
        }
    }

    // TODO: For simplicity, all functions take 3 shadow arguments right now,
    // env, term, term.
    // For the three different function types these are as follows:
    // * Function: null, ok_cont, err_cont
    // * Closure: env, ok_cont, err_cont
    // * Continuation: env, null, null
    fn gen_prototype(
        &mut self,
        data: &mut Data,
        fun: &EirFunction
    ) -> FunctionValue
    {
        let entry_ebb = fun.ebb_entry();
        let arity = fun.ebb_args(entry_ebb).len();
        let is_cont = fun.has_attribute(&AttributeKey::Continuation);
        let ident = fun.ident();

        let mut args = vec![
            self.types.env_ptr_type.into(),
        ];
        if ident.lambda.is_none() {
            assert!(!is_cont);
            args.push(self.types.term_type); // Dummy env
        }
        args.extend((0..arity).map(|_| self.types.term_type));

        let fn_type = data.context.void_type().fn_type(&args, false);

        let ident_mangled = crate::emit::mangle_ident(fun.ident());
        data.module.add_function(&ident_mangled, fn_type, None)

    }

    fn gen_prototype_pub(
        &mut self,
        data: &mut Data,
        ident: &FunctionIdent
    ) -> FunctionValue
    {
        assert!(ident.lambda.is_none());
        let mut args = vec![
            self.types.env_ptr_type.into(),
            self.types.term_type.into(),
            self.types.term_type.into(),
            self.types.term_type.into(),
        ];
        args.extend((0..ident.arity).map(|_n| self.types.term_type));

        let fn_type = data.context.void_type().fn_type(&args, false);

        let ident_mangled = crate::emit::mangle_ident(ident);
        data.module.add_function(&ident_mangled, fn_type, None)
    }

    fn finalize(
        &mut self,
        data: &mut Data,
    ) {
    }

}


