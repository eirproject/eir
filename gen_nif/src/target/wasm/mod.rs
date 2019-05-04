mod emit;
mod types;
pub use types::WasmTypes;

use eir::FunctionIdent;
use eir::Function as EirFunction;
use eir::Atom;
use eir::AttributeKey;

use inkwell::AddressSpace;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::values::{ FunctionValue, BasicValueEnum, PointerValue };

use crate::{ Data };

pub struct WasmTarget {
    context: Context,
    types: types::WasmTypes,
    env: Option<BasicValueEnum>,
    init_fun: FunctionValue,
    options: WasmTargetOptions,
}

pub struct WasmTargetOptions {
    pub eir_module: String,
}

impl WasmTarget {

    pub fn get_atom(&mut self, module: &Module, atom: &Atom) -> PointerValue {
        if let Some(glob) = self.types.static_atom_table.get(atom) {
            glob.as_pointer_value()
        } else {
            // TODO: Fix name mangling scheme
            let atom_global_name = format!("whirlc_module_{}_atom_{}",
                                           self.options.eir_module,
                                           atom);
            let glob = module.add_global(
                self.types.term_type,
                Some(AddressSpace::Generic),
                &atom_global_name,
            );
            self.types.static_atom_table.insert(atom.clone(), glob);

            let u64_type = self.context.i64_type();
            let const_0 = u64_type.const_int(0, false);
            glob.set_initializer(&const_0);

            glob.as_pointer_value()
        }
    }

}

impl super::Target for WasmTarget {
    type Options = WasmTargetOptions;

    fn new(context: Context, module: &Module, options: WasmTargetOptions) -> Self {
        let types = types::WasmTypes::new(&context, &module);

        let init_fun_name = format!("whirlc_module_init_{}", options.eir_module);
        let init_fun_type = types.void_type.fn_type(&[], false);
        let init_fun = module.add_function(
            &init_fun_name,
            init_fun_type,
            None,
        );

        WasmTarget {
            context: context,
            types: types,
            env: None,
            init_fun: init_fun,
            options: options,
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
            self.types.process_env_ptr_type.into(),
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
            self.types.process_env_ptr_type.into(),
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
        let i8_ptr_type = self.context.i8_type().ptr_type(AddressSpace::Generic);
        let i32_type = self.context.i32_type();

        // Generate module init function
        let fun = self.init_fun;
        let builder = self.context.create_builder();

        let entry_bb = self.context.append_basic_block(&fun, "entry");
        builder.position_at_end(&entry_bb);

        for (atom, glob) in self.types.static_atom_table.iter() {
            let atom_str = crate::primitives::make_c_string_const(
                &self.context, &data.module, atom.as_str());
            // TODO: UTF8 length incorrect!!
            let str_len = i32_type.const_int(atom.as_str().len() as u64, false);
            let call = builder.build_call(
                self.types.whirlrt_term_make_atom_from_string,
                &[
                    str_len.into(),
                    atom_str.as_pointer_value().const_cast(i8_ptr_type).into(),
                ],
                "init_atom",
            );
            let call_val = call.try_as_basic_value().left().unwrap();
            builder.build_store(glob.as_pointer_value(), call_val);
        }

        for (ident, fun) in data.protos.iter() {
            if ident.module.as_str() != self.options.eir_module {
                continue;
            }
            if ident.lambda.is_some() {
                continue;
            }

            let mod_name_str = crate::primitives::make_c_string_const(
                &self.context, &data.module, ident.module.as_str());
            // TODO: UTF8 length incorrect!!
            let mod_name_len = i32_type.const_int(
                ident.module.as_str().len() as u64, false);

            let fun_name_str = crate::primitives::make_c_string_const(
                &self.context, &data.module, ident.name.as_str());
            // TODO: UTF8 length incorrect!!
            let fun_name_len = i32_type.const_int(
                ident.name.as_str().len() as u64, false);

            let arity_val = i32_type.const_int(
                ident.arity as u64, false);
            builder.build_call(
                self.types.whirlrt_module_register_function,
                &[
                    mod_name_len.into(),
                    mod_name_str.as_pointer_value()
                        .const_cast(i8_ptr_type).into(),
                    fun_name_len.into(),
                    fun_name_str.as_pointer_value()
                        .const_cast(i8_ptr_type).into(),
                    arity_val.into(),
                    fun.as_global_value().as_pointer_value()
                        .const_cast(i8_ptr_type).into(),
                ],
                "init_fun",
            );
        }

        builder.build_return(None);

    }

}
