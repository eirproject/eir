use std::collections::HashMap;

use inkwell::AddressSpace;
use inkwell::context::Context;
use inkwell::module::{ Module, Linkage };
use inkwell::values::{ BasicValueEnum, GlobalValue, PointerValue, FunctionValue };
use inkwell::types::{ BasicTypeEnum, VoidType };

use eir::Atom;

pub struct WasmTypes {
    // Basics
    pub void_type: VoidType,

    // Terms
    pub process_env_ptr_type: BasicTypeEnum,
    pub term_type: BasicTypeEnum,

    // Tables
    pub static_atom_table: HashMap<Atom, GlobalValue>,

    // Runtime functions
    pub whirlrt_term_eq: FunctionValue,
    pub whirlrt_term_make_tuple: FunctionValue,
    pub whirlrt_call_cont: FunctionValue,
    pub whirlrt_term_make_atom_from_string: FunctionValue,
    pub whirlrt_module_register_function: FunctionValue,
    pub whirlrt_term_unpack_closure_env: FunctionValue,
    pub whirlrt_temp_hacky_transmute_tup_to_fun_env: FunctionValue,
    pub whirlrt_term_make_fun: FunctionValue,
    pub whirlrt_term_get_fun: FunctionValue,
    pub whirlrt_term_make_smallint: FunctionValue,
    pub whirlrt_term_unpack_tuple: FunctionValue,
    pub whirlrt_unreachable_fail: FunctionValue,
    pub whirlrt_make_dynfun: FunctionValue,
    pub whirlrt_call_dynfun: FunctionValue,
}

impl WasmTypes {

    pub fn new(context: &Context, module: &Module) -> Self {
        let void_type = context.void_type();
        let bool_type = context.bool_type();
        let i8_type = context.i8_type();
        let i32_type = context.i32_type();
        let i64_type = context.i64_type();

        let i8_ptr_type = i8_type.ptr_type(AddressSpace::Generic);

        let process_env_ptr_type = context
            .opaque_struct_type("whirl_process_env")
            .ptr_type(AddressSpace::Generic);
        let term_type = context.i64_type();
        let term_ptr_type = term_type.ptr_type(AddressSpace::Generic);

        let whirlrt_term_eq = {
            let typ = bool_type.fn_type(&[
                process_env_ptr_type.into(),
                term_type.into(),
                term_type.into(),
            ], false);
            module.add_function("whirlrt_term_eq", typ, Some(Linkage::External))
        };

        let whirlrt_term_make_tuple = {
            let typ = term_type.fn_type(&[
                process_env_ptr_type.into(),
                i32_type.into(),
                term_ptr_type.into(),
            ], false);
            module.add_function("whirlrt_term_make_tuple", typ, Some(Linkage::External))
        };

        let whirlrt_call_cont = {
            let typ = void_type.fn_type(&[
                process_env_ptr_type.into(),
                term_type.into(),
                term_type.into(),
            ], false);
            module.add_function("whirlrt_call_cont", typ, Some(Linkage::External))
        };

        let whirlrt_term_make_atom_from_string = {
            let typ = term_type.fn_type(&[
                i32_type.into(),
                i8_ptr_type.into(),
            ], false);
            module.add_function("whirlrt_term_make_atom_from_string", typ,
                                Some(Linkage::External))
        };

        let whirlrt_module_register_function = {
            let typ = void_type.fn_type(&[
                i32_type.into(),
                i8_ptr_type.into(),
                i32_type.into(),
                i8_ptr_type.into(),
                i32_type.into(),
                i8_ptr_type.into(),
            ], false);
            module.add_function("whirlrt_module_register_function", typ,
                                Some(Linkage::External))
        };

        let whirlrt_term_unpack_closure_env = {
            let typ = void_type.fn_type(&[
                process_env_ptr_type.into(),
                term_type.into(), // ClosureEnv
                i32_type.into(), // NumFree
                term_ptr_type.into(),
            ], false);
            module.add_function("whirlrt_term_unpack_closure_env", typ,
                                Some(Linkage::External))
        };

        let whirlrt_temp_hacky_transmute_tup_to_fun_env = {
            let typ = void_type.fn_type(&[
                process_env_ptr_type.into(),
                term_type.into(), // Env as tuple
                i8_ptr_type.into(), // Function
            ], false);
            module.add_function("whirlrt_temp_hacky_transmute_tup_to_fun_env", typ,
                                Some(Linkage::External))
        };

        let whirlrt_term_make_fun = {
            let typ = term_type.fn_type(&[
                process_env_ptr_type.into(),
                i8_ptr_type.into(), // Function
            ], false);
            module.add_function("whirlrt_term_make_fun", typ,
                                Some(Linkage::External))
        };

        let whirlrt_term_get_fun = {
            let typ = i8_ptr_type.fn_type(&[
                process_env_ptr_type.into(),
                term_type.into(), // Function
            ], false);
            module.add_function("whirlrt_term_get_fun", typ,
                                Some(Linkage::External))
        };

        let whirlrt_term_make_smallint = {
            let typ = term_type.fn_type(&[
                process_env_ptr_type.into(),
                i64_type.into(),
            ], false);
            module.add_function("whirlrt_term_make_smallint", typ,
                                Some(Linkage::External))
        };

        let whirlrt_term_unpack_tuple = {
            let typ = bool_type.fn_type(&[
                process_env_ptr_type.into(),
                term_type.into(), // In term
                i32_type.into(), // Tuple length
                term_ptr_type.into(), // Out buf pointer
            ], false);
            module.add_function("whirlrt_term_unpack_tuple", typ,
                                Some(Linkage::External))
        };

        let whirlrt_unreachable_fail = {
            let typ = void_type.fn_type(&[
                process_env_ptr_type.into(),
            ], false);
            module.add_function("whirlrt_unreachable_fail", typ,
                                Some(Linkage::External))
        };

        let whirlrt_make_dynfun = {
            let typ = i32_type.fn_type(&[
                i32_type.into(),
                i8_ptr_type.into(),
                i32_type.into(),
                i8_ptr_type.into(),
                i32_type.into(),
            ], false);
            module.add_function("whirlrt_make_dynfun", typ,
                                Some(Linkage::External))
        };

        let whirlrt_call_dynfun = {
            let typ = void_type.fn_type(&[
                i32_type.into(),
                process_env_ptr_type.into(),
                term_ptr_type.into(),
                i32_type.into(),
            ], false);
            module.add_function("whirlrt_call_dynfun", typ,
                                Some(Linkage::External))
        };

        WasmTypes {
            process_env_ptr_type: process_env_ptr_type.into(),
            void_type: void_type,
            term_type: term_type.into(),
            static_atom_table: HashMap::new(),
            whirlrt_term_eq: whirlrt_term_eq,
            whirlrt_term_make_tuple: whirlrt_term_make_tuple,
            whirlrt_call_cont: whirlrt_call_cont,
            whirlrt_term_make_atom_from_string: whirlrt_term_make_atom_from_string,
            whirlrt_module_register_function: whirlrt_module_register_function,
            whirlrt_term_unpack_closure_env: whirlrt_term_unpack_closure_env,
            whirlrt_temp_hacky_transmute_tup_to_fun_env: whirlrt_temp_hacky_transmute_tup_to_fun_env,
            whirlrt_term_make_fun: whirlrt_term_make_fun,
            whirlrt_term_get_fun: whirlrt_term_get_fun,
            whirlrt_term_make_smallint: whirlrt_term_make_smallint,
            whirlrt_term_unpack_tuple: whirlrt_term_unpack_tuple,
            whirlrt_unreachable_fail: whirlrt_unreachable_fail,
            whirlrt_make_dynfun: whirlrt_make_dynfun,
            whirlrt_call_dynfun: whirlrt_call_dynfun,
        }
    }

}
