use inkwell::AddressSpace;
use inkwell::context::Context;
use inkwell::types::{ BasicTypeEnum, FunctionType };
use inkwell::values::FunctionValue;
use inkwell::module::{ Module, Linkage };

pub struct NifTypes {
    pub bool_type: BasicTypeEnum,
    pub term_type: BasicTypeEnum,
    pub term_ptr_type: BasicTypeEnum,
    pub env_type: BasicTypeEnum,
    pub env_ptr_type: BasicTypeEnum,
    pub nif_fun_type: FunctionType,

    pub printf: FunctionValue,

    pub enif_make_long: FunctionValue,
    pub enif_get_long: FunctionValue,
    pub enif_compare: FunctionValue,
    pub enif_make_atom_len: FunctionValue,
    pub enif_raise_exception: FunctionValue,

    pub on_load: FunctionValue,
    pub make_lambda_environment: FunctionValue,
    pub make_bound_lambda: FunctionValue,
}

pub fn make_nif_types(context: &Context, module: &Module) -> NifTypes {

    let void_type = context.void_type();
    let void_ptr_type = void_type.ptr_type(AddressSpace::Generic);
    let str_ptr = context.i8_type().ptr_type(AddressSpace::Generic);
    let bool_type = context.bool_type();
    let i64_type = context.i64_type();
    let i32_type = context.i32_type();
    let enif_env_type = context.opaque_struct_type("enif_environment_t");
    let env_ptr_type = enif_env_type.ptr_type(AddressSpace::Generic);
    let char_ptr = context.i8_type().ptr_type(AddressSpace::Generic);

    let term_type = i64_type;
    let term_ptr_type = term_type.ptr_type(AddressSpace::Generic);

    let printf_val = {
        let typ = void_type.fn_type(&[
            str_ptr.into(),
        ], true);
        module.add_function("printf", typ, None)
    };

    let enif_make_long_val = {
        let typ = term_type.fn_type(&[
            env_ptr_type.into(),
            i64_type.into(),
        ], false);
        module.add_function("enif_make_long", typ, Some(Linkage::DLLImport))
    };

    let enif_get_long_val = {
        let typ = bool_type.fn_type(&[
            env_ptr_type.into(),
            i64_type.into(),
            i64_type.ptr_type(AddressSpace::Generic).into(),
        ], false);
        module.add_function("enif_get_long", typ, Some(Linkage::DLLImport))
    };

    let enif_compare_val = {
        let typ = i32_type.fn_type(&[
            term_type.into(),
            term_type.into()
        ], false);
        module.add_function("enif_compare", typ, Some(Linkage::DLLImport))
    };

    let enif_make_atom_len = {
        let typ = term_type.fn_type(&[
            env_ptr_type.into(),
            char_ptr.into(),
            i64_type.into(),
        ], false);
        module.add_function("enif_make_atom_len", typ, Some(Linkage::DLLImport))
    };

    let enif_raise_exception = {
        let typ = term_type.fn_type(&[
            env_ptr_type.into(),
            term_type.into(),
        ], false);
        module.add_function("enif_raise_exception", typ, Some(Linkage::DLLImport))
    };

    let on_load = {
        let typ = term_type.fn_type(&[
            env_ptr_type.into(),
            void_ptr_type.into(),
            term_type.into(),
        ], false);
        module.add_function("on_load", typ, Some(Linkage::DLLImport))
    };

    let make_lambda_environment = {
        let typ = term_type.fn_type(&[
            env_ptr_type.into(), // Calling environment
            str_ptr.into(), // Module
            str_ptr.into(), // Name
            i32_type.into(), // Env num
            i64_type.into(), // Captures length
            term_ptr_type.into(), // Captures
        ], false);
        module.add_function("make_lambda_environment", typ, Some(Linkage::DLLImport))
    };

    let make_bound_lambda = {
        let typ = term_type.fn_type(&[
            env_ptr_type.into(), // Calling environment
            i32_type.into(), // Function num
            void_ptr_type.into(), // Actual function
            term_type.into(), // Environment
        ], false);
        module.add_function("make_bound_lambda", typ, Some(Linkage::DLLImport))
    };

    let nif_fun_type = term_type.fn_type(&[
        env_ptr_type.into(),
        i64_type.into(),
        term_ptr_type.into(),
    ], false);

    NifTypes {
        bool_type: context.bool_type().into(),
        env_type: enif_env_type.into(),
        env_ptr_type: enif_env_type.ptr_type(AddressSpace::Generic).into(),
        term_type: i64_type.into(),
        term_ptr_type: i64_type.ptr_type(AddressSpace::Generic).into(),

        printf: printf_val,

        nif_fun_type: nif_fun_type,

        enif_make_long: enif_make_long_val,
        enif_get_long: enif_get_long_val,
        enif_compare: enif_compare_val,
        enif_make_atom_len: enif_make_atom_len,
        enif_raise_exception: enif_raise_exception,

        on_load: on_load,
        make_lambda_environment: make_lambda_environment,
        make_bound_lambda: make_bound_lambda,
    }
}
