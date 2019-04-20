use inkwell::{ AddressSpace };
use inkwell::context::Context;
use inkwell::module::{ Module, Linkage };
use inkwell::types::{ BasicTypeEnum, FunctionType };
use inkwell::values::{ FunctionValue, GlobalValue };

pub struct NifTypes {
    pub term_type: BasicTypeEnum,
    pub term_ptr_type: BasicTypeEnum,
    pub env_type: BasicTypeEnum,
    pub env_ptr_type: BasicTypeEnum,
    pub nif_fun_type: FunctionType,
    //pub void_ptr_type: BasicTypeEnum,
    pub i8_ptr: BasicTypeEnum,

    //pub closure_type: BasicTypeEnum,
    //pub closure_env_type: BasicTypeEnum,

    pub printf: FunctionValue,

    pub enif_make_long: FunctionValue,
    pub enif_get_long: FunctionValue,
    pub enif_compare: FunctionValue,
    pub enif_make_atom_len: FunctionValue,
    pub enif_raise_exception: FunctionValue,
    pub enif_make_tuple_from_array: FunctionValue,
    pub enif_get_tuple: FunctionValue,
    pub enif_make_map_put: FunctionValue,
    pub enif_make_map_update: FunctionValue,
    pub enif_make_list_from_array: FunctionValue,

    pub unreachable_fail: FunctionValue,
    pub nifrt_launchpad: FunctionValue,
    pub nifrt_cont_call: FunctionValue,
    pub nifrt_unpack_fun_ptr: FunctionValue,

    pub nifrt_atom_false: GlobalValue,
    pub nifrt_atom_nil: GlobalValue,

    pub on_load: FunctionValue,
    //pub make_lambda_environment: FunctionValue,
    //pub make_bound_lambda: FunctionValue,
    pub make_fun_ptr_res: FunctionValue,
}

impl NifTypes {

    pub fn new(context: &Context, module: &Module) -> Self {

        let void_type = context.void_type();
        //let void_ptr_type = void_type.ptr_type(AddressSpace::Generic);
        let str_ptr = context.i8_type().ptr_type(AddressSpace::Generic);
        let bool_type = context.bool_type();
        let i64_type = context.i64_type();
        let i32_type = context.i32_type();
        let enif_env_type = context.opaque_struct_type("enif_environment_t");
        let env_ptr_type = enif_env_type.ptr_type(AddressSpace::Generic);
        let char_ptr = context.i8_type().ptr_type(AddressSpace::Generic);

        let term_type = i64_type;
        let term_ptr_type = term_type.ptr_type(AddressSpace::Generic);

        //let closure_type = context.struct_type(&[
        //    void_ptr_type.into(),
        //    void_ptr_type.into(), // TODO
        //], false);

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

        let enif_make_tuple_from_array = {
            let typ = term_type.fn_type(&[
                env_ptr_type.into(),
                term_ptr_type.into(),
                i32_type.into(),
            ], false);
            module.add_function("enif_make_tuple_from_array", typ,
                                Some(Linkage::DLLImport))
        };

        let enif_make_map_put = {
            let typ = i32_type.fn_type(&[
                env_ptr_type.into(),
                term_type.into(),
                term_type.into(),
                term_type.into(),
                term_ptr_type.into(),
            ], false);
            module.add_function("enif_make_map_put", typ,
                                Some(Linkage::DLLImport))
        };
        let enif_make_map_update = {
            let typ = i32_type.fn_type(&[
                env_ptr_type.into(),
                term_type.into(),
                term_type.into(),
                term_type.into(),
                term_ptr_type.into(),
            ], false);
            module.add_function("enif_make_map_update", typ,
                                Some(Linkage::DLLImport))
        };

        let enif_make_list_from_array = {
            let typ = term_type.fn_type(&[
                env_ptr_type.into(),
                term_ptr_type.into(),
                i32_type.into()
            ], false);
            module.add_function("enif_make_list_from_array", typ,
                                Some(Linkage::DLLImport))
        };

        let enif_get_tuple = {
            let typ = i32_type.fn_type(&[
                env_ptr_type.into(),
                term_type.into(),
                i32_type.ptr_type(AddressSpace::Generic).into(),
                term_ptr_type.ptr_type(AddressSpace::Generic).into(),
            ], false);
            module.add_function("enif_get_tuple", typ,
                                Some(Linkage::DLLImport))
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
                char_ptr.into(),
                term_type.into(),
            ], false);
            module.add_function("on_load", typ, Some(Linkage::DLLImport))
        };

        let make_fun_ptr_res = {
            let typ = term_type.fn_type(&[
                env_ptr_type.into(),
                char_ptr.into(),
                i32_type.into(),
            ], false);
            module.add_function("nifrt_make_fun_ptr", typ, Some(Linkage::DLLImport))
        };

        let nif_fun_type = term_type.fn_type(&[
            env_ptr_type.into(),
            i64_type.into(),
            term_ptr_type.into(),
        ], false);

        let unreachable_fail = {
            let typ = void_type.fn_type(&[i64_type.into()], false);
            module.add_function("nifrt_unreachable_fail", typ,
                                Some(Linkage::DLLImport))
        };

        let nifrt_launchpad = {
            let typ = term_type.fn_type(&[
                env_ptr_type.into(),
                term_ptr_type.into(),
                i64_type.into(),
                char_ptr.into(),
            ], false);
            module.add_function("nifrt_launchpad", typ,
                                Some(Linkage::DLLImport))
        };

        let nifrt_cont_call = {
            let typ = void_type.fn_type(&[
                env_ptr_type.into(),
                term_type.into(),
                term_type.into(),
            ], false);
            module.add_function("nifrt_cont_call", typ,
                                Some(Linkage::DLLImport))
        };

        let nifrt_unpack_fun_ptr = {
            let typ = char_ptr.fn_type(&[
                env_ptr_type.into(),
                i32_type.into(),
                term_type.into(),
            ], false);
            module.add_function("nifrt_unpack_fun_ptr", typ,
                                Some(Linkage::DLLImport))
        };

        let nifrt_atom_false = module.add_global(
            term_type, Some(AddressSpace::Const), "nifrt_false_atom");
        let nifrt_atom_nil = module.add_global(
            term_type, Some(AddressSpace::Const), "nifrt_nil_atom");

        NifTypes {
            env_type: enif_env_type.into(),
            env_ptr_type: enif_env_type.ptr_type(AddressSpace::Generic).into(),
            term_type: i64_type.into(),
            term_ptr_type: i64_type.ptr_type(AddressSpace::Generic).into(),
            //void_ptr_type: void_ptr_type.into(),
            i8_ptr: char_ptr.into(),

            //closure_type: closure_type.into(),
            //closure_env_type: void_type.ptr_type(AddressSpace::Generic).into(),

            printf: printf_val,

            nif_fun_type: nif_fun_type,

            enif_make_long: enif_make_long_val,
            enif_get_long: enif_get_long_val,
            enif_compare: enif_compare_val,
            enif_make_atom_len: enif_make_atom_len,
            enif_raise_exception: enif_raise_exception,
            enif_make_tuple_from_array: enif_make_tuple_from_array,
            enif_get_tuple: enif_get_tuple,
            enif_make_map_put: enif_make_map_put,
            enif_make_map_update: enif_make_map_update,
            enif_make_list_from_array: enif_make_list_from_array,

            unreachable_fail: unreachable_fail,
            nifrt_launchpad: nifrt_launchpad,
            nifrt_cont_call: nifrt_cont_call,
            nifrt_unpack_fun_ptr: nifrt_unpack_fun_ptr,

            nifrt_atom_false: nifrt_atom_false,
            nifrt_atom_nil: nifrt_atom_nil,

            on_load: on_load,
            //make_lambda_environment: make_lambda_environment,
            //make_bound_lambda: make_bound_lambda,
            make_fun_ptr_res: make_fun_ptr_res,
        }
    }

}
