use inkwell::AddressSpace;
use inkwell::context::Context;
use inkwell::values::{ FunctionValue, ArrayValue, StructValue, PointerValue,
                       GlobalValue, AnyValueEnum, BasicValueEnum };
use inkwell::types::{ StructType, BasicType, BasicTypeEnum };
use inkwell::module::{ Module };

pub struct EnifFuncT {
    pub name: String,
    pub arity: u32,
    pub fun: PointerValue,
    pub flags: u32,
}

pub struct EnifEntryT {
    pub major: i32,
    pub minor: i32,
    pub name: String,
    pub funcs: Vec<EnifFuncT>,
    pub load: Option<PointerValue>,
    pub reload: Option<PointerValue>,
    pub upgrade: Option<PointerValue>,
    pub unload: Option<PointerValue>,
    pub vm_variant: String,
    pub options: u32,
    pub sizeof_erlnifresourcetypeinit: isize,
    pub min_erts: String,
}

pub fn make_c_string_const(context: &Context, module: &Module, string: &str) -> GlobalValue {
    let i8_type = context.i8_type();

    let mut vals: Vec<_> = string.as_bytes().iter()
        .map(|b| i8_type.const_int(*b as u64, false))
        .collect();
    vals.push(i8_type.const_int(0, false));
    let arr = i8_type.const_array(&vals);
    let val = module.add_global(arr.get_type(), Some(AddressSpace::Const),
                                "const_str");
    val.set_initializer(&arr);
    val
}

pub fn make_enif_func_t(context: &Context, module: &Module, func: &EnifFuncT) -> StructValue {
    let i32_type = context.i32_type();

    let name_value = make_c_string_const(context, module, &func.name);
    let arity_value = i32_type.const_int(func.arity as u64, false);
    let flags_value = i32_type.const_int(0, false);

    context.const_struct(&[
        name_value.as_pointer_value().into(),
        arity_value.into(),
        func.fun.into(),
        flags_value.into(),
    ], false)
}

pub fn make_enif_entry_t(context: &Context, module: &Module, funct: &EnifEntryT) -> GlobalValue {
    let i32_type = context.i32_type();
    let i32_ptr_type = i32_type.ptr_type(AddressSpace::Generic);
    let null_ptr = i32_ptr_type.const_null();

    let major_value = i32_type.const_int(funct.major as u64, false);
    let minor_value = i32_type.const_int(funct.minor as u64, false);
    let module_name = make_c_string_const(context, module, &funct.name);
    let num_of_funcs = i32_type.const_int(funct.funcs.len() as u64, false);

    let funcs: Vec<_> = funct.funcs.iter()
        .map(|func| make_enif_func_t(context, module, func))
        .collect();
    let funcs_array = funcs[0].get_type().const_array(&funcs);
    let funcs_array_global = module.add_global(
        funcs_array.get_type(), Some(AddressSpace::Const), "nif_funcs_array");
    funcs_array_global.set_initializer(&funcs_array);

    let load_fun_ptr = if let Some(ptr) = funct.load { ptr } else { null_ptr };
    let reload_fun_ptr = if let Some(ptr) = funct.reload { ptr } else { null_ptr };
    let upgrade_fun_ptr = if let Some(ptr) = funct.upgrade { ptr } else { null_ptr };
    let unload_fun_ptr = if let Some(ptr) = funct.unload { ptr } else { null_ptr };

    let vm_variant = make_c_string_const(context, module, &funct.vm_variant);
    let options_value = i32_type.const_int(funct.options as u64, false);
    let sizeof_enrti_value =
        i32_type.const_int(funct.sizeof_erlnifresourcetypeinit as u64, false);
    let min_erts = make_c_string_const(context, module, &funct.min_erts);

    let stru = context.const_struct(&[
        major_value.into(),
        minor_value.into(),
        module_name.as_pointer_value().into(),
        num_of_funcs.into(),
        funcs_array_global.as_pointer_value().into(),
        load_fun_ptr.into(),
        reload_fun_ptr.into(),
        upgrade_fun_ptr.into(),
        unload_fun_ptr.into(),
        vm_variant.as_pointer_value().into(),
        options_value.into(),
        sizeof_enrti_value.into(),
        min_erts.as_pointer_value().into(),
    ], false);

    let val = module.add_global(stru.get_type(), Some(AddressSpace::Const),
                                "const_nif_entry");
    val.set_initializer(&stru);
    val
}
