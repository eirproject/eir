use inkwell::AddressSpace;
use inkwell::context::Context;
use inkwell::values::{ FunctionValue, ArrayValue, StructValue, PointerValue,
                       GlobalValue, AnyValueEnum, BasicValueEnum };
use inkwell::types::{ StructType, BasicType, BasicTypeEnum };
use inkwell::module::{ Module };

pub fn make_c_string_const(context: &Context, module: &Module, string: &str) -> GlobalValue {
    let i8_type = context.i8_type();

    let mut vals: Vec<_> = string.as_bytes().iter()
        .map(|b| i8_type.const_int(*b as u64, false))
        .collect();
    vals.push(i8_type.const_int(0, false));
    let arr = i8_type.const_array(&vals);
    let val = module.add_global(arr.get_type(), None,
                                "const_str");
    val.set_initializer(&arr);
    val
}
