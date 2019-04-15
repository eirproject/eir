use inkwell::AddressSpace;
use inkwell::context::Context;
use inkwell::types::{ BasicTypeEnum, FunctionType };
use inkwell::values::FunctionValue;
use inkwell::module::{ Module, Linkage };

pub struct CommonTypes {
    pub bool_type: BasicTypeEnum,
}

pub fn make_common_types(context: &Context, module: &Module) -> CommonTypes {
    let bool_type = context.bool_type();

    CommonTypes {
        bool_type: bool_type.into(),
    }
}
