use llvm_sys as llvm;
use std::ptr;

use inkwell::context::Context;

fn make_nif_init(context: &Context) {
    let module = context.create_module("entry");

}

fn main() {
    let context = Context::create();

}
