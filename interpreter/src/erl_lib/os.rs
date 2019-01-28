use ::{ NativeModule, Term, CallReturn, VMState };

fn getenv(_vm: &VMState, args: &[Term]) -> CallReturn {
    if args.len() == 1 {
        CallReturn::Return { term: Term::new_bool(false) }
    } else {
        CallReturn::Throw
    }
}

pub fn make_os() -> NativeModule {
    let mut module = NativeModule::new("os".to_string());
    module.add_fun("getenv".to_string(), 1, Box::new(getenv));
    module
}
