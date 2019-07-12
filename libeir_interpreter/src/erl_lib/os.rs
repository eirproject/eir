use ::vm::VMState;
use ::module::NativeModule;
use ::term::Term;
use ::process::{ CallReturn, ProcessContext };

fn getenv(_vm: &VMState, _proc: &mut ProcessContext, args: &[Term]) -> CallReturn {
    if args.len() == 1 {
        CallReturn::Return { term: Term::new_bool(false) }
    } else {
        CallReturn::Throw
    }
}

fn os_type(_vm: &VMState, _proc: &mut ProcessContext, args: &[Term]) -> CallReturn {
    assert!(args.len() == 0);
    // TODO
    let family = Term::new_atom("unix");
    let name = Term::new_atom("linux");
    CallReturn::Return { term: Term::Tuple(vec![family, name]) }
}

pub fn make_os() -> NativeModule {
    let mut module = NativeModule::new("os".to_string());
    module.add_fun("getenv".to_string(), 1, Box::new(getenv));
    module.add_fun("type".to_string(), 0, Box::new(os_type));
    module
}
