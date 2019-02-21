use ::module::NativeModule;
use ::vm::VMState;
use ::term::Term;
use ::process::{ CallReturn, ProcessContext };

fn delete(_vm: &VMState, _proc: &mut ProcessContext, args: &[Term]) -> CallReturn {
    assert!(args.len() == 1);
    // TODO
    let n = vec![Term::new_atom("error"), Term::new_atom("enoent")];
    CallReturn::Return { term: Term::Tuple(n) }
}

pub fn make_time() -> NativeModule {
    let mut module = NativeModule::new("file".to_string());
    module.add_fun("delete".to_string(), 1, Box::new(delete));
    module
}
