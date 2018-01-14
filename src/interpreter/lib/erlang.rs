use ::interpreter::{ NativeModule, Term, CallReturn };

fn add(args: &[Term]) -> CallReturn {
    // TODO
    CallReturn::Return { term: Term::Nil }
}

pub fn make_erlang() -> NativeModule {
    let mut module = NativeModule::new("erlang".to_string());
    module.add_fun("+".to_string(), 2, Box::new(add));
    module
}
