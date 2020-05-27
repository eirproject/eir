use libeir_intern::Symbol;

use crate::module::{NativeModule, NativeReturn};
use crate::process::ProcessContext;
use crate::vm::VMState;

use crate::term::{MapTerm, Term};

use std::rc::Rc;

fn new_0(_vm: &VMState, _proc: &mut ProcessContext, args: &[Rc<Term>]) -> NativeReturn {
    assert!(args.len() == 0);
    NativeReturn::Return {
        term: Term::Map(MapTerm::new()).into(),
    }
}

pub fn make_maps() -> NativeModule {
    let mut module = NativeModule::new(Symbol::intern("maps"));
    module.add_fun(Symbol::intern("new"), 0, Box::new(new_0));
    module
}
