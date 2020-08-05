use libeir_intern::Symbol;

use crate::module::{NativeModule, NativeReturn};
use crate::process::ProcessContext;
use crate::vm::VMState;

use crate::term::{ListIteratorItem, MapTerm, Term};

use std::rc::Rc;

fn new_0(_vm: &VMState, _proc: &mut ProcessContext, args: &[Rc<Term>]) -> NativeReturn {
    assert!(args.len() == 0);
    NativeReturn::Return {
        term: Term::Map(MapTerm::new()).into(),
    }
}

fn from_list_1(_vm: &VMState, _proc: &mut ProcessContext, args: &[Rc<Term>]) -> NativeReturn {
    assert!(args.len() == 1);
    let mut map = MapTerm::new();

    for elem in Term::list_iter(&args[0]) {
        match elem {
            ListIteratorItem::Elem(tup_term) => {
                let tup = tup_term.as_tuple().unwrap();
                assert!(tup.len() == 2);
                map.insert(tup[0].clone(), tup[1].clone());
            }
            ListIteratorItem::Tail(term) => {
                assert!(term.is_nil());
            }
        }
    }

    NativeReturn::Return {
        term: Term::Map(map).into(),
    }
}

pub fn make_maps() -> NativeModule {
    let mut module = NativeModule::new(Symbol::intern("maps"));
    module.add_fun(Symbol::intern("new"), 0, Box::new(new_0));
    module.add_fun(Symbol::intern("from_list"), 1, Box::new(from_list_1));
    module
}
