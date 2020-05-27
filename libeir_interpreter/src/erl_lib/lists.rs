use std::rc::Rc;

use crate::module::{NativeModule, NativeReturn};
use crate::process::ProcessContext;
use crate::term::{ErlEq, Term};
use crate::vm::VMState;

use libeir_intern::Symbol;

//fn member_list(item: &Term, list: &Term) -> NativeReturn {
//    if let Term::Nil = list {
//        NativeReturn::Return { term: Term::new_bool(false).into() }
//    } else if let Term::List(ref head, ref tail) = list {
//        for l_item in head {
//            if item.erl_exact_eq(l_item) {
//                return NativeReturn::Return { term: Term::new_bool(true).into() };
//            }
//        }
//        member_list(item, tail)
//    } else {
//        NativeReturn::Throw
//    }
//}
//
//fn member(_vm: &VMState, _proc: &mut ProcessContext, args: &[Rc<Term>]) -> NativeReturn {
//    assert!(args.len() == 2);
//    member_list(&args[0], &args[1])
//}
//
fn reverse_2(_vm: &VMState, _proc: &mut ProcessContext, args: &[Rc<Term>]) -> NativeReturn {
    assert!(args.len() == 2);

    let (mut head, tail) = Term::as_inproper_list(&args[0]);
    assert!(tail.erl_eq(&Term::Nil));

    head.reverse();
    NativeReturn::Return {
        term: Term::slice_to_list(&head, args[1].clone()),
    }
}

fn reverse_1(vm: &VMState, proc: &mut ProcessContext, args: &[Rc<Term>]) -> NativeReturn {
    assert!(args.len() == 1);
    reverse_2(vm, proc, &[args[0].clone(), Term::Nil.into()])
}

//fn keyfind(_vm: &VMState, _proc: &mut ProcessContext, args: &[Rc<Term>]) -> NativeReturn {
//    assert!(args.len() == 3);
//    let key = &*args[0];
//    let pos = if let Some(int) = args[1].as_i64() {
//        int
//    } else {
//        return NativeReturn::Throw;
//    };
//    let list_term = &*args[2];
//    let (list, list_tail) = list_term.as_inproper_list();
//    for term in list.iter() {
//        if let Term::Tuple(values) = &**term {
//            if let Some(val_term) = values.get(pos as usize) {
//                if val_term.erl_eq(key) {
//                    return NativeReturn::Return { term: term.clone() };
//                }
//            }
//        }
//    }
//    if let Term::Nil = list_tail {
//        NativeReturn::Return { term: Term::new_bool(false).into() }
//    } else {
//        NativeReturn::Throw
//    }
//}

pub fn make_lists() -> NativeModule {
    let mut module = NativeModule::new(Symbol::intern("lists"));
    //module.add_fun(Symbol::intern("member"), 2, Box::new(member));
    module.add_fun(Symbol::intern("reverse"), 1, Box::new(reverse_1));
    module.add_fun(Symbol::intern("reverse"), 2, Box::new(reverse_2));
    //module.add_fun(Symbol::intern("keyfind"), 3, Box::new(keyfind));
    module
}
