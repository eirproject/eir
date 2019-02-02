use ::term::{ Term, ErlEq, ErlExactEq };
use ::process::{ CallReturn, ProcessContext };
use ::vm::VMState;
use ::module::NativeModule;

fn member_list(item: &Term, list: &Term) -> CallReturn {
    if let Term::Nil = list {
        CallReturn::Return { term: Term::new_bool(false) }
    } else if let Term::List(ref head, ref tail) = list {
        for l_item in head {
            if item.erl_exact_eq(l_item) {
                return CallReturn::Return { term: Term::new_bool(true) };
            }
        }
        member_list(item, tail)
    } else {
        CallReturn::Throw
    }
}

fn member(_vm: &VMState, _proc: &mut ProcessContext, args: &[Term]) -> CallReturn {
    assert!(args.len() == 2);
    member_list(&args[0], &args[1])
}

fn reverse(_vm: &VMState, _proc: &mut ProcessContext, args: &[Term]) -> CallReturn {
    assert!(args.len() == 2);
    if let Some(mut list) = args[0].as_list() {
        list.reverse();
        CallReturn::Return { term: Term::List(list, Box::new(args[1].clone())) }
    } else {
        CallReturn::Throw
    }
}

fn keyfind(_vm: &VMState, _proc: &mut ProcessContext, args: &[Term]) -> CallReturn {
    assert!(args.len() == 3);
    let key = &args[0];
    let pos = if let Some(int) = args[1].as_i64() {
        int
    } else {
        return CallReturn::Throw;
    };
    let list_term = &args[2];
    let (list, list_tail) = list_term.as_inproper_list();
    for term in list.iter() {
        if let Term::Tuple(values) = term {
            if let Some(val_term) = values.get(pos as usize) {
                if val_term.erl_eq(key) {
                    return CallReturn::Return { term: term.clone() };
                }
            }
        }
    }
    if let Term::Nil = list_tail {
        CallReturn::Return { term: Term::new_bool(false) }
    } else {
        CallReturn::Throw
    }
}

pub fn make_lists() -> NativeModule {
    let mut module = NativeModule::new("lists".to_string());
    module.add_fun("member".to_string(), 2, Box::new(member));
    module.add_fun("reverse".to_string(), 2, Box::new(reverse));
    module.add_fun("keyfind".to_string(), 3, Box::new(keyfind));
    module
}
