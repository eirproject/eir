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

pub fn make_lists() -> NativeModule {
    let mut module = NativeModule::new("lists".to_string());
    module.add_fun("member".to_string(), 2, Box::new(member));
    module.add_fun("reverse".to_string(), 2, Box::new(reverse));
    module
}
