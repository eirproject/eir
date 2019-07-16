use libeir_ir::FunctionIdent;
use libeir_intern::Symbol;

use crate::vm::{ VMState, WatchType };
use crate::module::{ NativeModule, NativeReturn };
use crate::process::{ ProcessContext };

use crate::term::{ Term, Pid, Reference };
use crate::term::{ ErlEq, ErlExactEq, ErlOrd };
use crate::term::{ ListIteratorItem };

use ::num_bigint::{ BigInt, Sign };
use ::num_traits::{ Signed };

use std::rc::Rc;
use std::cell::RefCell;

fn bignum_to_f64(n: &BigInt) -> Option<f64> {
    // ieee float layout:
    // 1b sign
    // 11b exponent
    // 52b fraction

    let (sign, data) = n.to_bytes_be();
    if data.len() == 0 {
        return Some(0f64);
    }

    assert!(data[0] != 0);

    // Construct exponent
    let mut exp: u64 = n.bits() as u64;
    if exp < (64 - 12) {
        exp = 0;
    } else {
        exp -= 64 - 12;
    }
    exp += 1023;
    if exp >= 2048 {
        return None;
    }
    exp = exp << 52;

    // Collect largest part of number
    let mut fraction_part: u64 = 0;
    if data.len() > 7 {
        for b in &data[0..8] {
            fraction_part = (fraction_part << 8) | (*b as u64);
        }
    } else {
        for b in &data {
            fraction_part = (fraction_part << 8) | (*b as u64);
        }
    }

    // Offset data to be contained within fraction part
    let leading_zeros = fraction_part.leading_zeros();
    if leading_zeros < 12 {
        fraction_part = fraction_part >> (12 - leading_zeros);
    }

    // Construct number
    let mut ret: u64 = 0;

    // Sign
    if sign == Sign::Minus {
        ret |= 1 << 63;
    }

    // Exponent
    ret |= exp;

    // Fraction
    ret |= fraction_part;

    Some(f64::from_bits(ret))
}

fn abs(_vm: &VMState, _proc: &mut ProcessContext, args: &[Rc<Term>]) -> NativeReturn {
    if args.len() != 1 {
        return NativeReturn::Throw;
    }
    let a1 = &*args[0];

    let ret = match a1 {
        Term::Integer(ref int) => Term::Integer(int.clone().abs()),
        Term::Float(flt) => Term::Float(flt.abs()),
        _ => panic!(),
    };

    NativeReturn::Return { term: ret.into() }
}

fn add(_vm: &VMState, _proc: &mut ProcessContext, args: &[Rc<Term>]) -> NativeReturn {
    // TODO: Verify semantics

    if args.len() != 2 {
        return NativeReturn::Throw;
    }
    let a1 = &*args[0];
    let a2 = &*args[1];

    match (a1, a2) {
        (Term::Integer(ref i1), Term::Integer(ref i2)) =>
            NativeReturn::Return { term: Term::Integer(i1 + i2).into() },
        (Term::Integer(ref i1), Term::Float(f2)) => {
            let f1 = bignum_to_f64(i1);
            if f1 == None {
                NativeReturn::Throw
            } else {
                NativeReturn::Return { term: Term::Float(f1.unwrap() + f2).into() }
            }
        }
        (Term::Float(f1), Term::Integer(ref i2)) => {
            let f2 = bignum_to_f64(i2);
            if f2 == None {
                NativeReturn::Throw
            } else {
                NativeReturn::Return { term: Term::Float(f1 + f2.unwrap()).into() }
            }
        }
        (Term::Float(f1), Term::Float(f2)) => {
            NativeReturn::Return { term: Term::Float(f1 + f2).into() }
        }
        _ => NativeReturn::Throw,
    }
}

fn sub(_vm: &VMState, _proc: &mut ProcessContext, args: &[Rc<Term>]) -> NativeReturn {
    if args.len() != 2 {
        return NativeReturn::Throw;
    }
    let a1 = &*args[0];
    let a2 = &*args[1];

    match (a1, a2) {
        (Term::Integer(ref i1), Term::Integer(ref i2)) =>
            NativeReturn::Return { term: Term::Integer(i1 - i2).into() },
        (Term::Integer(ref int), Term::Float(ref flt)) => {
            let flt_c = bignum_to_f64(int).unwrap();
            NativeReturn::Return { term: Term::Float(flt_c - *flt).into() }
        }
        (Term::Float(ref flt), Term::Integer(ref int)) => {
            let flt_c = bignum_to_f64(int).unwrap();
            NativeReturn::Return { term: Term::Float(*flt - flt_c).into() }
        }
        (Term::Float(flt1), Term::Float(flt2)) =>
            NativeReturn::Return { term: Term::Float(flt1 - flt2).into() },
        _ => unimplemented!(),
    }
}

fn mul(_vm: &VMState, _proc: &mut ProcessContext, args: &[Rc<Term>]) -> NativeReturn {
    if args.len() != 2 {
        return NativeReturn::Throw;
    }
    let a1 = &*args[0];
    let a2 = &*args[1];

    match (a1, a2) {
        (Term::Integer(ref i1), Term::Integer(ref i2)) =>
            NativeReturn::Return { term: Term::Integer(i1 * i2).into() },
        _ => unimplemented!(),
    }
}

fn div(_vm: &VMState, _proc: &mut ProcessContext, args: &[Rc<Term>]) -> NativeReturn {
    if args.len() != 2 {
        return NativeReturn::Throw;
    }

    let a1 = match &*args[0] {
        Term::Integer(i1) => bignum_to_f64(i1).unwrap(),
        Term::Float(flt) => *flt,
        _ => panic!(),
    };
    let a2 = match &*args[1] {
        Term::Integer(i1) => bignum_to_f64(i1).unwrap(),
        Term::Float(flt) => *flt,
        _ => panic!(),
    };

    NativeReturn::Return { term: Term::Float(a1 / a2).into() }
}

fn is_list(_vm: &VMState, _proc: &mut ProcessContext, args: &[Rc<Term>]) -> NativeReturn {
    if args.len() != 1 {
        return NativeReturn::Throw;
    }
    let a1 = &*args[0];

    match a1 {
        Term::ListCell(_, _) => NativeReturn::Return { term: Term::new_atom("true").into() },
        Term::Nil => NativeReturn::Return { term: Term::new_atom("true").into() },
        _ => NativeReturn::Return { term: Term::new_atom("false").into() },
    }
}

fn is_atom(_vm: &VMState, _proc: &mut ProcessContext, args: &[Rc<Term>]) -> NativeReturn {
    if args.len() != 1 {
        return NativeReturn::Throw;
    }
    let a1 = &*args[0];

    match a1 {
        Term::Atom(_) => NativeReturn::Return { term: Term::new_bool(true).into() },
        _ => NativeReturn::Return { term: Term::new_bool(false).into() },
    }
}

fn is_integer(_vm: &VMState, _proc: &mut ProcessContext, args: &[Rc<Term>]) -> NativeReturn {
    assert!(args.len() == 1);
    let a1 = &*args[0];
    match a1 {
        Term::Integer(_) => NativeReturn::Return { term: Term::new_bool(true).into() },
        _ => NativeReturn::Return { term: Term::new_bool(false).into() },
    }
}

//fn list_append(_vm: &VMState, _proc: &mut ProcessContext, args: &[Rc<Term>]) -> NativeReturn {
//    // TODO: Validate semantics
//    assert!(args.len() == 2);
//    match (&*args[0], &*args[1]) {
//        (Term::Nil, Term::Nil) => NativeReturn::Return { term: Term::Nil.into() },
//        (Term::Nil, Term::List(_, _)) => NativeReturn::Return { term: args[1].clone() },
//        (Term::List(_, ref tail), Term::Nil) if tail.erl_eq(&Term::Nil)
//            => NativeReturn::Return { term: args[0].clone() },
//        (Term::List(ref _f_head, ref _f_tail), Term::List(ref b_head, ref b_tail)) => {
//            let (mut f_head_terms, f_tail_term) = args[0].as_inproper_list();
//            if let Term::Nil = f_tail_term {
//                f_head_terms.extend(b_head.iter().cloned());
//                NativeReturn::Return { term: Term::List(f_head_terms, b_tail.clone()).into() }
//            } else {
//                NativeReturn::Throw
//            }
//        }
//        _ => NativeReturn::Throw,
//    }
//}
fn list_subtract(_vm: &VMState, _proc: &mut ProcessContext, args: &[Rc<Term>]) -> NativeReturn {
    let (mut to_remove_vec, tail) = Term::as_inproper_list(&args[1]);
    assert!(tail.erl_eq(&Term::Nil));

    let mut out = Vec::new();
    for item in Term::list_iter(&args[0]) {
        match item {
            ListIteratorItem::Elem(elem) => {
                if let Some(idx) = to_remove_vec.iter().enumerate()
                    .find(|(_, term)| elem.erl_eq(term))
                    .map(|(idx, _)| idx)
                {
                    to_remove_vec.remove(idx);
                } else {
                    out.push(elem);
                }
            }
            ListIteratorItem::Tail(tail) => {
                assert!(tail.erl_eq(&Term::Nil));
                return NativeReturn::Return { term: Term::slice_to_list(&out, tail.clone()) }
            }
        }
    }

    unreachable!()
}

fn exact_eq(_vm: &VMState, _proc: &mut ProcessContext, args: &[Rc<Term>]) -> NativeReturn {
    assert!(args.len() == 2);
    NativeReturn::Return { term: Term::new_bool(args[0].erl_exact_eq(&*args[1])).into() }
}

fn and(_vm: &VMState, _proc: &mut ProcessContext, args: &[Rc<Term>]) -> NativeReturn {
    assert!(args.len() == 2);
    if let (Some(a1), Some(a2)) = (args[0].as_boolean(), args[1].as_boolean()) {
        NativeReturn::Return { term: Term::new_bool(a1 && a2).into() }
    } else {
        NativeReturn::Throw
    }
}

fn or(_vm: &VMState, _proc: &mut ProcessContext, args: &[Rc<Term>]) -> NativeReturn {
    assert!(args.len() == 2);
    if let (Some(a1), Some(a2)) = (args[0].as_boolean(), args[1].as_boolean()) {
        NativeReturn::Return { term: Term::new_bool(a1 || a2).into() }
    } else {
        NativeReturn::Throw
    }
}

fn tuple_size(_vm: &VMState, _proc: &mut ProcessContext, args: &[Rc<Term>]) -> NativeReturn {
    assert!(args.len() == 1);
    if let Term::Tuple(ref terms) = &*args[0] {
        NativeReturn::Return { term: Term::new_i64(terms.len() as i64).into() }
    } else {
        NativeReturn::Throw
    }
}

fn is_function(_vm: &VMState, _proc: &mut ProcessContext, args: &[Rc<Term>]) -> NativeReturn {
    assert!(args.len() == 1 || args.len() == 2);

    let arity_ref = if args.len() == 2 {
        if let Some(int) = args[1].as_i64() {
            Some(int)
        } else {
            return NativeReturn::Throw;
        }
    } else {
        None
    };

    if let Term::CapturedFunction { ident, .. } = &*args[0] {
        let res = arity_ref.map(|a| a == ident.arity as i64).unwrap_or(true);
        NativeReturn::Return { term: Term::new_bool(res).into() }
    } else if let Term::BoundLambda { ident, .. } = &*args[0] {
        let res = arity_ref.map(|a| a == ident.arity as i64).unwrap_or(true);
        NativeReturn::Return { term: Term::new_bool(res).into() }
    } else {
        NativeReturn::Return { term: Term::new_bool(false).into() }
    }
}

//fn base_spawn(vm: &VMState, ident: &FunctionIdent, args: Vec<Term>) -> Pid {
//    let new_pid = {
//        let mut processes = vm.processes.borrow();
//        Pid(processes.len())
//    };
//
//    let process = ProcessContext::new(new_pid);
//
//    let orig_pid = crate::trace::get_pid();
//    crate::trace::set_pid(new_pid);
//    let frame = process.make_call_stackframe(
//        vm,
//        ident.module.clone(),
//        ident.clone(),
//        args
//    );
//    crate::trace::set_pid(orig_pid);
//
//    let stack_i = process.stack.clone();
//    let mut stack = stack_i.borrow_mut();
//    stack.push(frame);
//
//    {
//        let mut processes = vm.processes.borrow_mut();
//        processes.push(Rc::new(RefCell::new(process)));
//    }
//
//    {
//        let mut mailboxes = vm.mailboxes.borrow_mut();
//        mailboxes.insert(new_pid, ::mailbox::Mailbox::new());
//    }
//
//    new_pid
//}
//
//fn base_spawn_term(vm: &VMState, callable: &Term, mut args: Vec<Term>) -> Pid {
//    match callable {
//        Term::CapturedFunction { module, fun_name, arity } => {
//            let ident = FunctionIdent {
//                module: module.clone(),
//                name: fun_name.clone(),
//                arity: *arity,
//                lambda: None,
//            };
//            base_spawn(vm, &ident, args)
//        }
//        Term::BoundLambda { module, fun_name, arity, lambda, bound_env } => {
//            let ident = FunctionIdent {
//                module: module.clone(),
//                name: fun_name.clone(),
//                arity: *arity,
//                lambda: Some(*lambda),
//            };
//            args.insert(0, Term::LambdaEnv(bound_env.clone()));
//            base_spawn(vm, &ident, args)
//        },
//        _ => panic!(),
//    }
//}
//
//fn base_monitor(vm: &VMState, proc: &mut ProcessContext, other: Pid) -> Reference {
//    let monitor_ref = vm.ref_gen.borrow_mut().next();
//    let mut watches = vm.watches.borrow_mut();
//
//    if !watches.contains_key(&other) {
//        watches.insert(other, Vec::new());
//    }
//    let for_proc = watches.get_mut(&other).unwrap();
//
//    for_proc.push((proc.pid, WatchType::Monitor(monitor_ref)));
//
//    monitor_ref
//}
//
//fn spawn_1(vm: &VMState, _proc: &mut ProcessContext, args: &[Rc<Term>]) -> NativeReturn {
//    assert!(args.len() == 1);
//    let fun_term = &*args[0];
//
//    let new_pid = base_spawn_term(vm, fun_term, vec![]);
//    NativeReturn::Return { term: Term::Pid(new_pid) }
//}
//
//fn spawn_monitor_1(vm: &VMState, proc: &mut ProcessContext, args: &[Rc<Term>]) -> NativeReturn {
//    assert!(args.len() == 1);
//    let fun_term = &*args[0];
//
//    let new_pid = base_spawn_term(vm, fun_term, vec![]);
//    let monitor_ref = base_monitor(vm, proc, new_pid);
//
//    let term = Term::Tuple(vec![
//        Term::Pid(new_pid),
//        Term::Reference(monitor_ref),
//    ]);
//    NativeReturn::Return { term: term }
//}
//
//fn monitor_2(vm: &VMState, proc: &mut ProcessContext, args: &[Rc<Term>]) -> NativeReturn {
//    assert!(args.len() == 2);
//    if args[0].erl_eq(&Term::new_atom("process")) {
//        if let Term::Pid(pid) = args[1] {
//            let monitor_ref = base_monitor(vm, proc, pid);
//            NativeReturn::Return { term: Term::Reference(monitor_ref) }
//        } else {
//            NativeReturn::Throw
//        }
//    } else {
//        unimplemented!()
//    }
//}

fn not(_vm: &VMState, _proc: &mut ProcessContext, args: &[Rc<Term>]) -> NativeReturn {
    assert!(args.len() == 1);
    if let Some(b) = args[0].as_boolean() {
        NativeReturn::Return { term: Term::new_bool(!b).into() }
    } else {
        NativeReturn::Throw
    }
}

fn is_binary(_vm: &VMState, _proc: &mut ProcessContext, args: &[Rc<Term>]) -> NativeReturn {
    assert!(args.len() == 1);
    let a1 = &*args[0];

    match a1 {
        Term::Binary(_) => NativeReturn::Return { term: Term::new_bool(true).into() },
        _ => NativeReturn::Return { term: Term::new_bool(false).into() },
    }
}

fn atom_to_list(_vm: &VMState, _proc: &mut ProcessContext, args: &[Rc<Term>]) -> NativeReturn {
    assert!(args.len() == 1);
    let a1 = &*args[0];

    match a1 {
        Term::Atom(atom) => {
            let chars: Vec<_> = atom.as_str()
                .chars()
                .map(|c| Term::new_i64(c as i64).into())
                .collect();
            NativeReturn::Return { term: Term::slice_to_list(&chars, Term::Nil.into()).into() }
        }
        _ => NativeReturn::Throw,
    }
}

fn less_than_or_equal(_vm: &VMState, _proc: &mut ProcessContext, args: &[Rc<Term>]) -> NativeReturn {
    assert!(args.len() == 2);
    let a1 = &*args[0];
    let a2 = &*args[1];
    let ord = a1.erl_ord(a2);
    NativeReturn::Return { term: Term::new_bool(ord != std::cmp::Ordering::Greater).into() }
}
fn less_than(_vm: &VMState, _proc: &mut ProcessContext, args: &[Rc<Term>]) -> NativeReturn {
    assert!(args.len() == 2);
    let a1 = &*args[0];
    let a2 = &*args[1];
    let ord = a1.erl_ord(a2);
    NativeReturn::Return { term: Term::new_bool(ord == std::cmp::Ordering::Less).into() }
}
fn greater_than_or_equal(_vm: &VMState, _proc: &mut ProcessContext, args: &[Rc<Term>]) -> NativeReturn {
    assert!(args.len() == 2);
    let a1 = &*args[0];
    let a2 = &*args[1];
    let ord = a1.erl_ord(a2);
    NativeReturn::Return { term: Term::new_bool(ord != std::cmp::Ordering::Less).into() }
}

fn setelement(_vm: &VMState, _proc: &mut ProcessContext, args: &[Rc<Term>]) -> NativeReturn {
    assert!(args.len() == 3);
    let idx = if let Some(num) = args[0].as_usize() { num } else {
        return NativeReturn::Throw;
    };
    let value = args[2].clone();
    if let Term::Tuple(vals) = &*args[1] {
        if idx == 0 || idx > vals.len() {
            NativeReturn::Throw
        } else {
            let mut vals = vals.clone();
            vals[idx-1] = value;
            NativeReturn::Return { term: Term::Tuple(vals).into() }
        }
    } else {
        NativeReturn::Throw
    }
}
fn element(_vm: &VMState, _proc: &mut ProcessContext, args: &[Rc<Term>]) -> NativeReturn {
    assert!(args.len() == 2);
    let idx = if let Some(num) = args[0].as_usize() { num } else {
        return NativeReturn::Throw;
    };
    if let Term::Tuple(vals) = &*args[1] {
        if idx == 0 || idx > vals.len() {
            NativeReturn::Throw
        } else {
            NativeReturn::Return { term: vals[idx-1].clone() }
        }
    } else {
        NativeReturn::Throw
    }
}

fn erl_self(_vm: &VMState, proc: &mut ProcessContext, args: &[Rc<Term>]) -> NativeReturn {
    assert!(args.len() == 0);
    NativeReturn::Return { term: Term::Pid(proc.pid).into() }
}

//fn process_flag(vm: &VMState, proc: &mut ProcessContext, args: &[Rc<Term>]) -> NativeReturn {
//    assert!(args.len() == 2);
//    if args[0].erl_eq(&Term::new_atom("trap_exit")) {
//        let mut mailboxes = vm.mailboxes.borrow_mut();
//        let mailbox = &mut mailboxes.get_mut(&proc.pid).unwrap();
//        let old_trap_exits = mailbox.get_trap_exits();
//        mailbox.set_trap_exits(args[1].as_boolean().unwrap());
//        NativeReturn::Return { term: Term::new_bool(old_trap_exits).into() }
//    } else {
//        unimplemented!()
//    }
//}

pub fn make_erlang() -> NativeModule {
    let mut module = NativeModule::new(Symbol::intern("erlang"));
    module.add_fun(Symbol::intern("+"), 2, Box::new(add));
    module.add_fun(Symbol::intern("-"), 2, Box::new(sub));
    module.add_fun(Symbol::intern("*"), 2, Box::new(mul));
    module.add_fun(Symbol::intern("/"), 2, Box::new(div));
    module.add_fun(Symbol::intern("abs"), 1, Box::new(abs));
    //module.add_fun(Symbol::intern("++"), 2, Box::new(list_append));
    module.add_fun(Symbol::intern("--"), 2, Box::new(list_subtract));
    module.add_fun(Symbol::intern("=:="), 2, Box::new(exact_eq));
    module.add_fun(Symbol::intern("=<"), 2, Box::new(less_than_or_equal));
    module.add_fun(Symbol::intern("<"), 2, Box::new(less_than));
    module.add_fun(Symbol::intern(">="), 2, Box::new(greater_than_or_equal));
    module.add_fun(Symbol::intern("is_list"), 1, Box::new(is_list));
    module.add_fun(Symbol::intern("is_atom"), 1, Box::new(is_atom));
    module.add_fun(Symbol::intern("is_binary"), 1, Box::new(is_binary));
    module.add_fun(Symbol::intern("is_integer"), 1, Box::new(is_integer));
    module.add_fun(Symbol::intern("and"), 2, Box::new(and));
    module.add_fun(Symbol::intern("or"), 2, Box::new(or));
    module.add_fun(Symbol::intern("tuple_size"), 1, Box::new(tuple_size));
    module.add_fun(Symbol::intern("is_function"), 1, Box::new(is_function));
    module.add_fun(Symbol::intern("is_function"), 2, Box::new(is_function));
    //module.add_fun(Symbol::intern("spawn_monitor"), 1, Box::new(spawn_monitor_1));
    module.add_fun(Symbol::intern("not"), 1, Box::new(not));
    module.add_fun(Symbol::intern("atom_to_list"), 1, Box::new(atom_to_list));
    module.add_fun(Symbol::intern("setelement"), 3, Box::new(setelement));
    module.add_fun(Symbol::intern("element"), 2, Box::new(element));
    module.add_fun(Symbol::intern("self"), 0, Box::new(erl_self));
    //module.add_fun(Symbol::intern("spawn"), 1, Box::new(spawn_1));
    //module.add_fun(Symbol::intern("monitor"), 2, Box::new(monitor_2));
    //module.add_fun(Symbol::intern("process_flag"), 2, Box::new(process_flag));
    module
}
