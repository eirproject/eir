use libeir_ir::FunctionIdent;
use libeir_intern::Symbol;

use crate::vm::{ VMState, WatchType };
use crate::module::{ NativeModule, NativeReturn };
use crate::process::{ ProcessContext };

use crate::term::{ Term, Pid, Reference };
use crate::term::{ ErlEq, ErlExactEq, ErlOrd };
use crate::term::{ ListIteratorItem };

use ::rug::{ Integer };
use ::num_traits::{ Signed };

use std::rc::Rc;
use std::cell::RefCell;

fn abs(_vm: &VMState, _proc: &mut ProcessContext, args: &[Rc<Term>]) -> NativeReturn {
    if args.len() != 1 {
        panic!()
    }
    let a1 = &*args[0];

    let ret = match a1 {
        Term::Integer(ref int) => Term::Integer(int.clone().abs()),
        Term::Float(flt) => Term::Float(flt.0.abs().into()),
        _ => panic!(),
    };

    NativeReturn::Return { term: ret.into() }
}

fn add(_vm: &VMState, _proc: &mut ProcessContext, args: &[Rc<Term>]) -> NativeReturn {
    // TODO: Verify semantics

    if args.len() != 2 {
        panic!();
    }
    let a1 = &*args[0];
    let a2 = &*args[1];

    match (a1, a2) {
        (Term::Integer(ref i1), Term::Integer(ref i2)) =>
            NativeReturn::Return { term: Term::Integer(i1.clone() + i2).into() },
        (Term::Integer(ref i1), Term::Float(f2)) => {
            let f1 = i1.to_f64();
            NativeReturn::Return { term: Term::Float((f1 + f2.0).into()).into() }
        }
        (Term::Float(f1), Term::Integer(ref i2)) => {
            let f2 = i2.to_f64();
            NativeReturn::Return { term: Term::Float((f1.0 + f2).into()).into() }
        }
        (Term::Float(f1), Term::Float(f2)) => {
            NativeReturn::Return { term: Term::Float((f1.0 + f2.0).into()).into() }
        }
        _ => NativeReturn::Throw {
            typ: Term::new_atom("error").into(),
            reason: Term::new_atom("badarith").into(),
        },
    }
}

fn sub(_vm: &VMState, _proc: &mut ProcessContext, args: &[Rc<Term>]) -> NativeReturn {
    if args.len() != 2 {
        panic!();
    }
    let a1 = &*args[0];
    let a2 = &*args[1];

    match (a1, a2) {
        (Term::Integer(ref i1), Term::Integer(ref i2)) =>
            NativeReturn::Return { term: Term::Integer(i1.clone() - i2).into() },
        (Term::Integer(ref int), Term::Float(ref flt)) => {
            let flt_c = int.to_f64();
            NativeReturn::Return { term: Term::Float((flt_c - flt.0).into()).into() }
        }
        (Term::Float(ref flt), Term::Integer(ref int)) => {
            let flt_c = int.to_f64();
            NativeReturn::Return { term: Term::Float((flt.0 - flt_c).into()).into() }
        }
        (Term::Float(flt1), Term::Float(flt2)) =>
            NativeReturn::Return { term: Term::Float((flt1.0 - flt2.0).into()).into() },
        _ => NativeReturn::Throw {
            typ: Term::new_atom("error").into(),
            reason: Term::new_atom("badarith").into(),
        },
    }
}

fn invert(_vm: &VMState, _proc: &mut ProcessContext, args: &[Rc<Term>]) -> NativeReturn {
    if args.len() != 1 {
        panic!();
    }
    match &*args[0] {
        Term::Integer(ref i1) =>
            NativeReturn::Return { term: Term::Integer(-i1.clone()).into() },
        Term::Float(ref f1) =>
            NativeReturn::Return { term: Term::Float((-f1.0).into()).into() },
        _ => unimplemented!(),
    }
}

fn mul(_vm: &VMState, _proc: &mut ProcessContext, args: &[Rc<Term>]) -> NativeReturn {
    if args.len() != 2 {
        panic!();
    }
    let a1 = &*args[0];
    let a2 = &*args[1];

    match (a1, a2) {
        (Term::Integer(ref i1), Term::Integer(ref i2)) =>
            NativeReturn::Return { term: Term::Integer(i1.clone() * i2).into() },
        _ => unimplemented!(),
    }
}

fn div(_vm: &VMState, _proc: &mut ProcessContext, args: &[Rc<Term>]) -> NativeReturn {
    if args.len() != 2 {
        panic!();
    }

    let a1 = match &*args[0] {
        Term::Integer(i1) => i1.to_f64(),
        Term::Float(flt) => flt.0,
        _ => panic!(),
    };
    let a2 = match &*args[1] {
        Term::Integer(i1) => i1.to_f64(),
        Term::Float(flt) => flt.0,
        _ => panic!(),
    };

    NativeReturn::Return { term: Term::Float((a1 / a2).into()).into() }
}

fn is_list(_vm: &VMState, _proc: &mut ProcessContext, args: &[Rc<Term>]) -> NativeReturn {
    if args.len() != 1 {
        panic!();
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
        panic!();
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

fn is_pid(_vm: &VMState, _proc: &mut ProcessContext, args: &[Rc<Term>]) -> NativeReturn {
    assert!(args.len() == 1);
    let a1 = &*args[0];
    match a1 {
        Term::Pid(_) => NativeReturn::Return { term: Term::new_bool(true).into() },
        _ => NativeReturn::Return { term: Term::new_bool(false).into() },
    }
}

fn is_tuple(_vm: &VMState, _proc: &mut ProcessContext, args: &[Rc<Term>]) -> NativeReturn {
    assert!(args.len() == 1);
    let a1 = &*args[0];
    match a1 {
        Term::Tuple(_) => NativeReturn::Return { term: Term::new_bool(true).into() },
        _ => NativeReturn::Return { term: Term::new_bool(false).into() },
    }
}

fn is_map(_vm: &VMState, _proc: &mut ProcessContext, args: &[Rc<Term>]) -> NativeReturn {
    assert!(args.len() == 1);
    let a1 = &*args[0];
    match a1 {
        Term::Map(_) => NativeReturn::Return { term: Term::new_bool(true).into() },
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
fn exact_not_eq(_vm: &VMState, _proc: &mut ProcessContext, args: &[Rc<Term>]) -> NativeReturn {
    assert!(args.len() == 2);
    NativeReturn::Return { term: Term::new_bool(!args[0].erl_exact_eq(&*args[1])).into() }
}

fn and(_vm: &VMState, _proc: &mut ProcessContext, args: &[Rc<Term>]) -> NativeReturn {
    assert!(args.len() == 2);
    if let (Some(a1), Some(a2)) = (args[0].as_boolean(), args[1].as_boolean()) {
        NativeReturn::Return { term: Term::new_bool(a1 && a2).into() }
    } else {
        panic!()
    }
}

fn or(_vm: &VMState, _proc: &mut ProcessContext, args: &[Rc<Term>]) -> NativeReturn {
    assert!(args.len() == 2);
    if let (Some(a1), Some(a2)) = (args[0].as_boolean(), args[1].as_boolean()) {
        NativeReturn::Return { term: Term::new_bool(a1 || a2).into() }
    } else {
        panic!()
    }
}

fn tuple_size(_vm: &VMState, _proc: &mut ProcessContext, args: &[Rc<Term>]) -> NativeReturn {
    assert!(args.len() == 1);
    if let Term::Tuple(ref terms) = &*args[0] {
        NativeReturn::Return { term: Term::new_i64(terms.len() as i64).into() }
    } else {
        panic!()
    }
}

fn is_function(_vm: &VMState, _proc: &mut ProcessContext, args: &[Rc<Term>]) -> NativeReturn {
    assert!(args.len() == 1 || args.len() == 2);

    let arity_ref = if args.len() == 2 {
        if let Some(int) = args[1].as_i64() {
            Some(int)
        } else {
            panic!()
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
        panic!()
    }
}

fn is_binary(_vm: &VMState, _proc: &mut ProcessContext, args: &[Rc<Term>]) -> NativeReturn {
    assert!(args.len() == 1);
    let a1 = &*args[0];

    match a1 {
        Term::Binary(_) => NativeReturn::Return { term: Term::new_bool(true).into() },
        Term::BinarySlice { .. } => NativeReturn::Return { term: Term::new_bool(true).into() },
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
        _ => panic!(),
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
fn greater_than(_vm: &VMState, _proc: &mut ProcessContext, args: &[Rc<Term>]) -> NativeReturn {
    assert!(args.len() == 2);
    let a1 = &*args[0];
    let a2 = &*args[1];
    let ord = a1.erl_ord(a2);
    NativeReturn::Return { term: Term::new_bool(ord == std::cmp::Ordering::Greater).into() }
}

fn setelement(_vm: &VMState, _proc: &mut ProcessContext, args: &[Rc<Term>]) -> NativeReturn {
    assert!(args.len() == 3);
    let idx = if let Some(num) = args[0].as_usize() { num } else {
        panic!()
    };
    let value = args[2].clone();
    if let Term::Tuple(vals) = &*args[1] {
        if idx == 0 || idx > vals.len() {
            panic!()
        } else {
            let mut vals = vals.clone();
            vals[idx-1] = value;
            NativeReturn::Return { term: Term::Tuple(vals).into() }
        }
    } else {
        panic!()
    }
}
fn element(_vm: &VMState, _proc: &mut ProcessContext, args: &[Rc<Term>]) -> NativeReturn {
    assert!(args.len() == 2);
    let idx = if let Some(num) = args[0].as_usize() { num } else {
        panic!()
    };
    if let Term::Tuple(vals) = &*args[1] {
        if idx == 0 || idx > vals.len() {
            panic!()
        } else {
            NativeReturn::Return { term: vals[idx-1].clone() }
        }
    } else {
        panic!()
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

fn put(_vm: &VMState, proc: &mut ProcessContext, args: &[Rc<Term>]) -> NativeReturn {
    assert!(args.len() == 2);
    if let Some(entry) = proc.dict.iter_mut().find(|e| e.0.erl_exact_eq(&args[0])) {
        let old = entry.1.clone();
        entry.1 = args[1].clone();
        NativeReturn::Return { term: old }
    } else {
        proc.dict.push((args[0].clone(), args[1].clone()));
        NativeReturn::Return { term: Term::new_atom("undefined").into() }
    }
}

fn get(_vm: &VMState, proc: &mut ProcessContext, args: &[Rc<Term>]) -> NativeReturn {
    assert!(args.len() == 1);
    if let Some(entry) = proc.dict.iter().find(|e| e.0.erl_exact_eq(&args[0])) {
        NativeReturn::Return { term: entry.1.clone() }
    } else {
        NativeReturn::Return { term: Term::new_atom("undefined").into() }
    }
}

fn erase(_vm: &VMState, proc: &mut ProcessContext, args: &[Rc<Term>]) -> NativeReturn {
    assert!(args.len() == 1);
    let idx = proc.dict.iter().enumerate()
        .find(|e| (e.1).0.erl_exact_eq(&args[0]))
        .map(|(idx, _)| idx);
    if let Some(entry) = idx {
        let (_key, val) = proc.dict.remove(entry);
        NativeReturn::Return { term: val }
    } else {
        NativeReturn::Return { term: Term::new_atom("undefined").into() }
    }
}

fn length(_vm: &VMState, proc: &mut ProcessContext, args: &[Rc<Term>]) -> NativeReturn {
    assert!(args.len() == 1);
    let mut len = 0;
    for item in Term::list_iter(&args[0]) {
        match item {
            ListIteratorItem::Elem(_) => {
                len += 1;
            }
            ListIteratorItem::Tail(tail) => {
                if tail.erl_eq(&Term::Nil) {
                    return NativeReturn::Return { term: Term::new_i64(len as i64).into() };
                } else {
                    return NativeReturn::Throw {
                        typ: Term::new_atom("error").into(),
                        reason: Term::new_atom("badarg").into(),
                    };
                }
            }
        }
    }
    unreachable!()
}

fn hd(_vm: &VMState, _proc: &mut ProcessContext, args: &[Rc<Term>]) -> NativeReturn {
    if args.len() != 1 {
        panic!();
    }
    let a1 = &*args[0];

    match a1 {
        Term::ListCell(hd, _) => NativeReturn::Return { term: hd.clone() },
        _ => NativeReturn::Throw {
            typ: Term::new_atom("error").into(),
            reason: Term::new_atom("badarg").into(),
        },
    }
}
fn tl(_vm: &VMState, _proc: &mut ProcessContext, args: &[Rc<Term>]) -> NativeReturn {
    if args.len() != 1 {
        panic!();
    }
    let a1 = &*args[0];

    match a1 {
        Term::ListCell(_, tl) => NativeReturn::Return { term: tl.clone() },
        _ => NativeReturn::Throw {
            typ: Term::new_atom("error").into(),
            reason: Term::new_atom("badarg").into(),
        },
    }
}

fn map_size(_vm: &VMState, _proc: &mut ProcessContext, args: &[Rc<Term>]) -> NativeReturn {
    assert!(args.len() == 1);
    if let Some(map) = args[0].as_map() {
        NativeReturn::Return {
            term: Term::new_usize(map.len()).into(),
        }
    } else {
        unimplemented!()
    }
}

pub fn make_erlang() -> NativeModule {
    let mut module = NativeModule::new(Symbol::intern("erlang"));
    module.add_fun(Symbol::intern("+"), 2, Box::new(add));
    module.add_fun(Symbol::intern("-"), 1, Box::new(invert));
    module.add_fun(Symbol::intern("-"), 2, Box::new(sub));
    module.add_fun(Symbol::intern("*"), 2, Box::new(mul));
    module.add_fun(Symbol::intern("/"), 2, Box::new(div));
    module.add_fun(Symbol::intern("abs"), 1, Box::new(abs));
    //module.add_fun(Symbol::intern("++"), 2, Box::new(list_append));
    module.add_fun(Symbol::intern("--"), 2, Box::new(list_subtract));
    module.add_fun(Symbol::intern("=:="), 2, Box::new(exact_eq));
    module.add_fun(Symbol::intern("=/="), 2, Box::new(exact_not_eq));
    module.add_fun(Symbol::intern("=<"), 2, Box::new(less_than_or_equal));
    module.add_fun(Symbol::intern("<"), 2, Box::new(less_than));
    module.add_fun(Symbol::intern(">="), 2, Box::new(greater_than_or_equal));
    module.add_fun(Symbol::intern(">"), 2, Box::new(greater_than));
    module.add_fun(Symbol::intern("is_list"), 1, Box::new(is_list));
    module.add_fun(Symbol::intern("is_atom"), 1, Box::new(is_atom));
    module.add_fun(Symbol::intern("is_binary"), 1, Box::new(is_binary));
    module.add_fun(Symbol::intern("is_integer"), 1, Box::new(is_integer));
    module.add_fun(Symbol::intern("is_pid"), 1, Box::new(is_pid));
    module.add_fun(Symbol::intern("is_tuple"), 1, Box::new(is_tuple));
    module.add_fun(Symbol::intern("is_map"), 1, Box::new(is_map));
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
    module.add_fun(Symbol::intern("length"), 1, Box::new(length));
    module.add_fun(Symbol::intern("self"), 0, Box::new(erl_self));
    module.add_fun(Symbol::intern("put"), 2, Box::new(put));
    module.add_fun(Symbol::intern("get"), 1, Box::new(get));
    module.add_fun(Symbol::intern("erase"), 1, Box::new(erase));
    module.add_fun(Symbol::intern("hd"), 1, Box::new(hd));
    module.add_fun(Symbol::intern("tl"), 1, Box::new(tl));
    module.add_fun(Symbol::intern("map_size"), 1, Box::new(map_size));
    //module.add_fun(Symbol::intern("spawn"), 1, Box::new(spawn_1));
    //module.add_fun(Symbol::intern("monitor"), 2, Box::new(monitor_2));
    //module.add_fun(Symbol::intern("process_flag"), 2, Box::new(process_flag));
    module
}
