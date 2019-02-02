use ::{ FunctionIdent };
use ::vm::{ VMState, WatchType };
use ::module::NativeModule;
use ::term::{ Term, Pid };
use ::process::{ CallReturn, ProcessContext };

use ::num_bigint::{ BigInt, Sign };

use term::{ ErlEq, ErlExactEq, ErlOrd };

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

fn add(_vm: &VMState, _proc: &mut ProcessContext, args: &[Term]) -> CallReturn {
    // TODO: Verify semantics
    println!("{:?}", args);

    if args.len() != 2 {
        return CallReturn::Throw;
    }
    let a1 = &args[0];
    let a2 = &args[1];

    match (a1, a2) {
        (Term::Integer(ref i1), Term::Integer(ref i2)) =>
            CallReturn::Return { term: Term::Integer(i1 + i2) },
        (Term::Integer(ref i1), Term::Float(f2)) => {
            let f1 = bignum_to_f64(i1);
            if f1 == None {
                CallReturn::Throw
            } else {
                CallReturn::Return { term: Term::Float(f1.unwrap() + f2) }
            }
        }
        (Term::Float(f1), Term::Integer(ref i2)) => {
            let f2 = bignum_to_f64(i2);
            if f2 == None {
                CallReturn::Throw
            } else {
                CallReturn::Return { term: Term::Float(f1 + f2.unwrap()) }
            }
        }
        (Term::Float(f1), Term::Float(f2)) => {
            CallReturn::Return { term: Term::Float(f1 + f2) }
        }
        _ => CallReturn::Throw,
    }
}

fn sub(_vm: &VMState, _proc: &mut ProcessContext, args: &[Term]) -> CallReturn {
    if args.len() != 2 {
        return CallReturn::Throw;
    }
    let a1 = &args[0];
    let a2 = &args[1];

    match (a1, a2) {
        (Term::Integer(ref i1), Term::Integer(ref i2)) =>
            CallReturn::Return { term: Term::Integer(i1 - i2) },
        _ => unimplemented!(),
    }
}

fn mul(_vm: &VMState, _proc: &mut ProcessContext, args: &[Term]) -> CallReturn {
    if args.len() != 2 {
        return CallReturn::Throw;
    }
    let a1 = &args[0];
    let a2 = &args[1];

    match (a1, a2) {
        (Term::Integer(ref i1), Term::Integer(ref i2)) =>
            CallReturn::Return { term: Term::Integer(i1 * i2) },
        _ => unimplemented!(),
    }
}

fn is_list(_vm: &VMState, _proc: &mut ProcessContext, args: &[Term]) -> CallReturn {
    if args.len() != 1 {
        return CallReturn::Throw;
    }
    let a1 = &args[0];

    match a1 {
        Term::List(_, _) => CallReturn::Return { term: Term::new_atom("true") },
        Term::Nil => CallReturn::Return { term: Term::new_atom("true") },
        _ => CallReturn::Return { term: Term::new_atom("false") },
    }
}

fn is_atom(_vm: &VMState, _proc: &mut ProcessContext, args: &[Term]) -> CallReturn {
    if args.len() != 1 {
        return CallReturn::Throw;
    }
    let a1 = &args[0];

    match a1 {
        Term::Atom(_) => CallReturn::Return { term: Term::new_bool(true) },
        _ => CallReturn::Return { term: Term::new_bool(false) },
    }
}

fn is_integer(_vm: &VMState, _proc: &mut ProcessContext, args: &[Term]) -> CallReturn {
    assert!(args.len() == 1);
    let a1 = &args[0];
    match a1 {
        Term::Integer(_) => CallReturn::Return { term: Term::new_bool(true) },
        _ => CallReturn::Return { term: Term::new_bool(false) },
    }
}

fn list_append(_vm: &VMState, _proc: &mut ProcessContext, args: &[Term]) -> CallReturn {
    // TODO: Validate semantics
    assert!(args.len() == 2);
    match (&args[0], &args[1]) {
        (Term::Nil, Term::Nil) => CallReturn::Return { term: Term::Nil },
        (Term::Nil, Term::List(_, _)) => CallReturn::Return { term: args[1].clone() },
        (Term::List(_, ref tail), Term::Nil) if tail.erl_eq(&Term::Nil)
            => CallReturn::Return { term: args[0].clone() },
        (Term::List(ref _f_head, ref _f_tail), Term::List(ref b_head, ref b_tail)) => {
            let (mut f_head_terms, f_tail_term) = args[0].as_inproper_list();
            if let Term::Nil = f_tail_term {
                f_head_terms.extend(b_head.iter().cloned());
                CallReturn::Return { term: Term::List(f_head_terms, b_tail.clone()) }
            } else {
                CallReturn::Throw
            }
        }
        _ => CallReturn::Throw,
    }
}

fn exact_eq(_vm: &VMState, _proc: &mut ProcessContext, args: &[Term]) -> CallReturn {
    assert!(args.len() == 2);
    CallReturn::Return { term: Term::new_bool(args[0].erl_exact_eq(&args[1])) }
}

fn and(_vm: &VMState, _proc: &mut ProcessContext, args: &[Term]) -> CallReturn {
    assert!(args.len() == 2);
    if let (Some(a1), Some(a2)) = (args[0].as_boolean(), args[1].as_boolean()) {
        CallReturn::Return { term: Term::new_bool(a1 && a2) }
    } else {
        CallReturn::Throw
    }
}

fn or(_vm: &VMState, _proc: &mut ProcessContext, args: &[Term]) -> CallReturn {
    assert!(args.len() == 2);
    if let (Some(a1), Some(a2)) = (args[0].as_boolean(), args[1].as_boolean()) {
        CallReturn::Return { term: Term::new_bool(a1 || a2) }
    } else {
        CallReturn::Throw
    }
}

fn tuple_size(_vm: &VMState, _proc: &mut ProcessContext, args: &[Term]) -> CallReturn {
    assert!(args.len() == 1);
    if let Term::Tuple(ref terms) = &args[0] {
        CallReturn::Return { term: Term::new_i64(terms.len() as i64) }
    } else {
        CallReturn::Throw
    }
}

fn is_function(_vm: &VMState, _proc: &mut ProcessContext, args: &[Term]) -> CallReturn {
    assert!(args.len() == 1 || args.len() == 2);

    let arity_ref = if args.len() == 2 {
        if let Some(int) = args[1].as_i64() {
            Some(int)
        } else {
            return CallReturn::Throw;
        }
    } else {
        None
    };

    if let Term::CapturedFunction { arity, .. } = args[0] {
        let res = arity_ref.map(|a| a == arity as i64).unwrap_or(true);
        CallReturn::Return { term: Term::new_bool(res) }
    } else if let Term::BoundLambda { arity, .. } = args[0] {
        let res = arity_ref.map(|a| a == arity as i64).unwrap_or(true);
        CallReturn::Return { term: Term::new_bool(res) }
    } else {
        CallReturn::Return { term: Term::new_bool(false) }
    }
}

fn spawn_monitor_1(vm: &VMState, proc: &mut ProcessContext, args: &[Term]) -> CallReturn {
    assert!(args.len() == 1);
    let fun_term = &args[0];

    let new_pid = {
        let mut processes = vm.processes.borrow();
        Pid(processes.len())
    };

    let process = ProcessContext::new(new_pid);

    let process = match fun_term {
        Term::CapturedFunction { module, fun_name, arity } => {
            let ident = FunctionIdent {
                name: fun_name.clone(),
                arity: *arity,
                lambda: None,
            };

            let orig_pid = ::trace::get_pid();
            ::trace::set_pid(new_pid);
            let frame = process.make_call_stackframe(
                vm,
                module.clone(),
                ident,
                vec![]
            );
            ::trace::set_pid(orig_pid);

            let stack_i = process.stack.clone();
            let mut stack = stack_i.borrow_mut();
            stack.push(frame);
            process
        }
        Term::BoundLambda { module, fun_name, arity, lambda, bound_env } => {
            let ident = FunctionIdent {
                name: fun_name.clone(),
                arity: *arity,
                lambda: Some(*lambda),
            };

            let orig_pid = ::trace::get_pid();
            ::trace::set_pid(new_pid);
            let frame = process.make_call_stackframe(
                vm,
                module.clone(),
                ident,
                vec![Term::LambdaEnv(bound_env.clone())]
            );
            ::trace::set_pid(orig_pid);

            let stack_i = process.stack.clone();
            let mut stack = stack_i.borrow_mut();
            stack.push(frame);
            process
        },
        _ => panic!(),
    };

    let mut processes = vm.processes.borrow_mut();
    processes.push(Rc::new(RefCell::new(process)));

    let monitor_ref = vm.ref_gen.borrow_mut().next();
    let mut watches = vm.watches.borrow_mut();

    if !watches.contains_key(&new_pid) {
        watches.insert(new_pid, Vec::new());
    }
    let for_proc = watches.get_mut(&new_pid).unwrap();

    for_proc.push((proc.pid, WatchType::Monitor(monitor_ref)));

    let term = Term::Tuple(vec![
        Term::Pid(new_pid),
        Term::Reference(monitor_ref),
    ]);
    CallReturn::Return { term: term }
}

fn not(_vm: &VMState, _proc: &mut ProcessContext, args: &[Term]) -> CallReturn {
    assert!(args.len() == 1);
    if let Some(b) = args[0].as_boolean() {
        CallReturn::Return { term: Term::new_bool(!b) }
    } else {
        CallReturn::Throw
    }
}

fn is_binary(_vm: &VMState, _proc: &mut ProcessContext, args: &[Term]) -> CallReturn {
    assert!(args.len() == 1);
    let a1 = &args[0];

    match a1 {
        Term::Binary(_) => CallReturn::Return { term: Term::new_bool(true) },
        _ => CallReturn::Return { term: Term::new_bool(false) },
    }
}

fn atom_to_list(_vm: &VMState, _proc: &mut ProcessContext, args: &[Term]) -> CallReturn {
    assert!(args.len() == 1);
    let a1 = &args[0];

    match a1 {
        Term::Atom(atom) => {
            let chars: Vec<_> = atom.as_str()
                .chars()
                .map(|c| Term::new_i64(c as i64))
                .collect();
            CallReturn::Return { term: Term::List(chars, Box::new(Term::Nil)) }
        }
        _ => CallReturn::Throw,
    }
}

fn less_than_or_equal(_vm: &VMState, _proc: &mut ProcessContext, args: &[Term]) -> CallReturn {
    assert!(args.len() == 2);
    let a1 = &args[0];
    let a2 = &args[1];
    let ord = a1.erl_ord(a2);
    CallReturn::Return { term: Term::new_bool(ord != std::cmp::Ordering::Greater) }
}

fn setelement(_vm: &VMState, _proc: &mut ProcessContext, args: &[Term]) -> CallReturn {
    assert!(args.len() == 3);
    let idx = if let Some(num) = args[0].as_usize() { num } else {
        return CallReturn::Throw;
    };
    let value = args[2].clone();
    if let Term::Tuple(vals) = &args[1] {
        if idx > vals.len() {
            CallReturn::Throw
        } else {
            let mut vals = vals.clone();
            vals[idx] = value;
            CallReturn::Return { term: Term::Tuple(vals) }
        }
    } else {
        CallReturn::Throw
    }
}

pub fn make_erlang() -> NativeModule {
    let mut module = NativeModule::new("erlang".to_string());
    module.add_fun("+".to_string(), 2, Box::new(add));
    module.add_fun("-".to_string(), 2, Box::new(sub));
    module.add_fun("*".to_string(), 2, Box::new(mul));
    module.add_fun("++".to_string(), 2, Box::new(list_append));
    module.add_fun("=:=".to_string(), 2, Box::new(exact_eq));
    module.add_fun("=<".to_string(), 2, Box::new(less_than_or_equal));
    module.add_fun("is_list".to_string(), 1, Box::new(is_list));
    module.add_fun("is_atom".to_string(), 1, Box::new(is_atom));
    module.add_fun("is_binary".to_string(), 1, Box::new(is_binary));
    module.add_fun("is_integer".to_string(), 1, Box::new(is_integer));
    module.add_fun("and".to_string(), 2, Box::new(and));
    module.add_fun("or".to_string(), 2, Box::new(or));
    module.add_fun("tuple_size".to_string(), 1, Box::new(tuple_size));
    module.add_fun("is_function".to_string(), 1, Box::new(is_function));
    module.add_fun("is_function".to_string(), 2, Box::new(is_function));
    module.add_fun("spawn_monitor".to_string(), 1, Box::new(spawn_monitor_1));
    module.add_fun("not".to_string(), 1, Box::new(not));
    module.add_fun("atom_to_list".to_string(), 1, Box::new(atom_to_list));
    module.add_fun("setelement".to_string(), 3, Box::new(setelement));
    module
}
