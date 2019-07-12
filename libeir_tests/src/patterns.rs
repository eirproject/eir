use std::rc::Rc;

use super::{ lower, write_dot };

use libeir_ir::{ FunctionIdent };
use libeir_syntax_erl::{ ParseConfig };
use libeir_intern::Ident;
use libeir_passes::PassManager;

use libeir_interpreter::{ VMState, Term };

#[test]
fn test_pattern_equality() {
    let mut eir_mod = lower("
-module(woo).

basic_pat(A, A) -> true;
basic_pat(_, _) -> false.
", ParseConfig::default()).unwrap();

    let mut pass_manager = PassManager::default();
    pass_manager.run(&mut eir_mod);

    let fun = FunctionIdent {
        module: Ident::from_str("woo"),
        name: Ident::from_str("basic_pat"),
        arity: 2,
    };

    let mut vm = VMState::new();
    vm.add_builtin_modules();
    vm.add_erlang_module(eir_mod);

    assert!(vm.call(&fun, &[1.into(), 1.into()]).unwrap().as_boolean() == Some(true));
    assert!(vm.call(&fun, &[1.into(), 2.into()]).unwrap().as_boolean() == Some(false));
}

#[test]
fn test_tuple_pattern() {
    let mut eir_mod = lower("
-module(woo).

woo({1, 2}) -> 1;
woo({1, 2, 3}) -> 2;
woo(_) -> 3.
", ParseConfig::default()).unwrap();

    let mut pass_manager = PassManager::default();
    pass_manager.run(&mut eir_mod);

    let fun = FunctionIdent {
        module: Ident::from_str("woo"),
        name: Ident::from_str("woo"),
        arity: 1,
    };

    //write_dot(&eir_mod, Some(fun.clone()));

    let mut vm = VMState::new();
    vm.add_builtin_modules();
    vm.add_erlang_module(eir_mod);

    assert!(vm.call(&fun, &[1.into()]).unwrap().as_i64() == Some(3));

    {
        let arg = Term::Tuple(vec![
            Term::Integer(1.into()).into(),
            Term::Integer(2.into()).into(),
        ]);
        assert!(vm.call(&fun, &[arg.into()]).unwrap().as_i64() == Some(1));
    }

    {
        let arg = Term::Tuple(vec![
            Term::Integer(2.into()).into(),
            Term::Integer(2.into()).into(),
        ]);
        assert!(vm.call(&fun, &[arg.into()]).unwrap().as_i64() == Some(3));
    }

    {
        let arg = Term::Tuple(vec![
            Term::Integer(1.into()).into(),
            Term::Integer(2.into()).into(),
            Term::Integer(3.into()).into(),
        ]);
        assert!(vm.call(&fun, &[arg.into()]).unwrap().as_i64() == Some(2));
    }

    {
        let arg = Term::Tuple(vec![
            Term::Integer(1.into()).into(),
            Term::Integer(2.into()).into(),
            Term::Integer(2.into()).into(),
        ]);
        assert!(vm.call(&fun, &[arg.into()]).unwrap().as_i64() == Some(3));
    }

}

#[test]
fn test_list_pattern() {
    let mut eir_mod = lower("
-module(woo).

woo([6 | 7]) -> 1;
woo([1, 2]) -> 2;
woo([]) -> 3;
woo(_) -> 4.
", ParseConfig::default()).unwrap();

    let mut pass_manager = PassManager::default();
    pass_manager.run(&mut eir_mod);

    let fun = FunctionIdent {
        module: Ident::from_str("woo"),
        name: Ident::from_str("woo"),
        arity: 1,
    };

    let mut vm = VMState::new();
    vm.add_builtin_modules();
    vm.add_erlang_module(eir_mod);

    assert!(vm.call(&fun, &[1.into()]).unwrap().as_i64() == Some(4));

    {
        let arg = Term::slice_to_list(&[
            Term::Integer(1.into()).into(),
            Term::Integer(2.into()).into(),
        ], Term::Nil.into());
        assert!(vm.call(&fun, &[Rc::try_unwrap(arg).unwrap()]).unwrap().as_i64() == Some(2));
    }

}
