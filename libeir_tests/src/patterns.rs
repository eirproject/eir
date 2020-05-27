use std::rc::Rc;

use super::lower;

use libeir_intern::{Ident, Symbol};
use libeir_ir::FunctionIdent;
use libeir_passes::PassManager;
use libeir_syntax_erl::ParseConfig;

use libeir_interpreter::{ErlEq, Term, VMState};

#[test]
fn test_pattern_equality() {
    let _ = env_logger::try_init();

    let mut eir_mod = lower(
        "
-module(woo).

basic_pat(A, A) -> true;
basic_pat(_, _) -> false.
",
        ParseConfig::default(),
    )
    .unwrap();

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
    let _ = env_logger::try_init();

    let mut eir_mod = lower(
        "
-module(woo).

woo({1, 2}) -> 1;
woo({1, 2, 3}) -> 2;
woo(_) -> 3.
",
        ParseConfig::default(),
    )
    .unwrap();

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
    let _ = env_logger::try_init();

    let mut eir_mod = lower(
        "
-module(woo).

woo([6 | 7]) -> 1;
woo([1, 2]) -> 2;
woo([]) -> 3;
woo(_) -> 4.
",
        ParseConfig::default(),
    )
    .unwrap();

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
        let arg = Term::slice_to_list(
            &[
                Term::Integer(1.into()).into(),
                Term::Integer(2.into()).into(),
            ],
            Term::Nil.into(),
        );
        assert!(
            vm.call(&fun, &[Rc::try_unwrap(arg).unwrap()])
                .unwrap()
                .as_i64()
                == Some(2)
        );
    }
}

#[test]
fn test_fun_atom_pattern() {
    let _ = env_logger::try_init();

    let mut eir_mod = lower(
        "
-module(woo).

woo(abc) -> 1;
woo(def) -> 2.
",
        ParseConfig::default(),
    )
    .unwrap();

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

    assert!(
        vm.call(&fun, &[Term::Atom(Symbol::intern("abc")).into()])
            .unwrap()
            .as_i64()
            == Some(1)
    );

    assert!(
        vm.call(&fun, &[Term::Atom(Symbol::intern("def")).into()])
            .unwrap()
            .as_i64()
            == Some(2)
    );

    {
        let res = vm.call(&fun, &[Term::Nil.into()]).err().unwrap();
        assert!(res.0.erl_eq(&Term::Atom(Symbol::intern("error")).into()));
        assert!(res
            .1
            .erl_eq(&Term::Atom(Symbol::intern("function_clause")).into()));
    }
}

#[test]
fn test_case_atom_pattern() {
    let _ = env_logger::try_init();

    let mut eir_mod = lower(
        "
-module(woo).

woo(A) -> case A of
    abc -> 1;
    def -> 2
end.
",
        ParseConfig::default(),
    )
    .unwrap();

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

    assert!(
        vm.call(&fun, &[Term::Atom(Symbol::intern("abc")).into()])
            .unwrap()
            .as_i64()
            == Some(1)
    );

    assert!(
        vm.call(&fun, &[Term::Atom(Symbol::intern("def")).into()])
            .unwrap()
            .as_i64()
            == Some(2)
    );

    {
        let res = vm
            .call(&fun, &[Term::Atom(Symbol::intern("aaa")).into()])
            .err()
            .unwrap();
        assert!(res.0.erl_eq(&Term::Atom(Symbol::intern("error")).into()));
        assert!(res.1.erl_eq(&Term::Tuple(vec![
            Term::Atom(Symbol::intern("case_clause")).into(),
            Term::Atom(Symbol::intern("aaa")).into(),
        ])));
    }
}
