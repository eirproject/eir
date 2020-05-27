use super::{lower, write_dot};

use libeir_intern::{Ident, Symbol};
use libeir_ir::FunctionIdent;
use libeir_passes::PassManager;
use libeir_syntax_erl::ParseConfig;

use libeir_interpreter::{ErlEq, Term, VMState};

#[test]
fn record_creation() {
    let mut eir_mod = lower(
        "
-module(woo).

-record(person, {name, phone, address}).
-record(alien, {name, planet = zorgon}).

create_1(A) -> #person{name=A}.
create_2(A) -> #person{address=A}.
create_3(A) -> #person{address=A, phone=12}.

create_4(A) -> #alien{name=A}.
create_5(A, B) -> #alien{name=A, planet=B}.
",
        ParseConfig::default(),
    )
    .unwrap();

    let mut pass_manager = PassManager::default();
    pass_manager.run(&mut eir_mod);

    let fun = FunctionIdent {
        module: Ident::from_str("woo"),
        name: Ident::from_str("create_1"),
        arity: 1,
    };

    write_dot(&eir_mod, Some(fun.clone()));

    let mut vm = VMState::new();
    vm.add_builtin_modules();
    vm.add_erlang_module(eir_mod);

    {
        let fun = FunctionIdent {
            module: Ident::from_str("woo"),
            name: Ident::from_str("create_1"),
            arity: 1,
        };

        let out = Term::Tuple(vec![
            Term::Atom(Symbol::intern("person")).into(),
            Term::Integer(1.into()).into(),
            Term::Atom(Symbol::intern("undefined")).into(),
            Term::Atom(Symbol::intern("undefined")).into(),
        ]);
        assert!(vm.call(&fun, &[1.into()]).unwrap().erl_eq(&out));
    }

    {
        let fun = FunctionIdent {
            module: Ident::from_str("woo"),
            name: Ident::from_str("create_2"),
            arity: 1,
        };

        let out = Term::Tuple(vec![
            Term::Atom(Symbol::intern("person")).into(),
            Term::Atom(Symbol::intern("undefined")).into(),
            Term::Atom(Symbol::intern("undefined")).into(),
            Term::Integer(1.into()).into(),
        ]);
        assert!(vm.call(&fun, &[1.into()]).unwrap().erl_eq(&out));
    }

    {
        let fun = FunctionIdent {
            module: Ident::from_str("woo"),
            name: Ident::from_str("create_3"),
            arity: 1,
        };

        let out = Term::Tuple(vec![
            Term::Atom(Symbol::intern("person")).into(),
            Term::Atom(Symbol::intern("undefined")).into(),
            Term::Integer(12.into()).into(),
            Term::Integer(2.into()).into(),
        ]);
        assert!(vm.call(&fun, &[2.into()]).unwrap().erl_eq(&out));
    }

    {
        let fun = FunctionIdent {
            module: Ident::from_str("woo"),
            name: Ident::from_str("create_4"),
            arity: 1,
        };

        let out = Term::Tuple(vec![
            Term::Atom(Symbol::intern("alien")).into(),
            Term::Atom(Symbol::intern("zsf")).into(),
            Term::Atom(Symbol::intern("zorgon")).into(),
        ]);
        assert!(vm
            .call(&fun, &[Term::Atom(Symbol::intern("zsf")).into()])
            .unwrap()
            .erl_eq(&out));
    }

    {
        let fun = FunctionIdent {
            module: Ident::from_str("woo"),
            name: Ident::from_str("create_5"),
            arity: 2,
        };

        let out = Term::Tuple(vec![
            Term::Atom(Symbol::intern("alien")).into(),
            Term::Atom(Symbol::intern("zsf")).into(),
            Term::Atom(Symbol::intern("xon")).into(),
        ]);
        assert!(vm
            .call(
                &fun,
                &[
                    Term::Atom(Symbol::intern("zsf")).into(),
                    Term::Atom(Symbol::intern("xon")).into(),
                ]
            )
            .unwrap()
            .erl_eq(&out));
    }
}

#[test]
fn record_creation_a() {
    let mut eir_mod = lower(
        "
-module(woohoo).

-record(person, {name, phone, address}).
create_1(A) -> #person{name=A}.
",
        ParseConfig::default(),
    )
    .unwrap();

    let mut pass_manager = PassManager::default();
    pass_manager.run(&mut eir_mod);

    let fun = FunctionIdent {
        module: Ident::from_str("woo"),
        name: Ident::from_str("create_1"),
        arity: 1,
    };

    let mut vm = VMState::new();
    vm.add_builtin_modules();
    vm.add_erlang_module(eir_mod);
}
