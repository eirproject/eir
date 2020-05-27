use super::lower;

use libeir_intern::{Ident, Symbol};
use libeir_ir::FunctionIdent;
use libeir_passes::PassManager;
use libeir_syntax_erl::ParseConfig;

use libeir_interpreter::{Term, VMState};

#[test]
fn test_basic_catch() {
    let _ = env_logger::try_init();

    let mut eir_mod = lower(
        "
-module(woo).

foo(foo) -> false.

woo(A) -> try foo(A) catch
    error:function_clause ->
        true
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

    assert!(vm.call(&fun, &[1.into()]).unwrap().as_boolean() == Some(true));
    assert!(
        vm.call(&fun, &[Term::Atom(Symbol::intern("foo")).into()])
            .unwrap()
            .as_boolean()
            == Some(false)
    );
}

#[test]
fn test_basic_catch_miss() {
    let _ = env_logger::try_init();

    let mut eir_mod = lower(
        "
-module(woo).

foo(foo) -> false.

woo(A) -> try foo(A) catch
    error:function_clause_not ->
        true
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
        vm.call(&fun, &[Term::Atom(Symbol::intern("foo")).into()])
            .unwrap()
            .as_boolean()
            == Some(false)
    );
    assert!(vm.call(&fun, &[1.into()]).is_err());
}
