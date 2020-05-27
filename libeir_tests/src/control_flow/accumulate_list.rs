use crate::lower;

use libeir_intern::Ident;
use libeir_ir::FunctionIdent;
use libeir_passes::PassManager;
use libeir_syntax_erl::ParseConfig;

use libeir_interpreter::{Term, VMState};

use std::rc::Rc;

#[test]
fn test_list_acc() {
    let mut eir_mod = lower(
        "-module(woo).

woo([], Acc) -> Acc;
woo([H | T], Acc) -> woo(T, H + Acc).

woo(V) -> woo(V, 0).
",
        ParseConfig::default(),
    )
    .unwrap();

    let mut pass_manager = PassManager::default();
    pass_manager.run(&mut eir_mod);

    let mut vm = VMState::new();
    vm.add_builtin_modules();
    vm.add_erlang_module(eir_mod);

    let fun = FunctionIdent {
        module: Ident::from_str("woo"),
        name: Ident::from_str("woo"),
        arity: 1,
    };

    {
        let arg = Term::slice_to_list(
            &[
                Term::Integer(1.into()).into(),
                Term::Integer(2.into()).into(),
                Term::Integer(4.into()).into(),
            ],
            Term::Nil.into(),
        );
        assert!(
            vm.call(&fun, &[Rc::try_unwrap(arg).unwrap()])
                .unwrap()
                .as_i64()
                == Some(7)
        );
    }
}
