use std::rc::Rc;

use crate::{ lower, write_dot };

use libeir_ir::{ FunctionIdent };
use libeir_syntax_erl::{ ParseConfig };
use libeir_intern::{ Ident, Symbol };
use libeir_passes::PassManager;

use libeir_interpreter::{ VMState, Term, ErlEq };

#[test]
fn test_nth_root() {
    let mut eir_mod = lower("
-module(woo).

fixed_point(F, Guess, Tolerance) ->
    fixed_point(F, Guess, Tolerance, F(Guess)).
fixed_point(_, Guess, Tolerance, Next) when erlang:abs(Guess - Next) < Tolerance ->
    Next;
fixed_point(F, _, Tolerance, Next) ->
    fixed_point(F, Next, Tolerance, F(Next)).

nth_root(N, X) -> nth_root(N, X, 1.0e-5).
nth_root(N, X, Precision) ->
    F = fun(Prev) -> ((N - 1) * Prev + X / math:pow(Prev, (N-1))) / N end,
    fixed_point(F, X, Precision).
", ParseConfig::default()).unwrap();

    let mut pass_manager = PassManager::default();
    pass_manager.run(&mut eir_mod);

    let fun = FunctionIdent {
        module: Ident::from_str("woo"),
        name: Ident::from_str("nth_root"),
        arity: 2,
    };

    let mut vm = VMState::new();
    vm.add_builtin_modules();
    vm.add_erlang_module(eir_mod);

    assert!(vm.call(&fun, &[2.into(), 2.into()]).unwrap().as_boolean() == Some(true));
}
