use crate::lower;

use libeir_ir::{ FunctionIdent };
use libeir_syntax_erl::{ ParseConfig };
use libeir_intern::Ident;
use libeir_passes::PassManager;

use libeir_interpreter::{ VMState, Term };

#[test]
fn test_fib() {

    let mut eir_mod = lower(
        "-module(fib).

fib(X) when X < 2 -> 1;
fib(X) -> fib(X - 1) + fib(X-2).
",
        ParseConfig::default()
    ).unwrap();

    let mut pass_manager = PassManager::default();
    pass_manager.run(&mut eir_mod);

    let mut vm = VMState::new();
    vm.add_builtin_modules();
    vm.add_erlang_module(eir_mod);

    let fun = FunctionIdent {
        module: Ident::from_str("fib"),
        name: Ident::from_str("fib"),
        arity: 1,
    };

    let mut call_fib = |n: i64| {
        let num = Term::Integer(n.into());
        vm.call(&fun, &[num.into()])
    };

    fn rust_fib(x: i64) -> i64 {
        if x < 2 {
            1
        } else {
            rust_fib(x - 1) + rust_fib(x - 2)
        }
    }

    assert!(call_fib(1).unwrap().as_i64().unwrap() == rust_fib(1));
    assert!(call_fib(2).unwrap().as_i64().unwrap() == rust_fib(2));
    assert!(call_fib(3).unwrap().as_i64().unwrap() == rust_fib(3));
    assert!(call_fib(4).unwrap().as_i64().unwrap() == rust_fib(4));
    assert!(call_fib(5).unwrap().as_i64().unwrap() == rust_fib(5));
    assert!(call_fib(6).unwrap().as_i64().unwrap() == rust_fib(6));
    assert!(call_fib(7).unwrap().as_i64().unwrap() == rust_fib(7));
    assert!(call_fib(8).unwrap().as_i64().unwrap() == rust_fib(8));

}
