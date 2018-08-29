extern crate tempdir;

use ::std::io::{ Read, Write };
use ::core_erlang_compiler::ir::Module;
use ::term::ErlEq;

fn erl_to_core(erlang_code: &str) -> String {
    let temp = tempdir::TempDir::new("core_erlang_crate_tests").unwrap();
    let temp_dir = temp.path();
    println!("Compilation dir: {:?}", temp_dir);

    {
        let in_file_name = temp_dir.join("code.erl");
        let mut in_file = ::std::fs::File::create(in_file_name).unwrap();
        in_file.write_all(erlang_code.as_bytes()).unwrap();
    }

    let out = ::std::process::Command::new("erlc")
        .arg("+to_core")
        .arg("code.erl")
        .current_dir(temp_dir.clone())
        .output()
        .expect("failed to execute erlc");
    println!("{:?}", out);
    assert!(out.status.success());

    let out_file_name = temp_dir.join("code.core");
    let mut out_file = ::std::fs::File::open(out_file_name).unwrap();
    let mut out_core = String::new();
    out_file.read_to_string(&mut out_core).unwrap();

    println!("====== Core ======\n{}\n==================\n\n", out_core);

    out_core
}

fn erl_to_ir(erlang_code: &str) -> Module {
    let core = erl_to_core(erlang_code);

    let parsed = ::core_erlang_compiler::parser::annotated_module(&core).unwrap();
    let ir = ::core_erlang_compiler::ir::from_parsed(&parsed.0);

    println!("Ir:\n{:?}", ir);

    ir
}

const TEST_ERL_1: &str = r##"
-module(test).
-export([add/2, add_two/3, return_closure/1]).

add(A, B) ->
    A + B.

add_two(A, B, C) ->
    I = add(A, B),
    add(I, C).

return_closure(I) ->
    fun(A) ->
            add(I, A)
    end.

add_with_closure(A, B) ->
    F = return_closure(A),
    F(B).

matching([], []) ->
    one;
matching([], _) ->
    two;
matching(_, []) ->
    three;
matching(A, B) ->
    {A, B}.

    "##;

#[test]
fn simple_add() {
    let module = erl_to_ir(TEST_ERL_1);

    use ::{ ExecutionContext, Term };
    let mut ctx = ExecutionContext::new();

    ctx.add_native_module(::erl_lib::make_erlang());
    ctx.add_erlang_module(module);

    let args = vec![Term::new_i64(1), Term::new_i64(2)];
    let result = ctx.call("test", "add", &args);

    assert!(result.unwrap_return().erl_eq(&Term::Integer(3.into())));
}

#[test]
fn simple_function_call() {
    let module = erl_to_ir(TEST_ERL_1);

    use ::{ ExecutionContext, Term };
    let mut ctx = ExecutionContext::new();

    ctx.add_native_module(::erl_lib::make_erlang());
    ctx.add_erlang_module(module);

    let args = vec![Term::new_i64(1), Term::new_i64(2), Term::new_i64(3)];
    let result = ctx.call("test", "add_two", &args);

    assert!(result.unwrap_return().erl_eq(&Term::Integer(6.into())));
}

#[test]
fn simple_lambda() {
    let module = erl_to_ir(TEST_ERL_1);

    use ::{ ExecutionContext, Term };
    let mut ctx = ExecutionContext::new();

    ctx.add_native_module(::erl_lib::make_erlang());
    ctx.add_erlang_module(module);

    let args = vec![Term::new_i64(1), Term::new_i64(2)];
    let result = ctx.call("test", "add_with_closure", &args);
}
