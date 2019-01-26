extern crate tempdir;

use ::std::io::{ Read, Write };
use ::{ ExecutionContext, Term };
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

    let parsed = ::core_erlang_compiler::parser::parse(&core).unwrap();
    let ir = ::core_erlang_compiler::ir::from_parsed(&parsed.0);

    println!("Ir:\n{:?}", ir);

    ir
}

fn ctx_from_erl(erlang_code: &str) -> ExecutionContext {
    let module = erl_to_ir(erlang_code);

    let mut ctx = ExecutionContext::new();

    ctx.add_native_module(::erl_lib::make_erlang());
    ctx.add_erlang_module(module);

    ctx
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
    O = 1,
    fun(A) ->
            add(add(I, A), O)
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
    let ctx = ctx_from_erl(TEST_ERL_1);

    let args = vec![Term::new_i64(1), Term::new_i64(2)];
    let result = ctx.call("test", "add", &args);

    assert!(result.unwrap_return().erl_eq(&Term::Integer(3.into())));
}

#[test]
fn simple_function_call() {
    let ctx = ctx_from_erl(TEST_ERL_1);

    let args = vec![Term::new_i64(1), Term::new_i64(2), Term::new_i64(3)];
    let result = ctx.call("test", "add_two", &args);

    assert!(result.unwrap_return().erl_eq(&Term::Integer(6.into())));
}

#[test]
fn simple_lambda() {
    let ctx = ctx_from_erl(TEST_ERL_1);

    let args = vec![Term::new_i64(1), Term::new_i64(2)];
    let result = ctx.call("test", "add_with_closure", &args);

    assert!(result.unwrap_return().erl_eq(&Term::Integer(4.into())));
}

#[test]
fn simple_pattern_match() {
    let ctx = ctx_from_erl(TEST_ERL_1);

    let args = vec![Term::new_i64(1), Term::new_i64(2)];
    let result = ctx.call("test", "matching", &args);
    assert!(result.unwrap_return().erl_eq(&Term::Tuple(vec![
        Term::new_i64(1),
        Term::new_i64(2),
    ])));

    let args = vec![Term::Nil, Term::Nil];
    let result = ctx.call("test", "matching", &args);
    assert!(result.unwrap_return().erl_eq(&Term::new_atom("one")));
}

const FACTORIAL_ERL: &str = r##"
-module(test).
-export([factorial/1]).

factorial(0) -> 1;
factorial(N) -> N * factorial(N-1).

    "##;

#[test]
fn factorial() {
    let ctx = ctx_from_erl(FACTORIAL_ERL);

    let args = vec![Term::new_i64(10)];
    let result = ctx.call("test", "factorial", &args);

    println!("Res: {:?}", result);
}

//#[test]
fn long_strings() {
    let mut ctx = ExecutionContext::new();

    let mut f = ::std::fs::File::open("../test_data/long_strings.core")
        .unwrap();
    let mut core = String::new();
    f.read_to_string(&mut core).unwrap();

    println!("Parsing");
    let parsed = ::core_erlang_compiler::parser::parse(&core).unwrap();

}

#[test]
fn compiler() {
    println!("A");

    let mut ctx = ExecutionContext::new();

    ctx.add_native_module(::erl_lib::make_erlang());
    ctx.add_native_module(::erl_lib::make_os());

    println!("B");

    let mut f = ::std::fs::File::open("../test_data/compile.core")
        .unwrap();
    println!("C");
    let mut core = String::new();
    f.read_to_string(&mut core).unwrap();
    println!("D");
    let parsed = ::core_erlang_compiler::parser::parse(&core).unwrap();
    println!("E");
    let ir = ::core_erlang_compiler::ir::from_parsed(&parsed.0);
    println!("F");
    ctx.add_erlang_module(ir);

    let mut f = ::std::fs::File::open("/home/hansihe/proj/checkout/otp/lib/stdlib/ebin/proplists.core")
        .unwrap();
    let mut core = String::new();
    f.read_to_string(&mut core).unwrap();
    let parsed = ::core_erlang_compiler::parser::parse(&core).unwrap();
    let ir = ::core_erlang_compiler::ir::from_parsed(&parsed.0);
    ctx.add_erlang_module(ir);


    let args = vec![Term::new_atom("foo.erl")];
    ctx.call("compile", "file", &args);
}
