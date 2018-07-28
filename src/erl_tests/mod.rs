extern crate tempdir;

use ::std::io::{ Read, Write };
use ::ir::Module;

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

    let parsed = ::parser::annotated_module(&core).unwrap();
    let ir = ::ir::from_parsed(&parsed.0);

    println!("Ir:\n{:?}", ir);

    ir
}

#[test]
fn simple_add() {
    let module = erl_to_ir(r##"
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

matching([], []) ->
    one;
matching([], _) ->
    two;
matching(_, []) ->
    three;
matching(A, B) ->
    {A, B}.

    "##);

    use ::interpreter::{ ExecutionContext, Term };
    let mut ctx = ExecutionContext::new();

    ctx.add_native_module(::interpreter::lib::make_erlang());
    ctx.add_erlang_module(module);

    let args = vec![Term::Nil, Term::Nil];
    let result = ctx.call("test", "add", &args);
    println!("{:?}", result);
}
