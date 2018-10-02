extern crate core_erlang_compiler;

use core_erlang_compiler::parser::Atom;

use std::io::Read;
use std::str::FromStr;

fn main() {
    let mut text = String::new();
    std::fs::File::open("language_test.core").unwrap()
        .read_to_string(&mut text).unwrap();

    let res = core_erlang_compiler::parser::parse(&text).unwrap();
    let hir = core_erlang_compiler::ir::from_parsed(&res.0);

    for fun in hir.functions.iter() {
        println!("{}", fun.ident);
    }

    // do_config_change/3

    let name_sym = Atom::from("try_catch");
    let fun = hir.functions.iter().find(|f| {
        f.ident.name == name_sym && f.ident.arity == 1 && f.ident.lambda == None
    }).unwrap();

    let mut out = ::std::fs::File::create("cfg.dot").unwrap();
    core_erlang_compiler::ir::lir::to_dot::function_to_dot(
        fun, &mut out).unwrap();

}
