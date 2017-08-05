extern crate core_erlang;

use std::io::Read;

fn main() {
    let mut text = String::new();
    std::fs::File::open("gen_server.core").unwrap()
        .read_to_string(&mut text).unwrap();

    let res = core_erlang::parser::annotated_module(&text).unwrap();
    let hir = core_erlang::ir::from_parsed(&res.0);

    for fun in hir.functions.iter() {
        println!("{}", fun.ident);
    }

    let mut out = ::std::fs::File::create("cfg.dot").unwrap();
    core_erlang::ir::lir::to_dot::function_to_dot(
        &hir.functions[6], &mut out).unwrap();

}
