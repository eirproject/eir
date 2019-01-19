extern crate core_erlang_compiler;

use core_erlang_compiler::parser::Atom;

use std::io::Read;
use std::str::FromStr;

fn main() {
    let mut args = std::env::args();
    args.next().unwrap();
    let infile = args.next().unwrap();

    let fun_name = args.next();
    let arity = args.next();
    let lambda = args.next();

    let mut text = String::new();
    std::fs::File::open(&infile).unwrap()
        .read_to_string(&mut text).unwrap();

    let res = core_erlang_compiler::parser::parse(&text).unwrap();
    let hir = core_erlang_compiler::ir::from_parsed(&res.0);

    for fun in hir.functions.iter() {
        println!("{}", fun.ident);
    }

    if let Some(fun_name) = fun_name {

        let name_sym = Atom::from_str(&fun_name);
        let arity = arity.unwrap().parse().unwrap();
        let lambda: Option<usize> = lambda.map(|s| s.parse().unwrap());

        let funs: Vec<_> = hir.functions.iter().map(|f| f.ident.clone()).collect();
        println!("{:?}", funs);
        let fun = hir.functions.iter().find(|f| {
            f.ident.name == name_sym
                && f.ident.arity == arity
                && f.ident.lambda.map(|v| v.0) == lambda
        }).unwrap();

        println!("Writing to {}.dot", infile);
        let mut out = ::std::fs::File::create(infile + ".dot").unwrap();
        core_erlang_compiler::ir::lir::to_dot::function_to_dot(
            fun, &mut out).unwrap();

    } else {
        println!("No function name provided");
    }

}