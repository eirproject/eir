use crate::ast::*;
use crate::*;

use crate::lower::lower_module;

use libeir_ir::FunctionIdent;
use libeir_diagnostics::{ColorChoice, Emitter, StandardStreamEmitter};

use std::path::{ Path, PathBuf };

fn parse<T>(input: &str) -> T
where
    T: Parse<T>,
{
    let mut config = ParseConfig::default();
    let parser = Parser::new(config);
    let errs = match parser.parse_string::<&str, T>(input) {
        Ok(ast) => return ast,
        Err(errs) => errs,
    };
    let emitter = StandardStreamEmitter::new(ColorChoice::Auto)
        .set_codemap(parser.config.codemap.clone());
    for err in errs.iter() {
        emitter.diagnostic(&err.to_diagnostic()).unwrap();
    }
    panic!("parse failed");
}

fn parse_file<T, P>(path: P) -> T
where
    T: Parse<T>,
    P: AsRef<Path>,
{
    let mut config = ParseConfig::default();

    config.include_paths.push_front(PathBuf::from("../otp/lib/compiler/src/"));
    config.include_paths.push_front(PathBuf::from("../otp/bootstrap/lib/stdlib/include/"));

    let parser = Parser::new(config);
    let errs = match parser.parse_file::<_, T>(path) {
        Ok(ast) => return ast,
        Err(errs) => errs,
    };
    let emitter = StandardStreamEmitter::new(ColorChoice::Auto)
        .set_codemap(parser.config.codemap.clone());
    for err in errs.iter() {
        emitter.diagnostic(&err.to_diagnostic()).unwrap();
    }
    panic!("parse failed");
}

#[test]
fn fib_lower() {
    let result: Module = parse(
        "-module(fib).

fib(X) when X < 2 -> 1;
fib(X) -> fib(X - 1) + fib(X-2).
"
    );

    let ir = lower_module(&result);

    let ident = FunctionIdent {
        module: Ident::from_str("fib"),
        name: Ident::from_str("fib"),
        arity: 1,
    };
    let fun = &ir.functions[&ident];

    print!("{}", fun.to_text());

    let mut dot = Vec::<u8>::new();
    libeir_ir::text::dot_printer::function_to_dot(fun, &mut dot).unwrap();
    let dot_text = std::str::from_utf8(&dot).unwrap();
    print!("{}", dot_text);
}

#[test]
fn compiler_lower() {
    let result: Module = parse_file("../otp/lib/compiler/src/compile.erl");
    let ir = lower_module(&result);
}
