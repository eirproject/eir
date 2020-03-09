use crate::ast::*;
use crate::*;

use crate::parser::ParseConfig;
use crate::lower::lower_module;

use libeir_ir::{Module as IrModule, StandardFormatConfig};
use libeir_util_parse::{Errors, ArcCodemap};

fn parse<T>(input: &str, config: ParseConfig, codemap: &ArcCodemap) -> T
where
    T: Parse<T, Config = ParseConfig, Error = ParserError>,
{
    let parser = Parser::new(config);
    let mut errors = Errors::new();
    match parser.parse_string::<&str, T>(&mut errors, codemap, input) {
        Ok(ast) => return ast,
        Err(()) => (),
    };
    errors.print(codemap);
    panic!("parse failed");
}

fn lower(input: &str, config: ParseConfig) -> Result<IrModule, ()> {
    let codemap = ArcCodemap::default();
   // let mut errors = MultiErrors::new(config.codemap.clone());
    let parsed: Module = parse(input, config, &codemap);

    let mut errors = Errors::new();
    let res = lower_module(&mut errors, &codemap, &parsed);
    errors.print(&codemap);

    res
}

#[test]
fn fib_lower() {
    let _result = lower(
        "-module(fib).

fib(X) when X < 2 -> 1;
fib(X) -> fib(X - 1) + fib(X-2).
",
        ParseConfig::default()
    ).unwrap();


}

#[test]
fn pat_1_lower() {
    let fun = lower(
        "-module(pat).
pat(A, A) -> 1.
",
        ParseConfig::default()
    ).unwrap();

    println!("{}", fun.to_text(&mut StandardFormatConfig::default()));
}

//#[test]
//fn compiler_lower() {
//    let mut config = ParseConfig::default();
//
//    config.include_paths.push_front(PathBuf::from("../otp/lib/compiler/src/"));
//    config.include_paths.push_front(PathBuf::from("../otp/bootstrap/lib/stdlib/include/"));
//
//    let ir = lower_file("../otp/lib/compiler/src/compile.erl", config).unwrap();
//
//    let ident = FunctionIdent {
//        module: Ident::from_str("compile"),
//        name: Ident::from_str("iofile"),
//        arity: 1,
//    };
//
//    let mut errors = Vec::new();
//    for fun in ir.functions.values() {
//        //if fun.ident() == &ident {
//            println!("{:?}", fun.ident());
//        fun.validate(&mut errors);
//        //}
//    }
//    println!("{:#?}", errors);
//
//    let fun = &ir.functions[&ident];
//
//    print!("{}", fun.to_text());
//
//    let mut dot = Vec::<u8>::new();
//    libeir_ir::text::dot_printer::function_to_dot(fun, &mut dot).unwrap();
//    let dot_text = std::str::from_utf8(&dot).unwrap();
//    print!("{}", dot_text);
//
//}
