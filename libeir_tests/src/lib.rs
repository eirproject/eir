//#![deny(warnings)]
#![cfg(test)]

use std::path::Path;

use libeir_ir::{ Module, FunctionIdent };
use libeir_syntax_erl::{ Parse, Parser, ParseConfig };
use libeir_syntax_erl::ast::{ Module as ErlAstModule };
use libeir_syntax_erl::lower_module;
use libeir_diagnostics::{ Emitter, StandardStreamEmitter, ColorChoice };

mod patterns;
mod list_comprehensions;
mod control_flow;
mod records;
mod errors;
mod otp;
mod ct_runner;

fn parse<T>(input: &str, config: ParseConfig) -> (T, Parser)
where
    T: Parse<T>,
{
    let parser = Parser::new(config);
    let errs = match parser.parse_string::<&str, T>(input) {
        Ok(ast) => return (ast, parser),
        Err(errs) => errs,
    };
    let emitter = StandardStreamEmitter::new(ColorChoice::Auto)
        .set_codemap(parser.config.codemap.clone());
    for err in errs.iter() {
        emitter.diagnostic(&err.to_diagnostic()).unwrap();
    }
    panic!("parse failed");
}

fn parse_file<T, P>(path: P, config: ParseConfig) -> (T, Parser)
where
    T: Parse<T>,
    P: AsRef<Path>,
{
    let parser = Parser::new(config);
    let errs = match parser.parse_file::<_, T>(path) {
        Ok(ast) => return (ast, parser),
        Err(errs) => errs,
    };
    let emitter = StandardStreamEmitter::new(ColorChoice::Auto)
        .set_codemap(parser.config.codemap.clone());
    for err in errs.iter() {
        emitter.diagnostic(&err.to_diagnostic()).unwrap();
    }
    panic!("parse failed");
}

fn lower_file<P>(path: P, config: ParseConfig) -> Result<Module, ()>
where
    P: AsRef<Path>
{
    let (parsed, parser): (ErlAstModule, _) = parse_file(path, config);
    let (res, messages) = lower_module(&parsed);

    let emitter = StandardStreamEmitter::new(ColorChoice::Auto)
        .set_codemap(parser.config.codemap.clone());
    for err in messages.iter() {
        emitter.diagnostic(&err.to_diagnostic()).unwrap();
    }

    res
}

pub fn lower(input: &str, config: ParseConfig) -> Result<Module, ()> {
    let (parsed, parser): (ErlAstModule, _) = parse(input, config);
    let (res, messages) = lower_module(&parsed);

    let emitter = StandardStreamEmitter::new(ColorChoice::Auto)
        .set_codemap(parser.config.codemap.clone());
    for err in messages.iter() {
        emitter.diagnostic(&err.to_diagnostic()).unwrap();
    }

    res
}

pub fn write_dot(module: &Module, ident: Option<FunctionIdent>) {
    if let Some(ident) = ident {
        let fun = &module.functions[&ident];
        let mut dot = Vec::<u8>::new();
        libeir_ir::text::dot_printer::function_to_dot(fun, &mut dot).unwrap();
        let dot_text = std::str::from_utf8(&dot).unwrap();
        print!("{}", dot_text);
    } else {
        unimplemented!()
    }
}

