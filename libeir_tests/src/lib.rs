//#![deny(warnings)]
#![cfg(test)]

use std::path::Path;

use libeir_ir::{ Module, FunctionIdent };
use libeir_syntax_erl::{ Parse, Parser, ParseConfig, ParserError, ErlangError };
use libeir_syntax_erl::ast::{ Module as ErlAstModule };
use libeir_syntax_erl::lower_module;
use libeir_diagnostics::{ Emitter, StandardStreamEmitter, ColorChoice };
use libeir_util_parse::{Errors, ArcCodemap, error_tee};

mod patterns;
mod list_comprehensions;
mod control_flow;
mod records;
mod errors;
mod otp;
mod ct_runner;

fn parse<T>(input: &str, config: ParseConfig) -> T
where
    T: Parse<T, Config = ParseConfig, Error = ParserError>,
{
    let codemap = ArcCodemap::default();
    let mut errors = Errors::new();

    let parser = Parser::new(config);
    let res = parser.parse_string::<&str, T>(&mut errors, &codemap, input);

    errors.print(&codemap);

    res.unwrap()
}

fn parse_file<T, P>(path: P, config: ParseConfig) -> T
where
    T: Parse<T, Config = ParseConfig, Error = ParserError>,
    P: AsRef<Path>,
{
    let codemap = ArcCodemap::default();
    let mut errors = Errors::new();

    let parser = Parser::new(config);
    let res = parser.parse_file::<_, T>(&mut errors, &codemap, path);

    errors.print(&codemap);

    res.unwrap()
}

fn lower_file<P>(path: P, config: ParseConfig) -> Result<Module, ()>
where
    P: AsRef<Path>
{
    let codemap = ArcCodemap::default();
    let mut errors: Errors<ErlangError, ErlangError> = Errors::new();

    let eir_res = error_tee(&mut errors, |mut errors| {
        let parser = Parser::new(config);
        let ast = parser.parse_file(&mut errors.make_into_adapter(), &codemap, path)?;
        let eir = lower_module(&mut errors.make_into_adapter(), &codemap, &ast)?;
        Ok(eir)
    });

    errors.print(&codemap);

    eir_res
}

pub fn lower(input: &str, config: ParseConfig) -> Result<Module, ()> {
    let codemap = ArcCodemap::default();
    let mut errors: Errors<ErlangError, ErlangError> = Errors::new();

    let eir_res = error_tee(&mut errors, |mut errors| {
        let parser = Parser::new(config);
        let ast = parser.parse_string(&mut errors.make_into_adapter(), &codemap, input)?;
        let eir = lower_module(&mut errors.make_into_adapter(), &codemap, &ast)?;
        Ok(eir)
    });

    errors.print(&codemap);

    eir_res
}

pub fn write_dot(module: &Module, ident: Option<FunctionIdent>) {
    if let Some(ident) = ident {
        let idx = module.ident_index(&ident).unwrap();
        let fun_def = &module[idx];
        let fun = fun_def.function();

        let dot = libeir_ir::text::dot_printer::function_to_dot(fun);
        print!("{}", dot);
    } else {
        unimplemented!()
    }
}
