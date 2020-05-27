//#![deny(warnings)]
#![cfg(test)]

use std::path::Path;
use std::sync::Arc;

use libeir_diagnostics::*;
use libeir_ir::{FunctionIdent, Module};
use libeir_syntax_erl::lower_module;
use libeir_syntax_erl::{ErlangError, Parse, ParseConfig, Parser, ParserError};
use libeir_util_parse::{error_tee, Errors};

mod control_flow;
mod ct_runner;
mod errors;
mod list_comprehensions;
mod otp;
mod patterns;
mod records;

fn lower_file<S>(path: S, config: ParseConfig) -> Result<Module, ()>
where
    S: AsRef<Path>,
{
    let mut errors: Errors<ErlangError, ErlangError> = Errors::new();
    let codemap = Arc::new(CodeMap::new());
    let eir_res = error_tee(&mut errors, |mut errors| {
        let parser = Parser::new(config, codemap.clone());
        let ast = parser.parse_file(&mut errors.make_into_adapter(), path)?;
        let eir = lower_module(&mut errors.make_into_adapter(), codemap.clone(), &ast)?;
        Ok(eir)
    });

    errors.print(&codemap);

    eir_res
}

pub fn lower<S>(input: S, config: ParseConfig) -> Result<Module, ()>
where
    S: AsRef<str>,
{
    let mut errors: Errors<ErlangError, ErlangError> = Errors::new();
    let codemap = Arc::new(CodeMap::new());
    let eir_res = error_tee(&mut errors, |mut errors| {
        let parser = Parser::new(config, codemap.clone());
        let ast = parser.parse_string(&mut errors.make_into_adapter(), input)?;
        let eir = lower_module(&mut errors.make_into_adapter(), codemap.clone(), &ast)?;
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
