//#![deny(warnings)]
#![feature(trait_alias)]

mod abstr;
mod lexer;
mod lower;
mod parser;
mod preprocessor;

pub use self::abstr::lower as lower_abstr;
pub use self::lexer::*;
pub use self::lower::{lower_module, LowerError};
pub use self::parser::*;
pub use self::preprocessor::*;

pub enum ErlangError {
    Parser(ParserError),
    Lower(LowerError),
}
impl From<ParserError> for ErlangError {
    fn from(e: ParserError) -> Self {
        ErlangError::Parser(e)
    }
}
impl From<LowerError> for ErlangError {
    fn from(e: LowerError) -> Self {
        ErlangError::Lower(e)
    }
}
impl libeir_diagnostics::ToDiagnostic for ErlangError {
    fn to_diagnostic(&self) -> libeir_diagnostics::Diagnostic {
        match self {
            ErlangError::Parser(err) => err.to_diagnostic(),
            ErlangError::Lower(err) => err.to_diagnostic(),
        }
    }
}
