#![deny(warnings)]
#![feature(trait_alias)]

mod abstr;
mod lexer;
mod parser;
mod preprocessor;
mod lower;

pub use self::lexer::*;
pub use self::parser::*;
pub use self::preprocessor::*;
pub use self::lower::lower_module;
// Used for diagnostics
pub use self::lower::{LowerError, BinaryTypeName};
pub use self::abstr::lower as lower_abstr;
