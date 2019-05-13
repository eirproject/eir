#![feature(box_patterns)]
#![feature(box_syntax)]
#![feature(core_intrinsics)]
#![feature(custom_attribute)]
#![feature(map_get_key_value)]
#![feature(optin_builtin_traits)]
#![feature(dropck_eyepatch)]
#![feature(raw_vec_internals)]
#![feature(test)]

extern crate alloc;

#[cfg(any(test, bench))]
extern crate test;

mod arena;
mod lexer;
mod parser;
mod preprocessor;
mod util;

pub use self::lexer::*;
pub use self::parser::*;
pub use self::preprocessor::*;
