extern crate string_intern;
extern crate petgraph;
extern crate either;
extern crate prettytable;
extern crate pretty;

extern crate pattern_compiler;

mod intern;
pub use self::intern::{ Atom, Variable };

pub mod parser;
pub mod ir;
pub mod interpreter;
pub mod util;

#[cfg(test)]
mod erl_tests;

use pretty::{ BoxDoc, Doc };
use std::ops::Deref;
pub trait ToDoc {
    fn to_doc<'a>(&'a self) -> Doc<'a, BoxDoc>;
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
    }
}
