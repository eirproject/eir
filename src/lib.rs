extern crate string_intern;
extern crate petgraph;
extern crate either;
extern crate prettytable;

extern crate pattern_compiler;

mod intern;
pub use self::intern::{ Atom, Variable };

pub mod parser;
pub mod ir;
pub mod interpreter;
pub mod util;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
    }
}
