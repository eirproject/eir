extern crate string_intern;

mod intern;
pub use self::intern::{ Atom, Variable };

pub mod parser;
pub mod ir;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
    }
}
