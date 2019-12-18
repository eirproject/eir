mod source;
pub use source::*;

mod scanner;
pub use scanner::*;

mod util;
pub use util::*;

mod result;
pub use result::*;

mod errors;
pub use errors::*;

mod parser;
pub use parser::*;

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
