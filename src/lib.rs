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

    #[test]
    fn basic_regress() {
        use std::fs;
        use std::io::Read;
        let paths = fs::read_dir("test_data/basic_regress").unwrap();

        for path in paths {
            let path = path.unwrap();
            if path.file_name().to_str().unwrap().starts_with(".") {
                continue
            }

            println!("File: {:?}", path.path());
            assert!(path.file_type().unwrap().is_file());

            let mut f = fs::File::open(path.path()).unwrap();
            let mut contents = String::new();
            f.read_to_string(&mut contents).unwrap();

            let res = ::parser::annotated_module(&contents).unwrap();
            let hir = ::ir::from_parsed(&res.0);

        }
    }

}
