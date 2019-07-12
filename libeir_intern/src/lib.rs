#![feature(core_intrinsics)]
#![feature(dropck_eyepatch)]
#![feature(raw_vec_internals)]
#![feature(test)]

extern crate alloc;

#[cfg(any(test, bench))]
extern crate test;

pub mod arena;

pub mod symbol;
pub use symbol::{ Ident, Symbol, InternedString, LocalInternedString };

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
        assert_eq!(2 + 2, 4);
    }
}
