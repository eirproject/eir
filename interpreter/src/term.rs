use core_erlang_compiler::intern::Atom;

use ::num_bigint::BigInt;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum TermType {
    Nil,
    Integer,
    Float,
    Atom,
    CapturedFunction,
}

#[derive(Debug, Clone)]
pub enum Term {
    Nil,
    Integer(BigInt),
    Float(f64),
    Atom(Atom),
    CapturedFunction { module: Atom, fun_name: Atom, arity: u32 },
}
impl Term {

    pub fn new_i64(num: i64) -> Self {
        Term::Integer(num.into())
    }

    pub fn atom_str<'a>(&'a self) -> &'a str {
        if let Term::Atom(ref atom) = *self {
            *&atom
        } else {
            panic!();
        }
    }

    pub fn get_type(&self) -> TermType {
        match self {
            Term::Nil => TermType::Nil,
            Term::Integer(_) => TermType::Integer,
            Term::Float(_) => TermType::Float,
            Term::Atom(_) => TermType::Atom,
            Term::CapturedFunction { .. } => TermType::CapturedFunction,
        }
    }

}
