use num_bigint::BigInt;

trait TermTypes {
    type Atom;
}

#[derive(Debug, PartialEq)]
pub enum Term {
    Integer(i32),
    Float(f32),
    Tuple(Vec<Term>),
    Map(Vec<(Term, Term)>),
    Nil,
    List(Vec<Term>, Box<Term>),
    ByteList(Vec<u8>),
    Binary(Vec<u8>),
    BitBinary(Vec<u8>, u8),
    BigInt(BigInt),
    Atom(String),
}
