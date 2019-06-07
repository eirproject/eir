use super::Ident;
use libeir_diagnostics::ByteSpan;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct IntegerTerm(pub num_bigint::BigInt, pub ByteSpan);
impl From<IntegerTerm> for AtomicTerm {
    fn from(data: IntegerTerm) -> Self {
        AtomicTerm::Integer(data)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FloatTerm(pub ByteSpan); // TODO
impl From<FloatTerm> for AtomicTerm {
    fn from(data: FloatTerm) -> Self {
        AtomicTerm::Float(data)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct AtomTerm(pub Ident);
impl From<AtomTerm> for AtomicTerm {
    fn from(data: AtomTerm) -> Self {
        AtomicTerm::Atom(data)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CharTerm(pub char, pub ByteSpan);
impl From<CharTerm> for AtomicTerm {
    fn from(data: CharTerm) -> Self {
        AtomicTerm::Char(data)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BinaryTerm(pub Ident);
impl From<BinaryTerm> for AtomicTerm {
    fn from(data: BinaryTerm) -> Self {
        AtomicTerm::Binary(data)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct NilTerm(pub ByteSpan);
impl From<NilTerm> for AtomicTerm {
    fn from(data: NilTerm) -> Self {
        AtomicTerm::Nil(data)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AtomicTerm {
    Integer(IntegerTerm),
    Float(FloatTerm),
    Atom(AtomTerm),
    Char(CharTerm),
    Binary(BinaryTerm),
    Nil(NilTerm),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConstantTerm {
    Atomic(AtomicTerm),
    List(Vec<ConstantTerm>, Box<ConstantTerm>),
}
impl ConstantTerm {

    pub fn atom(&self) -> Option<AtomTerm> {
        if let ConstantTerm::Atomic(atomic) = self {
            if let AtomicTerm::Atom(atom) = atomic {
                Some(*atom)
            } else {
                None
            }
        } else {
            None
        }
    }

}
