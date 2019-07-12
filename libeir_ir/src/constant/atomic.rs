use std::fmt::{ Display, Formatter, Result as FmtResult };
use std::hash::{ Hash, Hasher };

use num::cast::{ NumCast, cast };

use libeir_intern::{ Symbol, Ident };
use libeir_diagnostics::ByteSpan;

use super::float::raw_double_bits;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BigIntTerm(pub num_bigint::BigInt);
impl From<BigIntTerm> for AtomicTerm {
    fn from(data: BigIntTerm) -> Self {
        AtomicTerm::BigInt(data)
    }
}
impl Display for BigIntTerm {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        write!(fmt, "{}", self.0)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct IntTerm(pub i64);
impl From<IntTerm> for AtomicTerm {
    fn from(data: IntTerm) -> Self {
        AtomicTerm::Int(data)
    }
}
impl Display for IntTerm {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        write!(fmt, "{}", self.0)
    }
}

fn from_num<N>(n: N) -> AtomicTerm where N: NumCast {
    if let Some(int) = cast(n) {
        AtomicTerm::Int(IntTerm(int))
    } else {
        // TODO bigint
        unimplemented!()
    }
}
macro_rules! impl_from_num {
    ($typ:ty) => {
        impl From<$typ> for AtomicTerm {
            fn from(data: $typ) -> Self {
                from_num(data)
            }
        }
    }
}
impl_from_num!(usize);
impl_from_num!(i32);
impl_from_num!(i64);
impl_from_num!(u32);
impl_from_num!(u64);

impl From<char> for AtomicTerm {
    fn from(data: char) -> Self {
        from_num(data as i64)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FloatTerm(pub f64);
impl Eq for FloatTerm {}
impl Hash for FloatTerm {
    fn hash<H>(&self, state: &mut H) where H: Hasher {
        raw_double_bits(&self.0).hash(state)
    }
}
impl From<FloatTerm> for AtomicTerm {
    fn from(data: FloatTerm) -> Self {
        AtomicTerm::Float(data)
    }
}
impl From<f64> for AtomicTerm {
    fn from(data: f64) -> Self {
        AtomicTerm::Float(FloatTerm(data))
    }
}
impl Display for FloatTerm {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        write!(fmt, "f{}", self.0)
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct AtomTerm(pub Symbol);
impl From<AtomTerm> for AtomicTerm {
    fn from(data: AtomTerm) -> Self {
        AtomicTerm::Atom(data)
    }
}
impl From<Symbol> for AtomicTerm {
    fn from(data: Symbol) -> Self {
        AtomTerm(data).into()
    }
}
impl From<bool> for AtomicTerm {
    fn from(data: bool) -> Self {
        let sym = if data {
            Symbol::intern("true")
        } else {
            Symbol::intern("false")
        };

        AtomTerm(sym).into()
    }
}
impl Display for AtomTerm {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        write!(fmt, "a'{}'", self.0) // TODO escape
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct BinaryTerm(pub Symbol);
impl From<BinaryTerm> for AtomicTerm {
    fn from(data: BinaryTerm) -> Self {
        AtomicTerm::Binary(data)
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct NilTerm;
impl From<NilTerm> for AtomicTerm {
    fn from(data: NilTerm) -> Self {
        AtomicTerm::Nil
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AtomicTerm {
    Int(IntTerm),
    BigInt(BigIntTerm),
    Float(FloatTerm),
    Atom(AtomTerm),
    Binary(BinaryTerm),
    Nil,
}

impl Display for AtomicTerm {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        match self {
            AtomicTerm::Int(int) => write!(fmt, "{}", int),
            AtomicTerm::BigInt(int) => write!(fmt, "{}", int),
            AtomicTerm::Float(float) => write!(fmt, "{}", float),
            AtomicTerm::Atom(atom) => write!(fmt, "{}", atom),
            AtomicTerm::Nil => write!(fmt, "[]"),
            _ => unimplemented!(),
        }
    }
}
