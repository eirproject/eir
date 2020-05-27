use std::cmp::Ord;
use std::fmt::Debug;
use std::hash::Hash;
use std::marker::PhantomData;
use std::ops::{BitAnd, BitOr, Not};

use num_traits::{CheckedShl, CheckedShr};

mod impls;

mod slice;
pub use self::slice::BitSlice;

mod bitvec;
pub use self::bitvec::BitVec;

mod integer;
pub use self::integer::{carrier_to_integer, integer_to_carrier, Endian};

/// A primitive data type that can be used to store bits.
/// Must be sized, copyable, and implement a set of
/// elemental operations.
pub trait BitTransport:
    Sized
    + Ord
    + Copy
    + Not<Output = Self>
    + BitAnd<Output = Self>
    + BitOr<Output = Self>
    + CheckedShr
    + CheckedShl
    + Eq
    + Hash
    + Debug
{
    const SIZE: usize;
    const BIT_SIZE: usize = Self::SIZE * 8;

    const ZERO: Self;
    const ONE: Self;
}

impl BitTransport for u8 {
    const SIZE: usize = 1;
    const ZERO: u8 = 0;
    const ONE: u8 = 0;
}

/// The base trait for bit carriers.
/// Has a certain bit length.
pub trait BitCarrier {
    type T: BitTransport;

    /// The total length in bits.
    fn bit_len(&self) -> usize;

    /// Total word length including partial word at the end.
    fn word_len(&self) -> usize {
        (self.bit_len() + (Self::T::BIT_SIZE - 1)) / Self::T::BIT_SIZE
    }
    /// Number of bits in the padding word at the end.
    fn partial_bit_len(&self) -> usize {
        let rem = self.bit_len() % Self::T::BIT_SIZE;
        if rem == 0 {
            Self::T::BIT_SIZE
        } else {
            rem
        }
    }
}

pub trait BitRead: BitCarrier {
    /// Reads the nth word from the data type.
    /// The last word may be padded with arbitrary bits at the
    /// least significant digit side.
    fn read_word(&self, n: usize) -> Self::T;

    fn read_bit(&self, bit_n: usize) -> bool {
        let word_n = bit_n / Self::T::BIT_SIZE;
        let sub_bit_n = bit_n % Self::T::BIT_SIZE;

        let word = self.read_word(word_n);

        let offset = (Self::T::BIT_SIZE - 1 - sub_bit_n) as u32;
        (word & (Self::T::ONE << offset)) != Self::T::ZERO
    }

    fn read<P>(&self, to: &mut P)
    where
        Self: Sized,
        P: BitWrite<T = Self::T>,
    {
        to.write(self)
    }

    fn iter_words(&self) -> CarrierWordIter<Self, Self::T>
    where
        Self: Sized,
    {
        CarrierWordIter {
            inner: self,
            _transport: PhantomData,
            idx: 0,
            rem: self.bit_len() + 8,
        }
    }
}

pub trait BitWrite: BitCarrier {
    /// Sets the masked bits of element n to data.
    /// Writes outside of the container with a non zero mask
    /// not allowed.
    fn write_word(&mut self, n: usize, data: Self::T, mask: Self::T);

    /// Writes one bit carrier into another bit carrier of the
    /// same length.
    fn write<P>(&mut self, from: P)
    where
        P: BitRead<T = Self::T>,
    {
        assert!(self.bit_len() == from.bit_len());

        let mut len = self.bit_len() as isize;
        for n in 0..self.word_len() {
            let num_bits = std::cmp::min(len as usize, Self::T::BIT_SIZE) as u32;
            let mask = !((!Self::T::ZERO)
                .checked_shr(num_bits)
                .unwrap_or(Self::T::ZERO));

            let read = from.read_word(n);
            self.write_word(n, read, mask);
            len -= Self::T::BIT_SIZE as isize;
        }
    }
}

pub fn copy<S, D, T>(from: S, mut to: D)
where
    S: BitRead<T = T>,
    D: BitWrite<T = T>,
    T: BitTransport,
{
    to.write(from)
}

pub struct CarrierWordIter<'a, I, T>
where
    I: BitRead<T = T>,
    T: BitTransport,
{
    inner: &'a I,
    _transport: PhantomData<T>,
    idx: usize,
    rem: usize,
}
impl<'a, I, T> Iterator for CarrierWordIter<'a, I, T>
where
    I: BitRead<T = T>,
    T: BitTransport,
{
    type Item = T;
    fn next(&mut self) -> Option<T> {
        if self.rem < 8 {
            None
        } else {
            let res = self.inner.read_word(self.idx);
            self.rem -= 8;
            self.idx += 1;
            Some(res)
        }
    }
}
