use std::marker::PhantomData;
use std::ops::{ Not, BitAnd, BitOr };
use std::cmp::{ Ord, PartialOrd, Ordering };
use std::hash::{ Hash, Hasher };
use std::fmt::Debug;

use num_traits::{ CheckedShl, CheckedShr };

mod impls;

mod slice;
pub use self::slice::BitSlice;

mod bitvec;
pub use self::bitvec::BitVec;

mod integer;
pub use self::integer::{ Endian, carrier_to_integer, integer_to_carrier };

/// A primitive data type that can be used to store bits.
/// Must be sized, copyable, and implement a set of
/// elemental operations.
pub trait BitTransport
    : Sized
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

    fn read<P>(&self, to: &mut P) where Self: Sized, P: BitWrite<T = Self::T> {
        to.write(self)
    }

    fn iter_words(&self) -> CarrierWordIter<Self, Self::T> where Self: Sized {
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
    fn write<P>(&mut self, from: P) where P: BitRead<T = Self::T> {
        assert!(self.bit_len() == from.bit_len());

        let mut len = self.bit_len() as isize;
        for n in 0..self.word_len() {
            let num_bits = std::cmp::min(len as usize, Self::T::BIT_SIZE) as u32;
            let mask = !((!Self::T::ZERO).checked_shr(num_bits)
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
    T: BitTransport
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


#[cfg(test)]
mod test {

}


//enum Endianness {
//    Big,
//    Little,
//}
//
//#[derive(Debug, Clone)]
//struct BitBuf {
//    buf: Vec<u8>,
//    bit_size: usize,
//}
//
//impl BitBuf {
//
//    pub fn new() -> Self {
//        BitBuf {
//            buf: vec![0],
//            bit_size: 0,
//        }
//    }
//
//    pub fn clear(&mut self) {
//        self.buf.clear();
//        self.buf.push(0);
//        self.bit_size = 0;
//    }
//
//    pub fn push_u8(&mut self, num: u8, bits: usize) {
//        assert!(bits <= 8);
//        let num = num as u16;
//
//        let bit_num = self.bit_size % 8;
//        let byte_num = self.bit_size / 8;
//
//        let num_left = num << (8 - bits);
//        self.buf[byte_num] |= (num_left >> bit_num) as u8;
//
//        if 8 - (bit_num as isize) <= (bits as isize) {
//            self.buf.push((num_left << (8 - bit_num)) as u8);
//        }
//
//        self.bit_size += bits;
//    }
//
//    pub fn push<T>(&mut self, prim: T) where T: BitPrimitive {
//        let num_bits = prim.num_bits();
//
//        let mut rem_bits = num_bits as isize;
//        let mut idx = 0;
//
//        while rem_bits > 0 {
//            self.push_u8(prim.index(idx), std::cmp::min(rem_bits, 8) as usize);
//            idx += 1;
//            rem_bits -= 8;
//        }
//    }
//
//    pub fn push_integer(
//        &mut self,
//        mut int: Integer,
//        endianness: Endianness,
//        bits: usize,
//    ) {
//        let neg = int < 0;
//        if neg {
//            int -= 1;
//        }
//        int.keep_bits_mut(bits as u32);
//
//        let data = int.to_digits::<u8>(match endianness {
//            Endianness::Big => Order::Msf,
//            Endianness::Little => unimplemented!(),
//        });
//        println!("{:?}", data);
//
//        let bytes_n = (bits + 7) / 8;
//        let rem_bits = bits % 8;
//
//        if data.len() < bytes_n {
//            for _ in 0..(bytes_n - data.len()) {
//                self.push_u8(0, 8);
//            }
//        }
//
//        for n in 0..data.len() {
//            if n == (data.len()-1) && rem_bits > 0 {
//                self.push_u8(data[n], rem_bits);
//            } else {
//                self.push_u8(data[n], 8);
//            }
//        }
//
//    }
//
//    pub fn get_byte(&self, idx: usize) -> Option<u8> {
//        self.buf.get(idx).cloned()
//    }
//
//    pub fn bit_len(&self) -> usize {
//        self.bit_size
//    }
//
//}
//
//trait BitPrimitive {
//    fn num_bits(&self) -> usize;
//    /// Gets byte n of the primitive
//    fn index(&self, n: usize) -> u8;
//}
//
//impl BitPrimitive for bool {
//    fn num_bits(&self) -> usize {
//        1
//    }
//    fn index(&self, _num: usize) -> u8 {
//        *self as u8
//    }
//}
//
//macro_rules! impl_primitive {
//    ($prim:ty) => {
//        impl BitPrimitive for $prim {
//            fn num_bits(&self) -> usize {
//                std::mem::size_of::<$prim>() * 8
//            }
//            fn index(&self, num: usize) -> u8 {
//                let n = *self as u64;
//                let idx = std::mem::size_of::<$prim>() - 1 - num;
//                (n >> (idx * 8)) as u8
//            }
//        }
//    }
//}
//impl_primitive!(i8);
//impl_primitive!(u8);
//impl_primitive!(i16);
//impl_primitive!(u16);
//impl_primitive!(i32);
//impl_primitive!(u32);
//impl_primitive!(i64);
//impl_primitive!(u64);
//
//#[cfg(test)]
//mod test {
//    use rug::Integer;
//    use super::{ BitBuf, Endianness };
//
//    #[test]
//    fn base_push() {
//        let mut buf = BitBuf::new();
//
//        buf.push_u8(0b0011, 4);
//        assert!(buf.bit_len() == 4);
//        assert!(buf.get_byte(0) == Some(0b00110000));
//        assert!(buf.get_byte(1) == None);
//
//        buf.push_u8(0b1, 1);
//        assert!(buf.bit_len() == 5);
//        assert!(buf.get_byte(0) == Some(0b00111000));
//        assert!(buf.get_byte(1) == None);
//
//        buf.push_u8(0b1111, 4);
//        assert!(buf.bit_len() == 9);
//        assert!(buf.get_byte(0) == Some(0b00111111));
//        assert!(buf.get_byte(1) == Some(0b10000000));
//        assert!(buf.get_byte(2) == None);
//
//        buf.push_u8(0b1111000, 7);
//        assert!(buf.get_byte(0) == Some(0b00111111));
//        assert!(buf.get_byte(1) == Some(0b11111000));
//        assert!(buf.get_byte(2) == Some(0b00000000));
//        assert!(buf.get_byte(3) == None);
//
//        buf.push_u8(0b1111000, 8);
//    }
//
//    #[test]
//    fn prim_push() {
//        let mut buf = BitBuf::new();
//
//        buf.push(0b10101010u8);
//        assert!(buf.bit_len() == 8);
//        buf.push(true);
//        assert!(buf.bit_len() == 9);
//        buf.push(213861u64);
//        assert!(buf.bit_len() == 9 + 64);
//
//        let mut buf = BitBuf::new();
//        buf.push_u8(0, 4);
//        buf.push(0b1100110011001100u16);
//        assert!(buf.get_byte(0) == Some(0b00001100));
//        assert!(buf.get_byte(1) == Some(0b11001100));
//        assert!(buf.get_byte(2) == Some(0b11000000));
//        assert!(buf.get_byte(3) == None);
//
//        let mut buf = BitBuf::new();
//        buf.push(0b1100000000000000u16);
//        assert!(buf.get_byte(0) == Some(0b11000000));
//        assert!(buf.get_byte(1) == Some(0b00000000));
//        assert!(buf.get_byte(2) == Some(0b00000000));
//        assert!(buf.get_byte(3) == None);
//    }
//
//    #[test]
//    fn integer_push() {
//        let mut buf = BitBuf::new();
//
//        let int = Integer::from(1);
//        buf.push_integer(int, Endianness::Big, 12);
//        assert!(buf.get_byte(0) == Some(0b00000000));
//        assert!(buf.get_byte(1) == Some(0b00010000));
//        assert!(buf.get_byte(2) == None);
//
//        buf.clear();
//        let int = Integer::from(0b1100);
//        buf.push_integer(int, Endianness::Big, 12);
//        assert!(buf.get_byte(0) == Some(0b00000000));
//        assert!(buf.get_byte(1) == Some(0b11000000));
//        assert!(buf.get_byte(2) == None);
//
//        buf.clear();
//        let int = Integer::from(0b111100);
//        buf.push_integer(int, Endianness::Big, 12);
//        println!("{:?}", buf);
//        assert!(buf.get_byte(1) == Some(0b11000000));
//        assert!(buf.get_byte(0) == Some(0b00000011));
//        assert!(buf.get_byte(2) == None);
//
//        panic!()
//    }
//
//}
