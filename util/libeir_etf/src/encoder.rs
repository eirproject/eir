use crate::{Term, Writer};
use num_bigint::BigInt;
use std::collections::{BTreeMap, HashMap};
use std::convert::TryInto;
use std::io::{Result, Write};

pub trait Encoder {
    fn encode<S: Write>(&self, writer: &mut Writer<S>) -> Result<()>;
}

impl Encoder for Term {
    fn encode<S: Write>(&self, writer: &mut Writer<S>) -> Result<()> {
        writer.term(self)
    }
}

impl Encoder for libeir_intern::Symbol {
    fn encode<S: Write>(&self, writer: &mut Writer<S>) -> Result<()> {
        writer.atom(&self.as_str())
    }
}

impl<T: Encoder> Encoder for &T {
    fn encode<S: Write>(&self, writer: &mut Writer<S>) -> Result<()> {
        <T as Encoder>::encode(*self, writer)
    }
}

#[derive(Debug, Copy, Clone)]
pub struct A<'a>(pub &'a str);
impl Encoder for A<'_> {
    fn encode<S: Write>(&self, writer: &mut Writer<S>) -> Result<()> {
        writer.atom(self.0)
    }
}

macro_rules! impl_tuple_encoder {
	($count:expr, ($(($typ:ident, $name:ident)),*)) => {
        impl<$($typ: Encoder, )*> Encoder for ($($typ, )*) {
            fn encode<S: Write>(&self, writer: &mut Writer<S>) -> Result<()> {
                writer.tuple($count)?;

                let ($($name,)*) = self;
                $(
                    $name.encode(writer)?;
                )*

                writer.pop();
                Ok(())
            }
        }
	};
}

impl_tuple_encoder!(0, ());
impl_tuple_encoder!(1, ((A, a)));
impl_tuple_encoder!(2, ((A, a), (B, b)));
impl_tuple_encoder!(3, ((A, a), (B, b), (C, c)));
impl_tuple_encoder!(4, ((A, a), (B, b), (C, c), (D, d)));
impl_tuple_encoder!(5, ((A, a), (B, b), (C, c), (D, d), (E, e)));
impl_tuple_encoder!(6, ((A, a), (B, b), (C, c), (D, d), (E, e), (F, f)));
impl_tuple_encoder!(7, ((A, a), (B, b), (C, c), (D, d), (E, e), (F, f), (G, g)));
impl_tuple_encoder!(
    8,
    (
        (A, a),
        (B, b),
        (C, c),
        (D, d),
        (E, e),
        (F, f),
        (G, g),
        (H, h)
    )
);
impl_tuple_encoder!(
    9,
    (
        (A, a),
        (B, b),
        (C, c),
        (D, d),
        (E, e),
        (F, f),
        (G, g),
        (H, h),
        (I, i)
    )
);
impl_tuple_encoder!(
    10,
    (
        (A, a),
        (B, b),
        (C, c),
        (D, d),
        (E, e),
        (F, f),
        (G, g),
        (H, h),
        (I, i),
        (J, j)
    )
);

//impl<T> Encoder for T
//where
//    T: crate::writer::WritableInteger,
//{
//    fn encode<S: Write>(&self, writer: &mut Writer<S>) -> Result<()> {
//        writer.integer(self)
//    }
//}

impl<K: Encoder, V: Encoder> Encoder for HashMap<K, V> {
    fn encode<S: Write>(&self, writer: &mut Writer<S>) -> Result<()> {
        let len = self.len();
        writer.map(len)?;

        for (k, v) in self.iter() {
            k.encode(writer)?;
            v.encode(writer)?;
            writer.next_kv();
        }

        writer.pop();
        Ok(())
    }
}
impl<K: Encoder, V: Encoder> Encoder for BTreeMap<K, V> {
    fn encode<S: Write>(&self, writer: &mut Writer<S>) -> Result<()> {
        let len = self.len();
        writer.map(len)?;

        for (k, v) in self.iter() {
            k.encode(writer)?;
            v.encode(writer)?;
            writer.next_kv();
        }

        writer.pop();
        Ok(())
    }
}

impl Encoder for BigInt {
    fn encode<S: Write>(&self, writer: &mut Writer<S>) -> Result<()> {
        writer.integer_big(self)
    }
}
impl Encoder for u8 {
    fn encode<S: Write>(&self, writer: &mut Writer<S>) -> Result<()> {
        writer.raw_small_integer_ext(*self)
    }
}
impl Encoder for i8 {
    fn encode<S: Write>(&self, writer: &mut Writer<S>) -> Result<()> {
        writer.raw_integer_ext(*self as i32)
    }
}
impl Encoder for u16 {
    fn encode<S: Write>(&self, writer: &mut Writer<S>) -> Result<()> {
        writer.raw_integer_ext(*self as i32)
    }
}
impl Encoder for i16 {
    fn encode<S: Write>(&self, writer: &mut Writer<S>) -> Result<()> {
        writer.raw_integer_ext(*self as i32)
    }
}
impl Encoder for u32 {
    fn encode<S: Write>(&self, writer: &mut Writer<S>) -> Result<()> {
        if let Ok(num) = (*self).try_into() {
            writer.raw_integer_ext(num)
        } else {
            unimplemented!()
        }
    }
}
impl Encoder for i32 {
    fn encode<S: Write>(&self, writer: &mut Writer<S>) -> Result<()> {
        writer.raw_integer_ext(*self)
    }
}
impl Encoder for u64 {
    fn encode<S: Write>(&self, writer: &mut Writer<S>) -> Result<()> {
        if let Ok(num) = (*self).try_into() {
            writer.raw_integer_ext(num)
        } else {
            unimplemented!()
        }
    }
}
impl Encoder for i64 {
    fn encode<S: Write>(&self, writer: &mut Writer<S>) -> Result<()> {
        if let Ok(num) = (*self).try_into() {
            writer.raw_integer_ext(num)
        } else {
            unimplemented!()
        }
    }
}
impl Encoder for usize {
    fn encode<S: Write>(&self, writer: &mut Writer<S>) -> Result<()> {
        if let Ok(num) = (*self).try_into() {
            writer.raw_integer_ext(num)
        } else {
            unimplemented!()
        }
    }
}
impl Encoder for isize {
    fn encode<S: Write>(&self, writer: &mut Writer<S>) -> Result<()> {
        if let Ok(num) = (*self).try_into() {
            writer.raw_integer_ext(num)
        } else {
            unimplemented!()
        }
    }
}

pub struct List<H: IntoIterator<Item = I> + Copy, I: Encoder, T: Encoder>(pub H, pub usize, pub T);
impl<H: IntoIterator<Item = I> + Copy, I: Encoder, T: Encoder> Encoder for List<H, I, T> {
    fn encode<S: Write>(&self, writer: &mut Writer<S>) -> Result<()> {
        writer.list(self.1)?;

        for entry in self.0.into_iter() {
            entry.encode(writer)?;
        }

        writer.next_tail();
        self.2.encode(writer)?;

        writer.pop();
        Ok(())
    }
}
