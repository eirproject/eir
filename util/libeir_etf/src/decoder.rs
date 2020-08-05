use crate::{RawTag, Reader};
use std::io::Read;

use snafu::{ResultExt, Snafu};

#[derive(Debug, Snafu)]
pub enum DecodeError {
    #[snafu(display("error from source: {}", source))]
    Source { source: std::io::Error },

    #[snafu(display("tried to decode bad data"))]
    BadData,
}

pub trait Decoder: Sized {
    fn decode<S: Read>(reader: &mut Reader<S>) -> Result<Self, DecodeError>;
}

macro_rules! impl_tuple_decoder {
    ($count:expr, ($($typ:ident),*)) => {
        impl<$($typ: Decoder, )*> Decoder for ($($typ, )*) {
            fn decode<S: Read>(reader: &mut Reader<S>) -> Result<Self, DecodeError> {
                if let RawTag::Tuple { arity } = reader.raw_tag().context(Source)? {
                    if arity == $count {
                        return Ok((
                            $(<$typ as Decoder>::decode(reader)?,)*
                        ));
                    }
                }
                Err(DecodeError::BadData)
            }
        }
    };
}

impl_tuple_decoder!(0, ());
impl_tuple_decoder!(1, (A));
impl_tuple_decoder!(2, (A, B));
impl_tuple_decoder!(3, (A, B, C));
impl_tuple_decoder!(4, (A, B, C, D));
impl_tuple_decoder!(5, (A, B, C, D, E));
impl_tuple_decoder!(6, (A, B, C, D, E, F));
impl_tuple_decoder!(7, (A, B, C, D, E, F, G));
impl_tuple_decoder!(8, (A, B, C, D, E, F, G, H));
impl_tuple_decoder!(9, (A, B, C, D, E, F, G, H, I));
impl_tuple_decoder!(10, (A, B, C, D, E, F, G, H, I, J));

macro_rules! ignore {
    {$a:tt} => {};
}

#[macro_export]
macro_rules! impl_record {
    ($struct:ident, $name:expr, {$($field:ident),*}) => {
        impl Decoder for $name {
            fn decode<S: Read>(reader: &mut Reader<S>) -> Result<Self, DecodeError> {
                let num_fields = 1 $(+ 1 ignore!($field))*;
                if let RawTag::Tuple { arity } = reader.raw_tag().context(Source)? {
                    if arity == num_fields {
                    }
                }
                Err(DecodeError::BadData)
            }
        }
    };
}
