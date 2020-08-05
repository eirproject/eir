use crate::term::Term;
use byteorder::{BigEndian, ReadBytesExt};
use std::io::{Read, Result, Seek};

use super::constants::tag;

pub struct Reader<S> {
    source: S,
}

impl<S> Reader<S> {
    pub fn new(source: S) -> Self {
        Reader { source }
    }
}

pub enum RawTag {
    AtomCacheRef {
        index: u8,
    },
    SmallIntegerExt {
        int: u8,
    },
    IntegerExt {
        int: i32,
    },
    FloatExt,
    PortExt,
    NewPortExt,
    PidExt,
    NewPidExt,
    Tuple {
        arity: u32,
    },
    MapExt {
        arity: u32,
    },
    NilExt,
    StringExt {
        length: u16,
    },
    ListExt {
        length: u32,
    },
    BinaryExt {
        length: u32,
    },
    SmallBigExt {
        data_len: u8,
        is_neg: bool,
    },
    LargeBigExt {
        data_len: u32,
        is_neg: bool,
    },
    NewerReferenceExt {
        id_len: u16,
    },
    FunExt {
        num_free: u32,
    },
    NewFunExt {
        size: u32,
        arity: u8,
        uniq: [u8; 16],
        index: u32,
        num_free: u32,
    },
    ExportExt,
    BitBinaryExt {
        length: u32,
        bits: u8,
    },
    NewFloatExt {
        num: f32,
    },
    Atom {
        len: u16,
    },
    AtomUtf8 {
        len: u16,
    },
}

macro_rules! trace {
    ($target:expr) => {
        println!($target);
    };
}

impl<S: Read> Reader<S> {
    pub fn header(&mut self) -> Result<()> {
        let byte = self.source.read_u8()?;
        if byte != 131 {
            panic!("invalid header");
        }
        Ok(())
    }

    pub fn raw_tag(&mut self) -> Result<RawTag> {
        let tag_int = self.source.read_u8()?;

        let res = match tag_int {
            tag::ATOM_CACHE_REF => {
                trace!("atom_cache_ref");
                let index = self.source.read_u8()?;
                RawTag::AtomCacheRef { index }
            }
            tag::SMALL_INTEGER_EXT => {
                trace!("small_integer_ext");
                let int = self.source.read_u8()?;
                RawTag::SmallIntegerExt { int }
            }
            tag::INTEGER_EXT => {
                trace!("integer_ext");
                let int = self.source.read_i32::<BigEndian>()?;
                RawTag::IntegerExt { int }
            }
            tag::FLOAT_EXT => {
                trace!("float_ext");
                RawTag::FloatExt
            }
            tag::PORT_EXT => {
                trace!("port_ext");
                RawTag::PortExt
            }
            tag::NEW_PORT_EXT => {
                trace!("new_port_ext");
                RawTag::NewPortExt
            }
            tag::PID_EXT => {
                trace!("pid_ext");
                RawTag::PidExt
            }
            tag::NEW_PID_EXT => {
                trace!("new_pid_ext");
                RawTag::NewPidExt
            }
            tag::SMALL_TUPLE_EXT => {
                trace!("small_tuple_ext");
                let arity = self.source.read_u8()?;
                RawTag::Tuple {
                    arity: arity as u32,
                }
            }
            tag::LARGE_TUPLE_EXT => {
                trace!("large_tuple_ext");
                let arity = self.source.read_u32::<BigEndian>()?;
                RawTag::Tuple { arity }
            }
            tag::MAP_EXT => {
                trace!("map_ext");
                let arity = self.source.read_u32::<BigEndian>()?;
                RawTag::MapExt { arity }
            }
            tag::NIL_EXT => {
                trace!("nil_ext");
                RawTag::NilExt
            }
            tag::STRING_EXT => {
                trace!("string_ext");
                let length = self.source.read_u16::<BigEndian>()?;
                RawTag::StringExt { length }
            }
            tag::LIST_EXT => {
                trace!("list_ext");
                let length = self.source.read_u32::<BigEndian>()?;
                RawTag::ListExt { length }
            }
            tag::BINARY_EXT => {
                trace!("binary_ext");
                let length = self.source.read_u32::<BigEndian>()?;
                RawTag::BinaryExt { length }
            }
            tag::SMALL_BIG_EXT => {
                trace!("small_big_ext");
                let data_len = self.source.read_u8()?;
                let is_neg = self.source.read_u8()? == 1;
                RawTag::SmallBigExt { data_len, is_neg }
            }
            tag::LARGE_BIG_EXT => {
                trace!("large_big_ext");
                let data_len = self.source.read_u32::<BigEndian>()?;
                let is_neg = self.source.read_u8()? == 1;
                RawTag::LargeBigExt { data_len, is_neg }
            }
            tag::NEWER_REFERENCE_EXT => {
                trace!("newer_reference_ext");
                let id_len = self.source.read_u16::<BigEndian>()?;
                RawTag::NewerReferenceExt { id_len }
            }
            tag::FUN_EXT => {
                trace!("fun_ext");
                let num_free = self.source.read_u32::<BigEndian>()?;
                RawTag::FunExt { num_free }
            }
            tag::NEW_FUN_EXT => {
                trace!("new_fun_ext");
                let size = self.source.read_u32::<BigEndian>()?;
                let arity = self.source.read_u8()?;
                let mut uniq = [0u8; 16];
                self.source.read_exact(&mut uniq)?;
                let index = self.source.read_u32::<BigEndian>()?;
                let num_free = self.source.read_u32::<BigEndian>()?;
                RawTag::NewFunExt {
                    size,
                    arity,
                    uniq,
                    index,
                    num_free,
                }
            }
            tag::EXPORT_EXT => {
                trace!("export_ext");
                RawTag::ExportExt
            }
            tag::BIT_BINARY_EXT => {
                trace!("bit_binary_ext");
                let length = self.source.read_u32::<BigEndian>()?;
                let bits = self.source.read_u8()?;
                RawTag::BitBinaryExt { length, bits }
            }
            tag::NEW_FLOAT_EXT => {
                trace!("new_float_ext");
                let num = self.source.read_f32::<BigEndian>()?;
                RawTag::NewFloatExt { num }
            }
            tag::ATOM_EXT => {
                trace!("atom_ext");
                let len = self.source.read_u16::<BigEndian>()?;
                RawTag::Atom { len }
            }
            tag::SMALL_ATOM_EXT => {
                trace!("small_atom_ext");
                let len = self.source.read_u8()?;
                RawTag::Atom { len: len as u16 }
            }
            tag::ATOM_UTF8_EXT => {
                trace!("atom_utf8_ext");
                let len = self.source.read_u16::<BigEndian>()?;
                RawTag::AtomUtf8 { len }
            }
            tag::SMALL_ATOM_UTF8_EXT => {
                trace!("small_atom_utf8_ext");
                let len = self.source.read_u8()?;
                RawTag::AtomUtf8 { len: len as u16 }
            }
            tag => unreachable!("unknown tag {}", tag),
        };
        Ok(res)
    }
    pub fn term(&mut self) -> Result<Term> {
        use RawTag::*;
        let res = match self.raw_tag()? {
            AtomUtf8 { len } => self.read_atom_term_utf8(len as usize)?,
            Atom { len } => self.read_atom_term_latin1(len as usize)?,
            SmallIntegerExt { int } => Term::Integer(int as i32),
            IntegerExt { int } => Term::Integer(int),
            NewFloatExt { num } => Term::Float(num),
            NilExt => Term::Nil,

            BinaryExt { length } => {
                let mut data = vec![0; length as usize];
                self.source.read_exact(&mut data[..])?;
                Term::Binary(data)
            }
            BitBinaryExt { length, bits } => {
                let mut data = vec![0; length as usize];
                self.source.read_exact(&mut data[..])?;
                Term::BitBinary(data, bits)
            }

            Tuple { arity } => {
                let terms = (0..arity).map(|_| self.term()).collect::<Result<_>>()?;
                Term::Tuple(terms)
            }
            MapExt { arity } => {
                let terms = (0..arity)
                    .map(|_| Ok((self.term()?, self.term()?)))
                    .collect::<Result<_>>()?;
                Term::Map(terms)
            }
            StringExt { length } => {
                let mut data = vec![0; length as usize];
                self.source.read_exact(&mut data[..])?;
                Term::ByteList(data)
            }
            ListExt { length } => {
                let head = (0..length).map(|_| self.term()).collect::<Result<_>>()?;
                let tail = self.term()?;
                Term::List(head, Box::new(tail))
            }

            _ => unimplemented!(),
        };
        Ok(res)
    }

    fn read_atom_term_latin1(&mut self, len: usize) -> Result<Term> {
        let mut data = vec![0; len];
        self.source.read_exact(&mut data[..])?;

        for byte in data.iter() {
            assert!(*byte < 128, "TODO latin1 characters");
        }

        match String::from_utf8(data) {
            Ok(string) => Ok(Term::Atom(string)),
            _ => unreachable!(),
        }
    }

    fn read_atom_term_utf8(&mut self, len: usize) -> Result<Term> {
        let mut data = vec![0; len];
        self.source.read_exact(&mut data[..])?;
        match String::from_utf8(data) {
            Ok(string) => Ok(Term::Atom(string)),
            _ => panic!("invalid atom"),
        }
    }
}
