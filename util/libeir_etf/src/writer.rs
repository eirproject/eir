use byteorder::{BigEndian, WriteBytesExt};
use num_bigint::{BigInt, Sign};
use std::convert::TryInto;
use std::io::{Result, Write};

use super::constants::tag;
use super::Term;

pub struct Writer<S> {
    sink: S,
    state: Vec<WriterState>,
}

#[derive(Debug, Copy, Clone)]
enum WriterState {
    RemainingTerms(usize),
    RemainingMap(usize, u8),
    RemainingList(usize),
    RemainingData(usize),
}
impl WriterState {
    fn decr(&mut self) {
        match self {
            WriterState::RemainingTerms(n) if *n > 0 => *n -= 1,
            WriterState::RemainingMap(n, s) if *n > 0 && *s < 2 => *s += 1,
            WriterState::RemainingList(n) if *n > 0 => *n -= 1,
            _ => panic!("no term expected at current level"),
        }
    }
}

impl<S> Writer<S> {
    pub fn new(sink: S) -> Self {
        Writer {
            sink,
            state: vec![WriterState::RemainingTerms(1)],
        }
    }

    pub fn pop(&mut self) {
        match self.state.pop().expect("tried to pop empty stack") {
            WriterState::RemainingTerms(0) => (),
            _ => panic!("tried to pop invalid state"),
        }
    }

    pub fn next_kv(&mut self) {
        match self.state.last_mut().expect("tried to next on empty stack") {
            WriterState::RemainingMap(0, _) => panic!("no map keys left"),
            WriterState::RemainingMap(n, s @ 2) => {
                *n -= 1;
                *s = 0;
            }
            _ => panic!("tried to next on invalid state"),
        }
    }

    pub fn next_tail(&mut self) {
        match self.state.last_mut().expect("tried to next on empty stack") {
            a @ WriterState::RemainingList(0) => *a = WriterState::RemainingTerms(1),
            _ => panic!("tried to next on invalid state"),
        }
    }
}

/// Generic
impl<S: Write> Writer<S> {
    pub fn push_data(&mut self, data: &[u8]) -> Result<()> {
        let last_state = self.state.pop().unwrap();
        if let WriterState::RemainingData(mut rem) = last_state {
            assert!(rem <= data.len());
            rem -= data.len();
            if rem > 0 {
                self.state.push(WriterState::RemainingData(rem));
            }

            self.sink.write_all(data)
        } else {
            panic!()
        }
    }
}

/// Raw interface
impl<S: Write> Writer<S> {
    pub fn raw_small_integer_ext(&mut self, int: u8) -> Result<()> {
        self.state.last_mut().unwrap().decr();
        self.sink.write_u8(tag::SMALL_INTEGER_EXT)?;
        self.sink.write_u8(int)
    }

    pub fn raw_integer_ext(&mut self, int: i32) -> Result<()> {
        self.state.last_mut().unwrap().decr();
        self.sink.write_u8(tag::INTEGER_EXT)?;
        self.sink.write_i32::<BigEndian>(int)
    }

    pub fn raw_big_ext(&mut self, int: &BigInt) -> Result<()> {
        self.state.last_mut().unwrap().decr();

        let (sign, bytes) = int.to_bytes_be();
        let bytes_len = bytes.len();

        let sign_u8 = if sign == Sign::Minus { 1 } else { 0 };

        if let Ok(len) = bytes_len.try_into() {
            self.sink.write_u8(tag::SMALL_BIG_EXT)?;
            self.sink.write_u8(len)?;
            self.sink.write_u8(sign_u8)?;
            self.sink.write_all(&bytes)
        } else if let Ok(len) = bytes_len.try_into() {
            self.sink.write_u8(tag::LARGE_BIG_EXT)?;
            self.sink.write_u32::<BigEndian>(len)?;
            self.sink.write_u8(sign_u8)?;
            self.sink.write_all(&bytes)
        } else {
            panic!("number too large")
        }
    }

    pub fn raw_new_float_ext(&mut self, float: f32) -> Result<()> {
        self.state.last_mut().unwrap().decr();
        self.sink.write_u8(tag::NEW_FLOAT_EXT)?;
        self.sink.write_f32::<BigEndian>(float)
    }

    pub fn raw_nil(&mut self) -> Result<()> {
        self.state.last_mut().unwrap().decr();
        self.sink.write_u8(tag::NIL_EXT)
    }

    pub fn raw_atom_utf8_ext(&mut self, data: &str) -> Result<()> {
        let bytes = data.as_bytes();
        let len_u16: u16 = bytes.len().try_into().unwrap();

        self.state.last_mut().unwrap().decr();
        self.sink.write_u8(tag::ATOM_UTF8_EXT)?;
        self.sink.write_u16::<BigEndian>(len_u16)?;
        self.sink.write_all(bytes)
    }

    pub fn raw_small_atom_utf8_ext(&mut self, data: &str) -> Result<()> {
        let bytes = data.as_bytes();
        let len_u8: u8 = bytes.len().try_into().unwrap();

        self.state.last_mut().unwrap().decr();
        self.sink.write_u8(tag::SMALL_ATOM_UTF8_EXT)?;
        self.sink.write_u8(len_u8)?;
        self.sink.write_all(bytes)
    }

    pub fn raw_small_tuple_ext(&mut self, arity: u8) -> Result<()> {
        self.state.last_mut().unwrap().decr();
        self.state.push(WriterState::RemainingTerms(arity as usize));

        self.sink.write_u8(tag::SMALL_TUPLE_EXT)?;
        self.sink.write_u8(arity)
    }

    pub fn raw_large_tuple_ext(&mut self, arity: u16) -> Result<()> {
        self.state.last_mut().unwrap().decr();
        self.state.push(WriterState::RemainingTerms(arity as usize));

        self.sink.write_u8(tag::LARGE_TUPLE_EXT)?;
        self.sink.write_u16::<BigEndian>(arity)
    }

    pub fn raw_map_ext(&mut self, size: u32) -> Result<()> {
        self.state.last_mut().unwrap().decr();
        self.state.push(WriterState::RemainingMap(size as usize, 0));

        self.sink.write_u8(tag::MAP_EXT)?;
        self.sink.write_u32::<BigEndian>(size)
    }

    pub fn raw_list_ext(&mut self, length: u32) -> Result<()> {
        self.state.last_mut().unwrap().decr();
        self.state.push(WriterState::RemainingList(length as usize));

        self.sink.write_u8(tag::LIST_EXT)?;
        self.sink.write_u32::<BigEndian>(length)
    }

    pub fn raw_binary_ext(&mut self, length: u32) -> Result<()> {
        self.state.last_mut().unwrap().decr();
        self.state.push(WriterState::RemainingData(length as usize));

        self.sink.write_u8(tag::BINARY_EXT)?;
        self.sink.write_u32::<BigEndian>(length)
    }

    pub fn raw_bit_binary_ext(&mut self, length: u32, last_bits: u8) -> Result<()> {
        debug_assert!(last_bits <= 7);

        self.state.last_mut().unwrap().decr();
        self.state.push(WriterState::RemainingData(length as usize));

        self.sink.write_u8(tag::BIT_BINARY_EXT)?;
        self.sink.write_u32::<BigEndian>(length)?;
        self.sink.write_u8(last_bits)
    }
}

/// Normal API for terminals
impl<S: Write> Writer<S> {
    pub fn integer_u8(&mut self, int: u8) -> Result<()> {
        self.raw_small_integer_ext(int)
    }
    pub fn integer_i32(&mut self, int: i32) -> Result<()> {
        self.raw_integer_ext(int)
    }
    pub fn integer_big(&mut self, int: &BigInt) -> Result<()> {
        self.raw_big_ext(int)
    }

    pub fn float(&mut self, num: f32) -> Result<()> {
        self.raw_new_float_ext(num)
    }
    pub fn nil(&mut self) -> Result<()> {
        self.raw_nil()
    }
    pub fn atom(&mut self, string: &str) -> Result<()> {
        let len = string.len();
        if let Ok(_len) = TryInto::<u8>::try_into(len) {
            self.raw_small_atom_utf8_ext(string)
        } else if let Ok(_len) = TryInto::<u16>::try_into(len) {
            self.raw_atom_utf8_ext(string)
        } else {
            panic!("atom cannot be longer than 2^16");
        }
    }
    pub fn tuple(&mut self, len: usize) -> Result<()> {
        if let Ok(len) = len.try_into() {
            self.raw_small_tuple_ext(len)
        } else if let Ok(len) = len.try_into() {
            self.raw_large_tuple_ext(len)
        } else {
            panic!("tuple cannot be longer than 2^16");
        }
    }
    pub fn map<N: TryInto<u32>>(&mut self, len: N) -> Result<()> {
        if let Ok(len) = len.try_into() {
            self.raw_map_ext(len)
        } else {
            panic!("map cannot be longer than 2^32");
        }
    }
    pub fn list<N: TryInto<u32>>(&mut self, len: N) -> Result<()> {
        if let Ok(len) = len.try_into() {
            self.raw_list_ext(len)
        } else {
            panic!("list cannot be longer than 2^32");
        }
    }
    pub fn binary<N: TryInto<u32>>(&mut self, len: N) -> Result<()> {
        if let Ok(len) = len.try_into() {
            self.raw_binary_ext(len)
        } else {
            panic!("binary cannot be longer than 2^32");
        }
    }
    pub fn bit_binary<N: TryInto<u32>>(&mut self, len: N, last_bits: u8) -> Result<()> {
        if let Ok(len) = len.try_into() {
            self.raw_bit_binary_ext(len, last_bits)
        } else {
            panic!("binary cannot be longer than 2^32");
        }
    }
}

impl<S: Write> Writer<S> {
    pub fn term(&mut self, term: &Term) -> Result<()> {
        match term {
            Term::Float(num) => self.float(*num),
            Term::Integer(int) => {
                if let Ok(int) = (*int).try_into() {
                    self.integer_u8(int)
                } else if let Ok(int) = (*int).try_into() {
                    self.integer_i32(int)
                } else {
                    unimplemented!()
                }
            }
            Term::Nil => self.nil(),

            Term::Tuple(elems) => {
                self.tuple(elems.len())?;
                for elem in elems {
                    self.term(elem)?;
                }
                self.pop();
                Ok(())
            }
            Term::List(elems, tail) => {
                self.list(elems.len())?;
                for elem in elems {
                    self.term(elem)?;
                }
                self.next_tail();
                self.term(tail)?;
                self.pop();
                Ok(())
            }
            Term::Map(elems) => {
                self.map(elems.len())?;
                for (k, v) in elems {
                    self.term(k)?;
                    self.term(v)?;
                    self.next_kv();
                }
                self.pop();
                Ok(())
            }
            _ => unimplemented!(),
        }
    }
}
