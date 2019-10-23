use std::str::FromStr;
use std::fmt::{Display, Formatter};
use std::convert::TryInto;

use num_traits::{ToPrimitive, FromPrimitive};
use num_bigint::{BigInt, ParseBigIntError};

#[derive(Debug, Clone)]
pub enum Integer {
    Small(i64),
    Big(BigInt),
}
impl Display for Integer {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Integer::Small(int) => int.fmt(f),
            Integer::Big(int) => int.fmt(f),
        }
    }
}

impl FromStr for Integer {
    type Err = ParseBigIntError;
    fn from_str(s: &str) -> Result<Self, ParseBigIntError> {
        match s.parse::<i64>() {
            Ok(int) => Ok(Integer::Small(int)),
            Err(_) => {
                match s.parse::<BigInt>() {
                    Ok(int) => Ok(Integer::Big(int)),
                    Err(err) => Err(err),
                }
            },
        }
    }
}

impl PartialEq for Integer {
    fn eq(&self, rhs: &Integer) -> bool {
        match (self, rhs) {
            (Integer::Small(lhs), Integer::Small(rhs)) => lhs.eq(rhs),
            (Integer::Small(lhs), Integer::Big(rhs)) => lhs.eq(rhs),
            (Integer::Big(lhs), Integer::Small(rhs)) => lhs.eq(rhs),
            (Integer::Big(lhs), Integer::Big(rhs)) => lhs.eq(rhs),
        }
    }
}
impl Eq for Integer {}

impl ToPrimitive for Integer {
    fn to_i64(&self) -> Option<i64> {
        match self {
            Integer::Small(int) => int.to_i64(),
            Integer::Big(int) => int.to_i64(),
        }
    }
    fn to_u64(&self) -> Option<u64> {
        match self {
            Integer::Small(int) => int.to_u64(),
            Integer::Big(int) => int.to_u64(),
        }
    }
}

impl FromPrimitive for Integer {
    fn from_i64(n: i64) -> Option<Integer> {
        Some(Integer::Small(n))
    }
    fn from_u64(n: u64) -> Option<Integer> {
        if let Ok(int) = n.try_into() {
            Some(Integer::Small(int))
        } else {
            Some(Integer::Big(n.into()))
        }
    }
}

impl From<i64> for Integer {
    fn from(i: i64) -> Integer {
        Integer::from_i64(i).unwrap()
    }
}
impl From<i32> for Integer {
    fn from(i: i32) -> Integer {
        Integer::from_i32(i).unwrap()
    }
}
impl From<usize> for Integer {
    fn from(i: usize) -> Integer {
        Integer::from_usize(i).unwrap()
    }
}
