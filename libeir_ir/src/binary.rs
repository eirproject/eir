use serde::{Deserialize, Serialize};
use std::default::Default;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Endianness {
    Big,
    Little,
    Native,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum BinaryEntrySpecifier {
    Integer {
        signed: bool,
        endianness: Endianness,
        unit: i64,
    },
    Float {
        endianness: Endianness,
        unit: i64,
    },
    Bytes {
        unit: i64,
    },
    Bits {
        unit: i64,
    },
    Utf8,
    Utf16 {
        endianness: Endianness,
    },
    Utf32 {
        endianness: Endianness,
    },
}

impl BinaryEntrySpecifier {
    pub fn has_size(&self) -> bool {
        match self {
            BinaryEntrySpecifier::Utf8 => false,
            BinaryEntrySpecifier::Utf16 { .. } => false,
            BinaryEntrySpecifier::Utf32 { .. } => false,
            _ => true,
        }
    }
    pub fn is_native_endian(&self) -> bool {
        match self {
            BinaryEntrySpecifier::Integer {
                endianness: Endianness::Native,
                ..
            } => true,
            BinaryEntrySpecifier::Float {
                endianness: Endianness::Native,
                ..
            } => true,
            BinaryEntrySpecifier::Utf16 {
                endianness: Endianness::Native,
                ..
            } => true,
            BinaryEntrySpecifier::Utf32 {
                endianness: Endianness::Native,
                ..
            } => true,
            _ => false,
        }
    }
}

impl Default for BinaryEntrySpecifier {
    fn default() -> Self {
        BinaryEntrySpecifier::Integer {
            signed: false,
            endianness: Endianness::Big,
            unit: 8,
        }
    }
}
