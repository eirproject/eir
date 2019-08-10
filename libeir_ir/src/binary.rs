#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Endianness {
    Big,
    Little,
    Native,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
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

}
