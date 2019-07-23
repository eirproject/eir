#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Endianness {
    Big,
    Little,
    Native,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum EntrySpecifier {
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

impl EntrySpecifier {

    pub fn has_size(&self) -> bool {
        match self {
            EntrySpecifier::Utf8 => false,
            EntrySpecifier::Utf16 { .. } => false,
            EntrySpecifier::Utf32 { .. } => false,
            _ => true,
        }
    }

}
