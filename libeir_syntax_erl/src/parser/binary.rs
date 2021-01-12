use snafu::Snafu;
pub use libeir_ir::binary::{BinaryEntrySpecifier, Endianness};
use libeir_diagnostics::{SourceSpan, ToDiagnostic, Diagnostic, Label};
use super::ast::{BitType, BinaryElement, Expr, Literal};
use crate::lower::LowerError;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum TypeName {
    Integer,
    Float,
    Bytes,
    Bits,
    Utf8,
    Utf16,
    Utf32,
}
impl std::fmt::Display for TypeName {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            TypeName::Integer => write!(f, "integer"),
            TypeName::Float => write!(f, "float"),
            TypeName::Bytes => write!(f, "binary"),
            TypeName::Bits => write!(f, "bitstring"),
            TypeName::Utf8 => write!(f, "utf8"),
            TypeName::Utf16 => write!(f, "utf16"),
            TypeName::Utf32 => write!(f, "utf32"),
        }
    }
}

pub fn default_specifier() -> BinaryEntrySpecifier {
    BinaryEntrySpecifier::Integer {
        signed: false,
        endianness: Endianness::Big,
        unit: 1,
    }
}

#[derive(Debug, Snafu)]
pub enum SpecifierError {
    #[snafu(display("unknown specifier in binary entry"))]
    BinaryUnknownSpecifier {
        span: SourceSpan,
    },
    #[snafu(display("conflicting specifiers in binary entry"))]
    BinaryConflictingSpecifier {
        new: SourceSpan,
        old: SourceSpan,
    },
    #[snafu(display("invalid specifier for {} in binary entry", typ))]
    BinaryInvalidSpecifier {
        span: SourceSpan,
        typ: TypeName,
    },
    #[snafu(display("size is not allowed for {}", typ))]
    BinarySizeNotAllowed {
        span: SourceSpan,
        typ: TypeName,
    },
}

impl ToDiagnostic for SpecifierError {
    fn to_diagnostic(&self) -> Diagnostic {
        let msg = self.to_string();
        match self {
            SpecifierError::BinaryUnknownSpecifier { span } => Diagnostic::error()
                .with_message(msg)
                .with_labels(vec![
                    Label::primary(span.source_id(), *span).with_message("specifier is not known")
                ]),
            SpecifierError::BinaryConflictingSpecifier { new, old } => {
                Diagnostic::error().with_message(msg).with_labels(vec![
                    Label::primary(new.source_id(), *new).with_message("specifier 1"),
                    Label::primary(old.source_id(), *old).with_message("specifier 2"),
                ])
            }
            SpecifierError::BinaryInvalidSpecifier { span, typ } => Diagnostic::error()
                .with_message(msg)
                .with_labels(vec![Label::primary(span.source_id(), *span)
                                  .with_message(format!("specifier is not valid for {} entries", typ))]),
            SpecifierError::BinarySizeNotAllowed { span, typ } => Diagnostic::error()
                .with_message(msg)
                .with_labels(vec![Label::primary(span.source_id(), *span)
                                  .with_message(format!("size is not allowed for {} entries", typ))]),
        }
    }
}

macro_rules! try_specifier {
    ($field:expr, $entry:expr, $spec:expr) => {{
        let spec = $spec;
        match $field {
            Some((f, _)) if f == spec => (),
            Some((_, old)) => {
                return Err(SpecifierError::BinaryConflictingSpecifier {
                    old,
                    new: $entry.span(),
                });
            }
            None => $field = Some((spec, $entry.span())),
        }
    }};
}

macro_rules! test_none {
    ($field:expr, $typ:expr) => {{
        if let Some((_, span)) = $field {
            return Err(SpecifierError::BinaryInvalidSpecifier {
                span: span,
                typ: $typ,
            });
        }
    }};
}

pub fn specifier_from_parsed(parsed: &[BitType], has_size: bool) -> Result<BinaryEntrySpecifier, SpecifierError> {
    let mut raw_typ = None;
    let mut signed = None;
    let mut endianness = None;
    let mut unit = None;

    for entry in parsed {
        match entry {
            // Types
            BitType::Name(_id, _span, ident) if ident.as_str() == "integer" => {
                try_specifier!(raw_typ, entry, TypeName::Integer)
            }
            BitType::Name(_id, _span, ident) if ident.as_str() == "float" => {
                try_specifier!(raw_typ, entry, TypeName::Float)
            }
            BitType::Name(_id, _span, ident) if ident.as_str() == "binary" => {
                try_specifier!(raw_typ, entry, TypeName::Bytes)
            }
            BitType::Name(_id, _span, ident) if ident.as_str() == "bytes" => {
                try_specifier!(raw_typ, entry, TypeName::Bytes)
            }
            BitType::Name(_id, _span, ident) if ident.as_str() == "bitstring" => {
                try_specifier!(raw_typ, entry, TypeName::Bits)
            }
            BitType::Name(_id, _span, ident) if ident.as_str() == "bits" => {
                try_specifier!(raw_typ, entry, TypeName::Bits)
            }
            BitType::Name(_id, _span, ident) if ident.as_str() == "utf8" => {
                try_specifier!(raw_typ, entry, TypeName::Utf8)
            }
            BitType::Name(_id, _span, ident) if ident.as_str() == "utf16" => {
                try_specifier!(raw_typ, entry, TypeName::Utf16)
            }
            BitType::Name(_id, _span, ident) if ident.as_str() == "utf32" => {
                try_specifier!(raw_typ, entry, TypeName::Utf32)
            }

            // Signed
            BitType::Name(_id, _span, ident) if ident.as_str() == "signed" => {
                try_specifier!(signed, entry, true)
            }
            BitType::Name(_id, _span, ident) if ident.as_str() == "unsigned" => {
                try_specifier!(signed, entry, false)
            }

            // Endianness
            BitType::Name(_id, _span, ident) if ident.as_str() == "big" => {
                try_specifier!(endianness, entry, Endianness::Big)
            }
            BitType::Name(_id, _span, ident) if ident.as_str() == "little" => {
                try_specifier!(endianness, entry, Endianness::Little)
            }
            BitType::Name(_id, _span, ident) if ident.as_str() == "native" => {
                try_specifier!(endianness, entry, Endianness::Native)
            }

            // Unit
            BitType::Sized(_id, _span, ident, num) if ident.as_str() == "unit" => {
                try_specifier!(unit, entry, *num)
            }

            entry => {
                return Err(SpecifierError::BinaryUnknownSpecifier { span: entry.span() });
            }
        }
    }

    let typ = raw_typ.map(|(t, _)| t).unwrap_or(TypeName::Integer);

    let size_not_allowed_err = || Err(SpecifierError::BinarySizeNotAllowed {
        typ,
        span: raw_typ.unwrap().1,
    });

    let spec = match typ {
        TypeName::Integer => {
            // Default is signed-big-unit:1
            let signed = signed.map(|(t, _)| t).unwrap_or(false);
            let endianness = endianness.map(|(t, _)| t).unwrap_or(Endianness::Big);
            let unit = unit.map(|(t, _)| t).unwrap_or(1);

            BinaryEntrySpecifier::Integer {
                signed,
                endianness,
                unit,
            }
        }
        TypeName::Float => {
            // Default is big-unit:1
            test_none!(signed, typ);
            let endianness = endianness.map(|(t, _)| t).unwrap_or(Endianness::Big);
            let unit = unit.map(|(t, _)| t).unwrap_or(1);

            BinaryEntrySpecifier::Float { endianness, unit }
        }
        TypeName::Bytes => {
            // Default is unit:8
            test_none!(signed, typ);
            test_none!(endianness, typ);
            let unit = unit.map(|(t, _)| t).unwrap_or(8);

            BinaryEntrySpecifier::Bytes { unit }
        }
        TypeName::Bits => {
            // Default is unit:1
            test_none!(signed, typ);
            test_none!(endianness, typ);
            let unit = unit.map(|(t, _)| t).unwrap_or(1);

            BinaryEntrySpecifier::Bits { unit }
        }
        TypeName::Utf8 => {
            test_none!(signed, typ);
            test_none!(endianness, typ);
            test_none!(unit, typ);

            if has_size {
                return size_not_allowed_err();
            }

            BinaryEntrySpecifier::Utf8
        }
        TypeName::Utf16 => {
            test_none!(signed, typ);
            let endianness = endianness.map(|(t, _)| t).unwrap_or(Endianness::Big);
            test_none!(unit, typ);

            if has_size {
                return size_not_allowed_err();
            }

            BinaryEntrySpecifier::Utf16 { endianness }
        }
        TypeName::Utf32 => {
            test_none!(signed, typ);
            let endianness = endianness.map(|(t, _)| t).unwrap_or(Endianness::Big);
            test_none!(unit, typ);

            if has_size {
                return size_not_allowed_err();
            }

            BinaryEntrySpecifier::Utf32 { endianness }
        }
    };

    Ok(spec)
}

pub fn specifier_to_typename(specifier: &BinaryEntrySpecifier) -> TypeName {
    match specifier {
        BinaryEntrySpecifier::Integer { .. } => TypeName::Integer,
        BinaryEntrySpecifier::Float { .. } => TypeName::Float,
        BinaryEntrySpecifier::Bytes { .. } => TypeName::Bytes,
        BinaryEntrySpecifier::Bits { .. } => TypeName::Bits,
        BinaryEntrySpecifier::Utf8 => TypeName::Utf8,
        BinaryEntrySpecifier::Utf16 { .. } => TypeName::Utf16,
        BinaryEntrySpecifier::Utf32 { .. } => TypeName::Utf32,
    }
}

pub fn specifier_can_have_size(specifier: &BinaryEntrySpecifier) -> bool {
    match specifier {
        BinaryEntrySpecifier::Utf8 => false,
        BinaryEntrySpecifier::Utf16 { .. } => false,
        BinaryEntrySpecifier::Utf32 { .. } => false,
        _ => true,
    }
}
