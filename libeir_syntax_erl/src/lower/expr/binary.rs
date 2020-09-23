use std::convert::TryInto;

pub use libeir_ir::binary::{BinaryEntrySpecifier, Endianness};
use libeir_ir::{
    operation::binary_construct::{
        BinaryConstructFinish, BinaryConstructPush, BinaryConstructStart,
    },
    Block as IrBlock, FunctionBuilder, Value as IrValue,
};

use crate::parser::ast::{Binary, BinaryElement, BitType, Expr, Literal};

use crate::lower::expr::lower_single_same_scope;
use crate::lower::strings::{
    string_to_binary, tokenize_string, Encoding as SEncoding, Endianness as SEndianness,
};
use crate::lower::{LowerCtx, LowerError};

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

pub fn specifier_can_have_size(specifier: &BinaryEntrySpecifier) -> bool {
    match specifier {
        BinaryEntrySpecifier::Utf8 => false,
        BinaryEntrySpecifier::Utf16 { .. } => false,
        BinaryEntrySpecifier::Utf32 { .. } => false,
        _ => true,
    }
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

macro_rules! try_specifier {
    ($field:expr, $entry:expr, $spec:expr) => {{
        let spec = $spec;
        match $field {
            Some((f, _)) if f == spec => (),
            Some((_, old)) => {
                return Err(LowerError::BinaryConflictingSpecifier {
                    old: old,
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
            return Err(LowerError::BinaryInvalidSpecifier {
                span: span,
                typ: $typ,
            });
        }
    }};
}

pub fn specifier_from_parsed(parsed: &[BitType]) -> Result<BinaryEntrySpecifier, LowerError> {
    let mut typ = None;
    let mut signed = None;
    let mut endianness = None;
    let mut unit = None;

    for entry in parsed {
        match entry {
            // Types
            BitType::Name(_id, _span, ident) if ident.as_str() == "integer" => {
                try_specifier!(typ, entry, TypeName::Integer)
            }
            BitType::Name(_id, _span, ident) if ident.as_str() == "float" => {
                try_specifier!(typ, entry, TypeName::Float)
            }
            BitType::Name(_id, _span, ident) if ident.as_str() == "binary" => {
                try_specifier!(typ, entry, TypeName::Bytes)
            }
            BitType::Name(_id, _span, ident) if ident.as_str() == "bytes" => {
                try_specifier!(typ, entry, TypeName::Bytes)
            }
            BitType::Name(_id, _span, ident) if ident.as_str() == "bitstring" => {
                try_specifier!(typ, entry, TypeName::Bits)
            }
            BitType::Name(_id, _span, ident) if ident.as_str() == "bits" => {
                try_specifier!(typ, entry, TypeName::Bits)
            }
            BitType::Name(_id, _span, ident) if ident.as_str() == "utf8" => {
                try_specifier!(typ, entry, TypeName::Utf8)
            }
            BitType::Name(_id, _span, ident) if ident.as_str() == "utf16" => {
                try_specifier!(typ, entry, TypeName::Utf16)
            }
            BitType::Name(_id, _span, ident) if ident.as_str() == "utf32" => {
                try_specifier!(typ, entry, TypeName::Utf32)
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
                return Err(LowerError::BinaryUnknownSpecifier { span: entry.span() });
            }
        }
    }

    let typ = typ.map(|(t, _)| t).unwrap_or(TypeName::Integer);

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

            BinaryEntrySpecifier::Utf8
        }
        TypeName::Utf16 => {
            test_none!(signed, typ);
            let endianness = endianness.map(|(t, _)| t).unwrap_or(Endianness::Big);
            test_none!(unit, typ);

            BinaryEntrySpecifier::Utf16 { endianness }
        }
        TypeName::Utf32 => {
            test_none!(signed, typ);
            let endianness = endianness.map(|(t, _)| t).unwrap_or(Endianness::Big);
            test_none!(unit, typ);

            BinaryEntrySpecifier::Utf32 { endianness }
        }
    };

    Ok(spec)
}

fn make_size(
    ctx: &mut LowerCtx,
    b: &mut FunctionBuilder,
    block: &mut IrBlock,
    bit_size: &Option<Expr>,
    spec: BinaryEntrySpecifier,
) -> Option<IrValue> {
    let spec_typ = specifier_to_typename(&spec);

    if let Some(size_expr) = bit_size {
        if !specifier_can_have_size(&spec) {
            ctx.error(LowerError::BinaryInvalidSize {
                span: size_expr.span(),
                typ: spec_typ,
            });
            None
        } else {
            Some(map_block!(
                *block,
                lower_single_same_scope(ctx, b, *block, size_expr)
            ))
        }
    } else {
        match spec_typ {
            TypeName::Integer => Some(b.value(8)),
            TypeName::Float => Some(b.value(64)),
            _ => None,
        }
    }
}

/// Lowers a single entry of a binary construction element.
///
/// A couple of gotchas with the syntax:
/// - `<<"abc">>.`: obviously valid, creates a binary of the ascii characters
/// - `<<"™">>.`: also valid, encodes each CODEPOINT in the string as a byte,
///   truncated. This evaluates to `<<34>>`.
/// - `<<[97, 98, 99]>>.` not valid, while a string binary element is handled
///   as a special case, this does not apply to char lists.
/// - `<<"abc":10/integer>>` valid! a string literal is desugared to a list of
///   individual integers in the binary entry, each with the specifier on it.
pub(crate) fn lower_binary_elem(
    ctx: &mut LowerCtx,
    b: &mut FunctionBuilder,
    mut block: IrBlock,
    bin_ref: &mut IrValue,
    elem: &BinaryElement,
) -> IrBlock {
    let opt_spec = elem.bit_type.as_ref().map(|b| specifier_from_parsed(&*b));

    let mut spec = match opt_spec {
        None => match &elem.bit_expr {
            Expr::Literal(Literal::String(_id, string)) => BinaryEntrySpecifier::Bytes { unit: 1 },
            _ => default_specifier(),
        },
        Some(Ok(inner)) => inner,
        Some(Err(err)) => {
            ctx.error(err);
            return block;
        }
    };

    let size_val = make_size(ctx, b, &mut block, &elem.bit_size, spec);

    let elem_val = match &elem.bit_expr {
        Expr::Literal(Literal::String(_id, string)) if !spec.is_native_endian() => {
            let decoded_spec = match spec {
                BinaryEntrySpecifier::Integer {
                    signed: false,
                    endianness,
                    unit: 8,
                } => Some((SEncoding::Latin1, endianness.try_into().unwrap())),
                BinaryEntrySpecifier::Integer { .. } => None,

                BinaryEntrySpecifier::Utf8 => Some((SEncoding::Utf8, SEndianness::Big)),
                BinaryEntrySpecifier::Utf16 { endianness } => {
                    Some((SEncoding::Utf16, endianness.try_into().unwrap()))
                }
                BinaryEntrySpecifier::Utf32 { endianness } => {
                    Some((SEncoding::Utf32, endianness.try_into().unwrap()))
                }

                _ => {
                    ctx.warn(LowerError::BinaryConstructEntryTypeWarning { span: elem.span });
                    None
                }
            };

            // TODO: Handle more cases statically here in the frontend?

            match decoded_spec {
                Some((str_enc, str_end)) => match string_to_binary(*string, str_enc, str_end) {
                    Ok(bin) => {
                        spec = BinaryEntrySpecifier::Bytes { unit: 8 };
                        Some(b.value(bin))
                    }
                    Err(err) => {
                        ctx.error(err);
                        Some(ctx.sentinel())
                    }
                },
                None => {
                    tokenize_string(*string, &mut |cp, _idx| {
                        let elem_val = b.value(cp);
                        let (ok_cont, err_cont) = BinaryConstructPush::build(
                            b, block, *bin_ref, elem_val, spec, size_val,
                        );
                        *bin_ref = b.block_args(ok_cont)[0];
                        block = ok_cont;

                        // TODO: Proper error
                        b.op_unreachable(elem.span, err_cont);

                        Ok(())
                    });
                    None
                }
            }
        }
        _ => Some(map_block!(
            block,
            lower_single_same_scope(ctx, b, block, &elem.bit_expr)
        )),
    };

    if let Some(elem_val) = elem_val {
        let (ok_cont, err_cont) =
            BinaryConstructPush::build(b, block, *bin_ref, elem_val, spec, size_val);
        *bin_ref = b.block_args(ok_cont)[0];
        block = ok_cont;

        // TODO: Proper error
        b.op_unreachable(elem.span, err_cont);
    }

    block
}

pub(super) fn lower_binary_expr(
    ctx: &mut LowerCtx,
    b: &mut FunctionBuilder,
    mut block: IrBlock,
    bin: Option<IrValue>,
    binary: &Binary,
) -> (IrBlock, IrValue) {
    block = BinaryConstructStart::build(b, block);
    let mut bin_ref = b.block_args(block)[0];

    if let Some(bin) = bin {
        let spec = BinaryEntrySpecifier::Bytes { unit: 1 };
        let (ok_cont, err_cont) = BinaryConstructPush::build(b, block, bin_ref, bin, spec, None);
        block = ok_cont;
        b.op_unreachable(binary.span, err_cont);

        bin_ref = b.block_args(ok_cont)[0];
    }

    for elem in binary.elements.iter() {
        block = lower_binary_elem(ctx, b, block, &mut bin_ref, elem);
    }

    block = BinaryConstructFinish::build(b, block, bin_ref);
    let res_arg = b.block_args(block)[0];

    (block, res_arg)
}
