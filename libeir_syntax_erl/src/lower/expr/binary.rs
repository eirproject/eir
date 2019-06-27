use libeir_ir::{
    FunctionBuilder,
    Value as IrValue,
    Block as IrBlock,
};
use libeir_ir::constant::NilTerm;
use libeir_ir::op::BinOp;

use libeir_intern::{ Ident, Symbol };
use libeir_diagnostics::DUMMY_SPAN;

use crate::parser::ast::{ Binary, BitType };

use crate::lower::{ LowerCtx, LowerError };
use crate::lower::expr::{ lower_single, lower_block };
use crate::lower::pattern::lower_clause;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
enum Endianness {
    Big,
    Little,
    Native,
}

struct EntrySpecifier {
    typ: TypeName,
    signed: Option<bool>,
    endianness: Option<Endianness>,
    unit: Option<i64>,
}

impl Default for EntrySpecifier {
    fn default() -> Self {
        EntrySpecifier {
            typ: TypeName::Integer,
            signed: Some(false),
            endianness: Some(Endianness::Big),
            unit: Some(1),
        }
    }
}

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

impl TypeName {
    fn can_have_size(&self) -> bool {
        match self {
            TypeName::Utf8 => false,
            TypeName::Utf16 => false,
            TypeName::Utf32 => false,
            _ => true,
        }
    }
}

macro_rules! try_specifier {
    ($field:expr, $entry:expr, $spec:expr) => {
        {
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
        }
    }
}

macro_rules! test_none {
    ($field:expr, $typ:expr) => {
        {
            if let Some((_, span)) = $field {
                return Err(LowerError::BinaryInvalidSpecifier {
                    span: span,
                    typ: $typ,
                });
            }
        }
    }
}

impl EntrySpecifier {

    fn from_parsed(parsed: &[BitType]) -> Result<Self, LowerError> {
        let mut typ = None;
        let mut signed = None;
        let mut endianness = None;
        let mut unit = None;

        for entry in parsed {
            match entry {
                // Types
                BitType::Name(_span, ident) if ident.as_str() == "integer" =>
                    try_specifier!(typ, entry, TypeName::Integer),
                BitType::Name(_span, ident) if ident.as_str() == "float" =>
                    try_specifier!(typ, entry, TypeName::Float),
                BitType::Name(_span, ident) if ident.as_str() == "binary" =>
                    try_specifier!(typ, entry, TypeName::Bytes),
                BitType::Name(_span, ident) if ident.as_str() == "bytes" =>
                    try_specifier!(typ, entry, TypeName::Bytes),
                BitType::Name(_span, ident) if ident.as_str() == "bitstring" =>
                    try_specifier!(typ, entry, TypeName::Bits),
                BitType::Name(_span, ident) if ident.as_str() == "bits" =>
                    try_specifier!(typ, entry, TypeName::Bits),
                BitType::Name(_span, ident) if ident.as_str() == "utf8" =>
                    try_specifier!(typ, entry, TypeName::Utf8),
                BitType::Name(_span, ident) if ident.as_str() == "utf16" =>
                    try_specifier!(typ, entry, TypeName::Utf16),
                BitType::Name(_span, ident) if ident.as_str() == "utf32" =>
                    try_specifier!(typ, entry, TypeName::Utf32),

                // Signed
                BitType::Name(_span, ident) if ident.as_str() == "signed" =>
                    try_specifier!(signed, entry, true),
                BitType::Name(_span, ident) if ident.as_str() == "unsigned" =>
                    try_specifier!(signed, entry, false),

                // Endianness
                BitType::Name(_span, ident) if ident.as_str() == "big" =>
                    try_specifier!(endianness, entry, Endianness::Big),
                BitType::Name(_span, ident) if ident.as_str() == "little" =>
                    try_specifier!(endianness, entry, Endianness::Little),
                BitType::Name(_span, ident) if ident.as_str() == "native" =>
                    try_specifier!(endianness, entry, Endianness::Native),

                // Unit
                BitType::Sized(_span, ident, num) if ident.as_str() == "unit" =>
                    try_specifier!(unit, entry, *num),

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

                EntrySpecifier {
                    typ: TypeName::Integer,
                    signed: Some(signed),
                    endianness: Some(endianness),
                    unit: Some(unit),
                }
            }
            TypeName::Float => {
                // Default is big-unit:1
                test_none!(signed, typ);
                let endianness = endianness.map(|(t, _)| t).unwrap_or(Endianness::Big);
                let unit = unit.map(|(t, _)| t).unwrap_or(1);

                EntrySpecifier {
                    typ: TypeName::Float,
                    signed: None,
                    endianness: Some(endianness),
                    unit: Some(unit),
                }
            }
            TypeName::Bytes => {
                // Default is unit:8
                test_none!(signed, typ);
                test_none!(endianness, typ);
                let unit = unit.map(|(t, _)| t).unwrap_or(8);

                EntrySpecifier {
                    typ: TypeName::Bytes,
                    signed: None,
                    endianness: None,
                    unit: Some(unit),
                }
            }
            TypeName::Bits => {
                // Default is unit:1
                test_none!(signed, typ);
                test_none!(endianness, typ);
                let unit = unit.map(|(t, _)| t).unwrap_or(1);

                EntrySpecifier {
                    typ: TypeName::Bits,
                    signed: None,
                    endianness: None,
                    unit: Some(unit),
                }
            }
            TypeName::Utf8 => {
                test_none!(signed, typ);
                test_none!(endianness, typ);
                test_none!(unit, typ);

                EntrySpecifier {
                    typ: TypeName::Utf8,
                    signed: None,
                    endianness: None,
                    unit: None,
                }
            }
            TypeName::Utf16 => {
                test_none!(signed, typ);
                let endianness = endianness.map(|(t, _)| t).unwrap_or(Endianness::Big);
                test_none!(unit, typ);

                EntrySpecifier {
                    typ: TypeName::Utf16,
                    signed: None,
                    endianness: Some(endianness),
                    unit: None,
                }
            }
            TypeName::Utf32 => {
                test_none!(signed, typ);
                let endianness = endianness.map(|(t, _)| t).unwrap_or(Endianness::Big);
                test_none!(unit, typ);

                EntrySpecifier {
                    typ: TypeName::Utf32,
                    signed: None,
                    endianness: Some(endianness),
                    unit: None,
                }
            }
        };

        Ok(spec)
    }

}

pub(super) fn lower_binary_expr(ctx: &mut LowerCtx, b: &mut FunctionBuilder, mut block: IrBlock,
                                binary: &Binary) -> (IrBlock, IrValue)
{
    // TODO: Revisit what we do here.
    // We should probably lower all values first, then construct
    // the binary in an atomic sequence of operations.

    // Binary start construct intrinsic
    let next = b.block_insert();
    let constr_ctx = b.block_arg_insert(next);
    let mut ib = b.op_intrinsic_build(Symbol::intern("binary_construct_start"));
    ib.push_value(next, b);
    ib.block = Some(block);
    ib.finish(b);
    block = next;

    for elem in binary.elements.iter() {
        let spec = elem.bit_type.as_ref()
            .map(|b| EntrySpecifier::from_parsed(&*b))
            .unwrap_or(Ok(EntrySpecifier::default()));

        let spec = match spec {
            Ok(inner) => inner,
            Err(err) => {
                ctx.error(err);
                continue;
            },
        };

        let bit_val = map_block!(block, lower_single(ctx, b, block, &elem.bit_expr));

        let size_val = if let Some(size_expr) = &elem.bit_size {
            if !spec.typ.can_have_size() {
                ctx.error(LowerError::BinaryInvalidSize {
                    span: size_expr.span(),
                    typ: spec.typ,
                });
            }
            map_block!(block, lower_single(ctx, b, block, size_expr))
        } else {
            match spec.typ {
                TypeName::Integer => b.value(8),
                TypeName::Float => b.value(64),
                _ => b.value(NilTerm),
            }
        };

        let next = b.block_insert();
        let mut ib = b.op_intrinsic_build(Symbol::intern("binary_construct_push"));
        ib.push_value(next, b);
        ib.push_value(constr_ctx, b);
        ib.push_value(bit_val, b);
        ib.push_value(size_val, b);

        let typ = match spec.typ {
            TypeName::Integer => b.value(Symbol::intern("integer")),
            TypeName::Float => b.value(Symbol::intern("float")),
            TypeName::Bytes => b.value(Symbol::intern("bytes")),
            TypeName::Bits => b.value(Symbol::intern("bits")),
            TypeName::Utf8 => b.value(Symbol::intern("utf8")),
            TypeName::Utf16 => b.value(Symbol::intern("utf16")),
            TypeName::Utf32 => b.value(Symbol::intern("utf32")),
        };
        ib.push_value(typ, b);

        let signed = match spec.signed {
            None => b.value(NilTerm),
            Some(true) => b.value(Symbol::intern("signed")),
            Some(false) => b.value(Symbol::intern("unsigned")),
        };
        ib.push_value(signed, b);

        let endianness = match spec.endianness {
            None => b.value(NilTerm),
            Some(Endianness::Big) => b.value(Symbol::intern("big")),
            Some(Endianness::Little) => b.value(Symbol::intern("little")),
            Some(Endianness::Native) => b.value(Symbol::intern("native")),
        };
        ib.push_value(endianness, b);

        let unit = match spec.unit {
            None => b.value(NilTerm),
            Some(unit) => b.value(unit),
        };
        ib.push_value(unit, b);

        ib.block = Some(block);
        ib.finish(b);
        block = next;

    }

    let next = b.block_insert();
    let res_arg = b.block_arg_insert(next);
    let mut ib = b.op_intrinsic_build(Symbol::intern("binary_construct_finish"));
    ib.push_value(next, b);
    ib.push_value(constr_ctx, b);
    ib.block = Some(block);
    ib.finish(b);
    block = next;

    (block, res_arg)
}
