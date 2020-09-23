use libeir_ir::constant::{AtomTerm, BinaryTerm, Const, ConstantContainer, NilTerm};
use libeir_ir::{Block as IrBlock, FunctionBuilder, Value as IrValue};

use libeir_diagnostics::{SourceIndex, SourceSpan};
use libeir_intern::Ident;

use super::super::{strings::tokenize_string, LowerCtx, LowerError};

use crate::parser::ast::Literal;

pub(super) fn lower_literal(
    ctx: &mut LowerCtx,
    b: &mut FunctionBuilder,
    block: IrBlock,
    literal: &Literal,
) -> (IrBlock, IrValue) {
    let value = match literal {
        Literal::Atom(_id, ident) => b.value(AtomTerm(ident.name)),
        Literal::Integer(_id, _span, int) => b.value(int.clone()),
        Literal::Float(_id, _span, flt) => b.value(*flt),
        Literal::String(_id, ident) => match intern_string_const(*ident, b.cons_mut()) {
            Ok(cons) => b.value(cons),
            Err(err) => {
                ctx.error(err);
                ctx.sentinel()
            }
        },
        Literal::Char(_id, _span, c) => b.value(*c),
    };
    (block, value)
}

pub fn intern_string_const(ident: Ident, c: &mut ConstantContainer) -> Result<Const, LowerError> {
    let mut chars = Vec::new();
    tokenize_string(ident, &mut |cp, _si| {
        chars.push(cp);
        Ok(())
    })?;

    let mut cons = c.from(NilTerm);
    for elem in chars.iter().rev() {
        let val = c.from(*elem);
        cons = c.list_cell(val, cons);
    }

    Ok(c.from(cons))
}
