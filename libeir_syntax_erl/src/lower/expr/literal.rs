use libeir_ir::{
    FunctionBuilder,
    Value as IrValue,
    Block as IrBlock,
};
use libeir_ir::constant::{ ConstantContainer, AtomTerm, NilTerm, Const };

use libeir_intern::Ident;
use libeir_diagnostics::{ ByteSpan, ByteIndex, DUMMY_SPAN };

use super::super::{ LowerCtx, LowerError };

use crate::parser::ast::Literal;

pub(super) fn lower_literal(ctx: &mut LowerCtx, b: &mut FunctionBuilder, block: IrBlock,
                 literal: &Literal) -> (IrBlock, IrValue)
{
    let value = match literal {
        Literal::Atom(ident) => b.value((AtomTerm(ident.name), ident.span)),
        Literal::Integer(span, int) => b.value((*int, *span)),
        Literal::String(ident) => {
            match intern_string_const(*ident, b.cons_mut()) {
                Ok(cons) => b.value(cons),
                Err(err) => {
                    ctx.failed = true;
                    ctx.errors.push(err);
                    b.value(NilTerm)
                },
            }
        },
        Literal::Char(span, c) => b.value((*c, *span)),
        _ => unimplemented!("{:?}", literal),
    };
    (block, value)
}

pub fn tokenize_string(ident: Ident) -> Result<Vec<u64>, LowerError> {
    let string = ident.name.as_str().get();

    // http://erlang.org/doc/reference_manual/data_types.html#escape-sequences

    #[derive(Copy, Clone)]
    enum StringState {
        Norm,
        Escape { start: usize },
        Oct { start: usize, digit_start: usize, num: usize },
        HexStart { start: usize },
        HexN { start: usize, digit_start: usize },
        Hex2 { start: usize, digit_start: usize, num: usize },
        Control { start: usize },
    }

    fn process(state: &mut StringState, out: &mut Vec<u64>, ident: Ident, full: &str,
               idx: usize, c: char) -> Result<(), LowerError> {

        let err_until_current = |start: usize| {
            let err_span = if ident.span == DUMMY_SPAN {
                DUMMY_SPAN
            } else {
                ByteSpan::new(
                    ByteIndex(ident.span.start().0 + start as u32),
                    ByteIndex(ident.span.start().0 + idx as u32),
                )
            };
            LowerError::InvalidStringEscape { span: err_span }
        };

        match *state {
            StringState::Norm => {
                match c {
                    '\\' => {
                        *state = StringState::Escape { start: idx };
                    },
                    _ => {
                        out.push(c as u64);
                    },
                }
            }
            StringState::Escape { start } => {
                match c {
                    'b' => { // Backspace
                        *state = StringState::Norm;
                        out.push('\x08' as u64);
                    }
                    'd' => { // Delete
                        *state = StringState::Norm;
                        out.push('\x7f' as u64);
                    }
                    'e' => { // Escape
                        *state = StringState::Norm;
                        out.push('\x1b' as u64);
                    }
                    'f' => { // Form feed
                        *state = StringState::Norm;
                        out.push('\x0c' as u64);
                    }
                    'n' => { // Line feed
                        *state = StringState::Norm;
                        out.push('\n' as u64);
                    }
                    'r' => { // Carriage return
                        *state = StringState::Norm;
                        out.push('\r' as u64);
                    }
                    's' => { // Space
                        *state = StringState::Norm;
                        out.push(' ' as u64);
                    }
                    't' => { // Tab
                        *state = StringState::Norm;
                        out.push('\t' as u64);
                    }
                    'v' => { // Vertical tab
                        *state = StringState::Norm;
                        out.push('\x0b' as u64);
                    }
                    n if n >= '0' && n <= '7' => {
                        *state = StringState::Oct { start, digit_start: idx, num: 1 };
                    }
                    'x' => {
                        *state = StringState::HexStart { start };
                    }
                    '^' => {
                        *state = StringState::Control { start };
                    }
                    '\'' => {
                        *state = StringState::Norm;
                        out.push('\'' as u64);
                    }
                    '"' => {
                        *state = StringState::Norm;
                        out.push('"' as u64);
                    }
                    '\\' => {
                        *state = StringState::Norm;
                        out.push('\\' as u64);
                    }
                    _ => {
                        return Err(err_until_current(start));
                    }
                }
            }
            StringState::Oct { start, digit_start, num } => {
                let curr_valid = c.is_digit(8);

                if num == 3 || !curr_valid {
                    let parsed = u64::from_str_radix(&full[digit_start..idx], 8).unwrap();
                    out.push(parsed);
                    *state = StringState::Norm;
                    process(state, out, ident, full, idx, c)?;
                } else if num == 0 {
                    unreachable!()
                } else {
                    *state = StringState::Oct { start, digit_start, num: num + 1 };
                }

            }
            StringState::HexStart { start } => {
                match c {
                    '{' => {
                        *state = StringState::HexN { start, digit_start: idx + 1 };
                    }
                    n if n.is_digit(16) => {
                        *state = StringState::Hex2 { start, digit_start: idx, num: 1 };
                    }
                    _ => {
                        return Err(err_until_current(start));
                    }
                }
            }
            _ => unimplemented!(),
        }

        Ok(())
    }

    let mut chars = Vec::new();
    let mut state = StringState::Norm;
    for (idx, c) in string.char_indices() {
        process(&mut state, &mut chars, ident, string, idx, c)?;
    }

    Ok(chars)
}

pub fn intern_string_const(ident: Ident, c: &mut ConstantContainer) -> Result<Const, LowerError> {
    let chars = tokenize_string(ident)?;

    let mut cons = c.value_from(NilTerm);
    for elem in chars.iter().rev() {
        let val = c.value_from(*elem);
        cons = c.value_list_cell(val, cons);
    }

    Ok(c.from((cons, ident.span)))
}
