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
        Literal::Atom(_id, ident) => b.value(AtomTerm(ident.name)),
        Literal::Integer(_id, _span, int) => b.value(int.clone()),
        Literal::Float(_id, _span, flt) => b.value(*flt),
        Literal::String(_id, ident) => {
            match intern_string_const(*ident, b.cons_mut()) {
                Ok(cons) => b.value(cons),
                Err(err) => {
                    ctx.failed = true;
                    ctx.errors.push(err);
                    b.value(NilTerm)
                },
            }
        },
        Literal::Char(_id, _span, c) => b.value(*c),
    };
    (block, value)
}

pub fn tokenize_string(ident: Ident) -> Result<Vec<u64>, LowerError> {
    let string = ident.name.as_str().get();

    // http://erlang.org/doc/reference_manual/data_types.html#escape-sequences

    #[derive(Copy, Clone, Debug)]
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
                *state = StringState::Oct { start, digit_start, num: num + 1 };
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
            StringState::Hex2 { start, digit_start, num } => {
                if !c.is_digit(16) {
                    unimplemented!()
                } else {
                    *state = StringState::Hex2 { start, digit_start, num: num + 1 };
                }
            }
            StringState::HexN { digit_start, .. } => {
                if c == '}' {
                    let parsed = u64::from_str_radix(&full[digit_start..idx], 16).unwrap();
                    out.push(parsed);
                    *state = StringState::Norm;
                }
            }
            StringState::Control { .. } => {
                let cl = c.to_ascii_lowercase();
                if cl >= 'a' && cl <= 'z' {
                    let num = (cl as u64 - 'a' as u64) + 1;
                    out.push(num);
                    *state = StringState::Norm;
                } else {
                    unimplemented!()
                }
            }
        }

        Ok(())
    }

    fn post_process(state: &mut StringState, out: &mut Vec<u64>, full: &str,
                    idx: usize, c: Option<char>) -> Result<(), LowerError> {
        match *state {
            StringState::Oct { digit_start, num, .. } => {
                if let Some(ci) = c {
                    if num == 3 || !ci.is_digit(8) {
                        let parsed = u64::from_str_radix(&full[digit_start..idx], 8).unwrap();
                        out.push(parsed);
                        *state = StringState::Norm;
                    }
                } else {
                    let parsed = u64::from_str_radix(&full[digit_start..idx], 8).unwrap();
                    out.push(parsed);
                    *state = StringState::Norm;
                }
            }
            StringState::Hex2 { digit_start, num, .. } => {
                if num == 2 {
                    let parsed = u64::from_str_radix(&full[digit_start..idx], 16).unwrap();
                    out.push(parsed);
                    *state = StringState::Norm;
                }
            }
            _ => (),
        }

        Ok(())
    }

    let mut chars = Vec::new();
    let mut state = StringState::Norm;
    for (idx, c) in string.char_indices() {
        post_process(&mut state, &mut chars, string, idx, Some(c))?;
        process(&mut state, &mut chars, ident, string, idx, c)?;
    }
    post_process(&mut state, &mut chars, string, string.len(), None)?;

    Ok(chars)
}

pub fn intern_string_const(ident: Ident, c: &mut ConstantContainer) -> Result<Const, LowerError> {
    let chars = tokenize_string(ident)?;

    let mut cons = c.from(NilTerm);
    for elem in chars.iter().rev() {
        let val = c.from(*elem);
        cons = c.list_cell(val, cons);
    }

    Ok(c.from(cons))
}

#[cfg(test)]
mod tests {
    use libeir_intern::Ident;

    use super::tokenize_string;

    #[test]
    fn string_literal_parse() {
        assert!(
            tokenize_string(Ident::from_str("abc")).unwrap() ==
                vec!['a' as u64, 'b' as u64, 'c' as u64]
        );

        assert!(
            tokenize_string(Ident::from_str("a\\bc")).unwrap() ==
                vec!['a' as u64, 8, 'c' as u64]
        );

        assert!(
            tokenize_string(Ident::from_str("a\\b\\d\\e\\f\\n\\r\\s\\t\\vc")).unwrap() ==
                vec!['a' as u64, 8, 127, 27, 12, 10, 13, ' ' as u64, 9, 11, 'c' as u64]
        );

        assert!(
            tokenize_string(Ident::from_str("a\\'\\\"\\\\c")).unwrap() ==
                vec!['a' as u64, '\'' as u64, '"' as u64, '\\' as u64, 'c' as u64]
        );

        assert!(
            tokenize_string(Ident::from_str("a\\1\\12\\123c")).unwrap() ==
                vec!['a' as u64, 0o1, 0o12, 0o123, 'c' as u64]
        );
        assert!(
            tokenize_string(Ident::from_str("\\123")).unwrap() ==
                vec![0o123]
        );
        assert!(
            tokenize_string(Ident::from_str("\\12")).unwrap() ==
                vec![0o12]
        );
        assert!(
            tokenize_string(Ident::from_str("\\1")).unwrap() ==
                vec![0o1]
        );

        assert!(
            tokenize_string(Ident::from_str("a\\xffc")).unwrap() ==
                vec!['a' as u64, 0xff, 'c' as u64]
        );
        assert!(
            tokenize_string(Ident::from_str("\\xff")).unwrap() ==
                vec![0xff]
        );

        assert!(
            tokenize_string(Ident::from_str("\\x{ff}")).unwrap() ==
                vec![0xff]
        );
        assert!(
            tokenize_string(Ident::from_str("\\x{ffff}")).unwrap() ==
                vec![0xffff]
        );

        assert!(
            tokenize_string(Ident::from_str("\\^a\\^z")).unwrap() ==
                vec![1, 26]
        );

    }

}
