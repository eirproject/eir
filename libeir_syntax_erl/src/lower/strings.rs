use super::LowerError;

use snafu::Snafu;

use libeir_diagnostics::{Diagnostic, Label, SourceIndex, SourceSpan, ToDiagnostic};
use libeir_intern::Ident;

use std::convert::TryFrom;
use std::fmt::{Display, Formatter};

pub fn tokenize_string(
    ident: Ident,
    out: &mut impl FnMut(u64, SourceIndex) -> Result<(), LowerError>,
) -> Result<(), LowerError> {
    let string = ident.name.as_str().get();

    // http://erlang.org/doc/reference_manual/data_types.html#escape-sequences

    #[derive(Copy, Clone, Debug)]
    enum StringState {
        Norm,
        Escape {
            start: usize,
        },
        Oct {
            start: usize,
            digit_start: usize,
            num: usize,
        },
        HexStart {
            start: usize,
        },
        HexN {
            start: usize,
            digit_start: usize,
        },
        Hex2 {
            start: usize,
            digit_start: usize,
            num: usize,
        },
        Control {
            start: usize,
        },
    }

    fn process(
        state: &mut StringState,
        out: &mut impl FnMut(u64, SourceIndex) -> Result<(), LowerError>,
        ident: Ident,
        full: &str,
        idx: usize,
        c: char,
    ) -> Result<(), LowerError> {
        let err_until_current = |start: usize| {
            let err_span = SourceSpan::new(ident.span.start() + start, ident.span.start() + idx);
            LowerError::InvalidStringEscape { span: err_span }
        };

        let current = ident.span.start() + idx;

        match *state {
            StringState::Norm => match c {
                '\\' => {
                    *state = StringState::Escape { start: idx };
                }
                _ => {
                    out(c as u64, current)?;
                }
            },
            StringState::Escape { start } => {
                match c {
                    'b' => {
                        // Backspace
                        *state = StringState::Norm;
                        out('\x08' as u64, current)?;
                    }
                    'd' => {
                        // Delete
                        *state = StringState::Norm;
                        out('\x7f' as u64, current)?;
                    }
                    'e' => {
                        // Escape
                        *state = StringState::Norm;
                        out('\x1b' as u64, current)?;
                    }
                    'f' => {
                        // Form feed
                        *state = StringState::Norm;
                        out('\x0c' as u64, current)?;
                    }
                    'n' => {
                        // Line feed
                        *state = StringState::Norm;
                        out('\n' as u64, current)?;
                    }
                    'r' => {
                        // Carriage return
                        *state = StringState::Norm;
                        out('\r' as u64, current)?;
                    }
                    's' => {
                        // Space
                        *state = StringState::Norm;
                        out(' ' as u64, current)?;
                    }
                    't' => {
                        // Tab
                        *state = StringState::Norm;
                        out('\t' as u64, current)?;
                    }
                    'v' => {
                        // Vertical tab
                        *state = StringState::Norm;
                        out('\x0b' as u64, current)?;
                    }
                    n if n >= '0' && n <= '7' => {
                        *state = StringState::Oct {
                            start,
                            digit_start: idx,
                            num: 1,
                        };
                    }
                    'x' => {
                        *state = StringState::HexStart { start };
                    }
                    '^' => {
                        *state = StringState::Control { start };
                    }
                    '\'' => {
                        *state = StringState::Norm;
                        out('\'' as u64, current)?;
                    }
                    '"' => {
                        *state = StringState::Norm;
                        out('"' as u64, current)?;
                    }
                    '\\' => {
                        *state = StringState::Norm;
                        out('\\' as u64, current)?;
                    }
                    '$' => {
                        *state = StringState::Norm;
                        out('$' as u64, current)?;
                    }
                    _ => {
                        return Err(err_until_current(start));
                    }
                }
            }
            StringState::Oct {
                start,
                digit_start,
                num,
            } => {
                *state = StringState::Oct {
                    start,
                    digit_start,
                    num: num + 1,
                };
            }
            StringState::HexStart { start } => match c {
                '{' => {
                    *state = StringState::HexN {
                        start,
                        digit_start: idx + 1,
                    };
                }
                n if n.is_digit(16) => {
                    *state = StringState::Hex2 {
                        start,
                        digit_start: idx,
                        num: 1,
                    };
                }
                _ => {
                    return Err(err_until_current(start));
                }
            },
            StringState::Hex2 {
                start,
                digit_start,
                num,
            } => {
                if !c.is_digit(16) {
                    unimplemented!()
                } else {
                    *state = StringState::Hex2 {
                        start,
                        digit_start,
                        num: num + 1,
                    };
                }
            }
            StringState::HexN { digit_start, .. } => {
                if c == '}' {
                    let parsed = u64::from_str_radix(&full[digit_start..idx], 16).unwrap();
                    out(parsed, current)?;
                    *state = StringState::Norm;
                }
            }
            StringState::Control { .. } => {
                let cl = c.to_ascii_lowercase();
                if cl >= 'a' && cl <= 'z' {
                    let num = (cl as u64 - 'a' as u64) + 1;
                    out(num, current)?;
                    *state = StringState::Norm;
                } else {
                    unimplemented!()
                }
            }
        }

        Ok(())
    }

    fn post_process(
        state: &mut StringState,
        out: &mut impl FnMut(u64, SourceIndex) -> Result<(), LowerError>,
        ident: Ident,
        full: &str,
        idx: usize,
        c: Option<char>,
    ) -> Result<(), LowerError> {
        let current = ident.span.start() + idx;
        match *state {
            StringState::Oct {
                digit_start, num, ..
            } => {
                if let Some(ci) = c {
                    if num == 3 || !ci.is_digit(8) {
                        let parsed = u64::from_str_radix(&full[digit_start..idx], 8).unwrap();
                        out(parsed, current)?;
                        *state = StringState::Norm;
                    }
                } else {
                    let parsed = u64::from_str_radix(&full[digit_start..idx], 8).unwrap();
                    out(parsed, current)?;
                    *state = StringState::Norm;
                }
            }
            StringState::Hex2 {
                digit_start, num, ..
            } => {
                if num == 2 {
                    let parsed = u64::from_str_radix(&full[digit_start..idx], 16).unwrap();
                    out(parsed, current)?;
                    *state = StringState::Norm;
                }
            }
            _ => (),
        }

        Ok(())
    }

    let mut state = StringState::Norm;
    for (idx, c) in string.char_indices() {
        post_process(&mut state, out, ident, string, idx, Some(c))?;
        process(&mut state, out, ident, string, idx, c)?;
    }
    post_process(&mut state, out, ident, string, string.len(), None)?;

    Ok(())
}

pub fn tokenize_string_to_vec(string: Ident) -> Result<Vec<u64>, LowerError> {
    let mut chars = Vec::new();
    tokenize_string(string, &mut |cp, _idx| {
        chars.push(cp);
        Ok(())
    })?;
    Ok(chars)
}

#[derive(Debug, Snafu)]
pub enum StringError {
    #[snafu(display("unicode codepoint #{} is not encodable in {}", codepoint, encoding))]
    CodepointEncoding {
        idx: SourceIndex,
        codepoint: u64,
        encoding: crate::lower::strings::Encoding,
    },
}

impl ToDiagnostic for StringError {
    fn to_diagnostic(&self) -> Diagnostic {
        let msg = self.to_string();
        match self {
            StringError::CodepointEncoding {
                idx,
                codepoint,
                encoding,
            } => Diagnostic::error()
                .with_message(msg)
                .with_labels(vec![Label::primary(
                    idx.source_id(),
                    SourceSpan::new(*idx, *idx),
                )
                .with_message("encoding failed at codepoint")]),
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Encoding {
    /// The default encoding for binary string literals.
    /// In practice this is the unicode codepoint modulo 2^8 (truncated to 8 bits).
    Latin1,
    Utf8,
    Utf16,
    Utf32,
}

impl Display for Encoding {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Encoding::Latin1 => write!(f, "latin1"),
            Encoding::Utf8 => write!(f, "utf8"),
            Encoding::Utf16 => write!(f, "utf16"),
            Encoding::Utf32 => write!(f, "utf32"),
        }
    }
}

impl Encoding {
    fn encode(&self, cp: u64, idx: SourceIndex) -> Result<Encoded, StringError> {
        match self {
            Encoding::Latin1 => encode_latin1(cp, idx),
            Encoding::Utf8 => encode_utf8(cp, idx),
            Encoding::Utf16 => encode_utf16(cp, idx),
            Encoding::Utf32 => encode_utf32(cp, idx),
        }
    }
}

#[derive(Debug, Copy, Clone)]
pub enum Endianness {
    Big,
    Little,
}

use libeir_ir::binary::Endianness as EirEndianness;
impl TryFrom<EirEndianness> for Endianness {
    type Error = ();
    fn try_from(end: EirEndianness) -> Result<Endianness, ()> {
        match end {
            EirEndianness::Big => Ok(Endianness::Big),
            EirEndianness::Little => Ok(Endianness::Little),
            EirEndianness::Native => Err(()),
        }
    }
}

#[derive(Copy, Clone)]
enum Encoded {
    N1(u8),
    N2(u8, u8),
    N3(u8, u8, u8),
    N4(u8, u8, u8, u8),
}
impl Encoded {
    pub fn write(self, endianness: Endianness, out: &mut Vec<u8>) {
        match endianness {
            Endianness::Big => self.write_be(out),
            Endianness::Little => self.write_le(out),
        }
    }

    pub fn write_le(self, out: &mut Vec<u8>) {
        match self {
            Encoded::N1(a) => out.push(a),
            Encoded::N2(a, b) => {
                out.push(b);
                out.push(a);
            }
            Encoded::N3(a, b, c) => {
                out.push(c);
                out.push(b);
                out.push(a);
            }
            Encoded::N4(a, b, c, d) => {
                out.push(d);
                out.push(c);
                out.push(b);
                out.push(a);
            }
        }
    }

    pub fn write_be(self, out: &mut Vec<u8>) {
        match self {
            Encoded::N1(a) => out.push(a),
            Encoded::N2(a, b) => {
                out.push(a);
                out.push(b);
            }
            Encoded::N3(a, b, c) => {
                out.push(a);
                out.push(b);
                out.push(c);
            }
            Encoded::N4(a, b, c, d) => {
                out.push(a);
                out.push(b);
                out.push(c);
                out.push(d);
            }
        }
    }
}

fn encode_utf8(cp: u64, idx: SourceIndex) -> Result<Encoded, StringError> {
    match cp {
        0x00..=0x7f => Ok(Encoded::N1(cp as u8)),
        0x80..=0x7ff => Ok(Encoded::N2(
            0b110_00000 | (cp >> 6 & 0b000_11111) as u8,
            0b10_000000 | (cp >> 0 & 0b00_111111) as u8,
        )),
        0x800..=0xffff => Ok(Encoded::N3(
            0b1110_0000 | (cp >> 12 & 0b0000_1111) as u8,
            0b10_000000 | (cp >> 6 & 0b00_111111) as u8,
            0b10_000000 | (cp >> 0 & 0b00_111111) as u8,
        )),
        0x10000..=0x1fffff => Ok(Encoded::N4(
            0b11110_000 | (cp >> 18 & 0b00000_111) as u8,
            0b10_000000 | (cp >> 12 & 0b00_111111) as u8,
            0b10_000000 | (cp >> 6 & 0b00_111111) as u8,
            0b10_000000 | (cp >> 0 & 0b00_111111) as u8,
        )),
        _ => Err(StringError::CodepointEncoding {
            idx,
            codepoint: cp,
            encoding: Encoding::Utf8,
        }),
    }
}

fn encode_utf16(cp: u64, idx: SourceIndex) -> Result<Encoded, StringError> {
    match cp {
        0x0000..=0xd7ff => Ok(Encoded::N2((cp >> 8) as u8, (cp >> 0) as u8)),
        0xd800..=0xdfff => Err(StringError::CodepointEncoding {
            idx,
            codepoint: cp,
            encoding: Encoding::Utf16,
        }),
        0xd800..=0xffff => Ok(Encoded::N2((cp >> 8) as u8, (cp >> 0) as u8)),
        0x10000..=0x10ffff => {
            let val = cp - 0x10000;
            Ok(Encoded::N4(
                0b110110_00 | (cp >> 18 & 0b000000_11) as u8,
                (cp >> 10) as u8,
                0b110111_00 | (cp >> 8 & 0b000000_11) as u8,
                (cp >> 0) as u8,
            ))
        }
        _ => Err(StringError::CodepointEncoding {
            idx,
            codepoint: cp,
            encoding: Encoding::Utf16,
        }),
    }
}

fn encode_utf32(cp: u64, idx: SourceIndex) -> Result<Encoded, StringError> {
    if cp > std::u32::MAX as u64 {
        Err(StringError::CodepointEncoding {
            idx,
            codepoint: cp,
            encoding: Encoding::Utf32,
        })
    } else {
        Ok(Encoded::N4(
            (cp >> 24) as u8,
            (cp >> 16) as u8,
            (cp >> 8) as u8,
            (cp >> 0) as u8,
        ))
    }
}

fn encode_latin1(cp: u64, _idx: SourceIndex) -> Result<Encoded, StringError> {
    Ok(Encoded::N1(cp as u8))
}

pub fn string_to_binary(
    ident: Ident,
    encoding: Encoding,
    endianness: Endianness,
) -> Result<Vec<u8>, LowerError> {
    let mut out = Vec::new();
    tokenize_string(ident, &mut |cp, si| {
        let encoded = encoding.encode(cp, si)?;
        encoded.write(endianness, &mut out);
        Ok(())
    })?;
    Ok(out)
}

#[cfg(test)]
mod tests {
    use libeir_intern::Ident;

    use super::tokenize_string_to_vec;

    #[test]
    fn string_literal_parse() {
        assert!(
            tokenize_string_to_vec(Ident::from_str("abc")).unwrap()
                == vec!['a' as u64, 'b' as u64, 'c' as u64]
        );

        assert!(
            tokenize_string_to_vec(Ident::from_str("a\\bc")).unwrap()
                == vec!['a' as u64, 8, 'c' as u64]
        );

        assert!(
            tokenize_string_to_vec(Ident::from_str("a\\b\\d\\e\\f\\n\\r\\s\\t\\vc")).unwrap()
                == vec!['a' as u64, 8, 127, 27, 12, 10, 13, ' ' as u64, 9, 11, 'c' as u64]
        );

        assert!(
            tokenize_string_to_vec(Ident::from_str("a\\'\\\"\\\\c")).unwrap()
                == vec!['a' as u64, '\'' as u64, '"' as u64, '\\' as u64, 'c' as u64]
        );

        assert!(
            tokenize_string_to_vec(Ident::from_str("a\\1\\12\\123c")).unwrap()
                == vec!['a' as u64, 0o1, 0o12, 0o123, 'c' as u64]
        );
        assert!(tokenize_string_to_vec(Ident::from_str("\\123")).unwrap() == vec![0o123]);
        assert!(tokenize_string_to_vec(Ident::from_str("\\12")).unwrap() == vec![0o12]);
        assert!(tokenize_string_to_vec(Ident::from_str("\\1")).unwrap() == vec![0o1]);

        assert!(
            tokenize_string_to_vec(Ident::from_str("a\\xffc")).unwrap()
                == vec!['a' as u64, 0xff, 'c' as u64]
        );
        assert!(tokenize_string_to_vec(Ident::from_str("\\xff")).unwrap() == vec![0xff]);

        assert!(tokenize_string_to_vec(Ident::from_str("\\x{ff}")).unwrap() == vec![0xff]);
        assert!(tokenize_string_to_vec(Ident::from_str("\\x{ffff}")).unwrap() == vec![0xffff]);

        assert!(tokenize_string_to_vec(Ident::from_str("\\^a\\^z")).unwrap() == vec![1, 26]);
    }
}
