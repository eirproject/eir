use super::LowerError;

use snafu::Snafu;

use libeir_diagnostics::{Diagnostic, Label, SourceIndex, SourceSpan, ToDiagnostic};
use libeir_intern::Ident;

use std::convert::TryFrom;
use std::fmt::{Display, Formatter};

pub mod escape {
    use libeir_diagnostics::{Diagnostic, Label, SourceIndex, SourceSpan, ToDiagnostic};
    use snafu::Snafu;

    #[derive(Snafu, Debug, Clone, PartialEq, Eq)]
    pub enum EscapeStmError<D> {
        /// Unknown escape character
        #[snafu(display("unknown escape character '{}'", escape_char))]
        UnknownEscape { range: (D, D), escape_char: char },
        /// Expected a base-n digit or alt
        #[snafu(display("expected base{} digit or '{}', found '{}'", base, alt, found))]
        InvalidBaseNDigitOrAlt {
            range: (D, D),
            found: char,
            base: usize,
            alt: char,
        },
        /// Expected a base-n digit
        #[snafu(display("expected base{} digit, found '{}'", base, found))]
        InvalidBaseNDigit {
            range: (D, D),
            found: char,
            base: usize,
        },
        /// Expected control character symbol
        #[snafu(display("expected control character symbol (a-z), found '{}'", found))]
        InvalidControl { range: (D, D), found: char },
        #[snafu(display("unexpected EOF"))]
        UnexpectedEof { range: (D, D) },
    }

    impl<D> EscapeStmError<D> {
        pub fn range(&self) -> &(D, D) {
            match self {
                Self::UnknownEscape { range, .. } => range,
                Self::InvalidBaseNDigitOrAlt { range, .. } => range,
                Self::InvalidBaseNDigit { range, .. } => range,
                Self::InvalidControl { range, .. } => range,
                Self::UnexpectedEof { range } => range,
            }
        }
    }

    impl EscapeStmError<SourceIndex> {
        pub fn span(&self) -> SourceSpan {
            let (start, end) = self.range();
            SourceSpan::new(*start, *end)
        }
    }

    impl ToDiagnostic for EscapeStmError<SourceIndex> {
        fn to_diagnostic(&self) -> Diagnostic {
            let msg = self.to_string();
            let span = self.span();

            Diagnostic::error()
                .with_message("invalid string escape")
                .with_labels(vec![
                    Label::primary(span.source_id(), span).with_message(msg)
                ])
        }
    }

    /// Erlang string escape state machine.
    #[derive(Clone, Debug)]
    pub struct EscapeStm<D> {
        buf: String,
        curr_start: Option<D>,
        state: EscapeStmState,
    }

    #[derive(Copy, Clone, Debug, PartialEq, Eq)]
    pub enum EscapeStmState {
        Norm,
        Escape,
        Oct,
        HexStart,
        HexN,
        Hex2,
        Control,
    }

    #[derive(Debug)]
    pub enum EscapeStmAction {
        Next,
        Again,
    }

    #[derive(Debug)]
    pub struct EscapeStmOut<D> {
        pub range: (D, D),
        pub cp: u64,
    }

    impl<D: Copy> EscapeStm<D> {
        pub fn new() -> Self {
            EscapeStm {
                buf: String::new(),
                curr_start: None,
                state: EscapeStmState::Norm,
            }
        }

        pub fn reset(&mut self) {
            self.buf.clear();
            self.curr_start = None;
            self.state = EscapeStmState::Norm;
        }

        pub fn is_norm(&self) -> bool {
            self.state == EscapeStmState::Norm
        }

        pub fn transition(
            &mut self,
            c: Option<char>,
            pos: D,
        ) -> Result<(EscapeStmAction, Option<EscapeStmOut<D>>), EscapeStmError<D>> {
            use EscapeStmAction as A;
            use EscapeStmState as S;

            let mut range = (self.curr_start.unwrap_or(pos), pos);
            let mut out = None;

            let action = match self.state {
                S::Norm => match c {
                    Some('\\') => {
                        self.state = S::Escape;
                        self.curr_start = Some(pos);
                        A::Next
                    }
                    Some(c) => {
                        self.state = S::Norm;
                        range = (pos, pos);
                        out = Some(c as u64);
                        A::Next
                    }
                    None => A::Next,
                },
                S::Escape => {
                    match c {
                        Some('b') => {
                            // Backspace
                            self.state = S::Norm;
                            out = Some('\x08' as u64);
                        }
                        Some('d') => {
                            // Delete
                            self.state = S::Norm;
                            out = Some('\x7f' as u64);
                        }
                        Some('e') => {
                            // Escape
                            self.state = S::Norm;
                            out = Some('\x1b' as u64);
                        }
                        Some('f') => {
                            // Form feed
                            self.state = S::Norm;
                            out = Some('\x0c' as u64);
                        }
                        Some('n') => {
                            // Line feed
                            self.state = S::Norm;
                            out = Some('\n' as u64);
                        }
                        Some('r') => {
                            // Carriage return
                            self.state = S::Norm;
                            out = Some('\r' as u64);
                        }
                        Some('s') => {
                            // Space
                            self.state = S::Norm;
                            out = Some(' ' as u64);
                        }
                        Some('t') => {
                            // Tab
                            self.state = S::Norm;
                            out = Some('\t' as u64);
                        }
                        Some('v') => {
                            // Vertical tab
                            self.state = S::Norm;
                            out = Some('\x0b' as u64);
                        }
                        Some(n) if n >= '0' && n <= '7' => {
                            self.buf.clear();
                            self.buf.push(n);
                            self.state = S::Oct;
                        }
                        Some('x') => {
                            self.state = S::HexStart;
                        }
                        Some('^') => {
                            self.state = S::Control;
                        }
                        Some(c) => {
                            self.state = S::Norm;
                            out = Some(c as u64);
                        }
                        None => return Err(EscapeStmError::UnexpectedEof { range }),
                    }
                    A::Next
                }
                S::Oct => match c {
                    Some(c) if c >= '0' && c <= '7' => {
                        self.buf.push(c);

                        if self.buf.len() == 3 {
                            let parsed = u64::from_str_radix(&self.buf, 8).unwrap();

                            self.state = S::Norm;
                            out = Some(parsed);
                        } else {
                            self.state = S::Oct;
                        }

                        A::Next
                    }
                    _ => {
                        let parsed = u64::from_str_radix(&self.buf, 8).unwrap();

                        self.state = S::Norm;
                        out = Some(parsed);

                        A::Again
                    }
                },
                S::HexStart => match c {
                    Some('{') => {
                        self.state = S::HexN;

                        A::Next
                    }
                    Some(n) if n.is_digit(16) => {
                        self.buf.clear();
                        self.buf.push(n);
                        self.state = S::Hex2;

                        A::Next
                    }
                    Some(c) => {
                        return Err(EscapeStmError::InvalidBaseNDigitOrAlt {
                            range,
                            found: c,
                            base: 16,
                            alt: '{',
                        });
                    }
                    None => return Err(EscapeStmError::UnexpectedEof { range }),
                },
                S::Hex2 => match c {
                    Some(n) if n.is_digit(16) => {
                        self.buf.push(n);
                        self.state = S::Norm;

                        let parsed = u64::from_str_radix(&self.buf, 16).unwrap();
                        out = Some(parsed);

                        A::Next
                    }
                    Some(c) => {
                        return Err(EscapeStmError::InvalidBaseNDigit {
                            range,
                            found: c,
                            base: 16,
                        });
                    }
                    None => return Err(EscapeStmError::UnexpectedEof { range }),
                },
                S::HexN => match c {
                    Some('}') => {
                        let parsed = u64::from_str_radix(&self.buf, 16).unwrap();

                        self.state = S::Norm;
                        out = Some(parsed);

                        A::Next
                    }
                    Some(n) if n.is_digit(16) => {
                        self.buf.push(n);
                        self.state = S::HexN;

                        A::Next
                    }
                    Some(c) => {
                        return Err(EscapeStmError::InvalidBaseNDigitOrAlt {
                            range,
                            found: c,
                            base: 16,
                            alt: '}',
                        });
                    }
                    None => return Err(EscapeStmError::UnexpectedEof { range }),
                },
                S::Control => match c.map(|c| c.to_ascii_lowercase()) {
                    Some(c) if c >= 'a' && c <= 'z' => {
                        let num = (c as u64 - 'a' as u64) + 1;

                        self.state = S::Norm;
                        out = Some(num);

                        A::Next
                    }
                    Some(c) => {
                        return Err(EscapeStmError::InvalidControl { range, found: c });
                    }
                    None => return Err(EscapeStmError::UnexpectedEof { range }),
                },
            };

            Ok((action, out.map(|c| EscapeStmOut { cp: c, range })))
        }
    }
}

pub fn tokenize_string(
    ident: Ident,
    out: &mut impl FnMut(u64, SourceSpan) -> Result<(), LowerError>,
) -> Result<(), LowerError> {
    let string = ident.name.as_str().get();

    // http://erlang.org/doc/reference_manual/data_types.html#escape-sequences

    use escape::{EscapeStm, EscapeStmAction, EscapeStmError};

    let mut stm = EscapeStm::new();
    let mut byte_idx = 0;

    for c in string.chars() {
        let idx = ident.span.start() + byte_idx;

        loop {
            let res = stm.transition(Some(c), idx);

            match res {
                Ok((action, result)) => {
                    if let Some(result) = result {
                        out(result.cp, SourceSpan::new(result.range.0, result.range.1))?;
                    }

                    match action {
                        EscapeStmAction::Next => break,
                        EscapeStmAction::Again => continue,
                    }
                }
                Err(err) => Err(LowerError::InvalidStringEscape {
                    span: err.span(),
                    source: err,
                })?,
            }
        }

        byte_idx += c.len_utf8();
    }

    let idx = ident.span.start() + byte_idx;

    loop {
        let res = stm.transition(None, idx);

        match res {
            Ok((action, result)) => {
                if let Some(result) = result {
                    out(result.cp, SourceSpan::new(result.range.0, result.range.1))?;
                }

                match action {
                    EscapeStmAction::Next => break,
                    EscapeStmAction::Again => continue,
                }
            }
            Err(err) => Err(LowerError::InvalidStringEscape {
                span: err.span(),
                source: err,
            })?,
        }
    }

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
        span: SourceSpan,
        codepoint: u64,
        encoding: crate::lower::strings::Encoding,
    },
}

impl ToDiagnostic for StringError {
    fn to_diagnostic(&self) -> Diagnostic {
        let msg = self.to_string();
        match self {
            StringError::CodepointEncoding {
                span,
                codepoint,
                encoding,
            } => Diagnostic::error()
                .with_message(msg)
                .with_labels(vec![Label::primary(span.source_id(), *span)
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
    fn encode(&self, cp: u64, span: SourceSpan) -> Result<Encoded, StringError> {
        match self {
            Encoding::Latin1 => encode_latin1(cp, span),
            Encoding::Utf8 => encode_utf8(cp, span),
            Encoding::Utf16 => encode_utf16(cp, span),
            Encoding::Utf32 => encode_utf32(cp, span),
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

fn encode_utf8(cp: u64, span: SourceSpan) -> Result<Encoded, StringError> {
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
            span,
            codepoint: cp,
            encoding: Encoding::Utf8,
        }),
    }
}

fn encode_utf16(cp: u64, span: SourceSpan) -> Result<Encoded, StringError> {
    match cp {
        0x0000..=0xd7ff => Ok(Encoded::N2((cp >> 8) as u8, (cp >> 0) as u8)),
        0xd800..=0xdfff => Err(StringError::CodepointEncoding {
            span,
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
            span,
            codepoint: cp,
            encoding: Encoding::Utf16,
        }),
    }
}

fn encode_utf32(cp: u64, span: SourceSpan) -> Result<Encoded, StringError> {
    if cp > std::u32::MAX as u64 {
        Err(StringError::CodepointEncoding {
            span,
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

fn encode_latin1(cp: u64, _span: SourceSpan) -> Result<Encoded, StringError> {
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

    use super::{string_to_binary, tokenize_string_to_vec, Encoding, Endianness};

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

    #[test]
    fn test_string_to_binary() {
        assert!(
            string_to_binary(Ident::from_str("abcå"), Encoding::Utf8, Endianness::Big).unwrap()
                == vec![0x61, 0x62, 0x63, 0xc3, 0xa5]
        )
    }
}
