#![allow(dead_code, unreachable_code, unused_mut, unused_variables, unused_macros)]

use std::collections::HashMap;

use lazy_static::lazy_static;

use libeir_intern::{Symbol, Ident};
use libeir_diagnostics::{ByteIndex, ByteOffset, ByteSpan};
use libeir_util_parse::{Source, Scanner};

use crate::constant::Integer;
use super::errors::ParserError;

macro_rules! pop {
    ($lex:ident) => {{
        $lex.skip();
    }};
    ($lex:ident, $code:expr) => {{
        $lex.skip();
        $code
    }};
}

macro_rules! pop2 {
    ($lex:ident) => {{
        $lex.skip();
        $lex.skip();
    }};
    ($lex:ident, $code:expr) => {{
        $lex.skip();
        $lex.skip();
        $code
    }};
}

macro_rules! pop3 {
    ($lex:ident) => {{
        $lex.skip();
        $lex.skip();
        $lex.skip()
    }};
    ($lex:ident, $code:expr) => {{
        $lex.skip();
        $lex.skip();
        $lex.skip();
        $code
    }};
}

#[derive(Clone, Debug, PartialEq)]
pub enum LexicalError {
}

#[derive(Debug, Clone, PartialEq)]
pub struct LexicalToken(pub ByteIndex, pub Token, pub ByteIndex);
impl LexicalToken {
    pub fn token(&self) -> Token {
        self.1.clone()
    }
    pub fn span(&self) -> ByteSpan {
        ByteSpan::new(self.0, self.2)
    }
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Token {
    EOF,

    // Identifiers
    /// something
    Ident(Ident),
    Variable(Ident),

    // Atomics
    /// a'true'
    Atom(Ident),
    /// 12
    Integer(Integer),
    /// 12.2
    Float(Ident),

    // Symbols
    ParenOpen,
    ParenClose,
    CurlyOpen,
    CurlyClose,
    SquareOpen,
    SquareClose,
    Less,
    Greater,
    MapOpen,
    Colon,
    Semicolon,
    Comma,
    Question,
    ForwardSlash,
    Percent,
    Equals,
    EqualsEquals,
    FatArrow,
    Underscore,
    Pipe,
    At,

    // Keywords
    Unreachable,
    IfBool,
    UnpackValueList,
    ValueList,
    Tuple,
    Arity,
    TraceCaptureRaw,
    Value,
    Match,
    Type,
    Case,
    Guard,
    Except,

}

lazy_static! {
    static ref KEYWORDS: HashMap<Symbol, Token> = {
        let mut map = HashMap::new();
        map.insert(Symbol::intern("unreachable"), Token::Unreachable);
        map.insert(Symbol::intern("if_bool"), Token::IfBool);
        map.insert(Symbol::intern("unpack_value_list"), Token::UnpackValueList);
        map.insert(Symbol::intern("value_list"), Token::ValueList);
        map.insert(Symbol::intern("tuple"), Token::Tuple);
        map.insert(Symbol::intern("unpack"), Token::UnpackValueList);
        map.insert(Symbol::intern("arity"), Token::Arity);
        map.insert(Symbol::intern("trace_capture_raw"), Token::TraceCaptureRaw);
        map.insert(Symbol::intern("value"), Token::Value);
        map.insert(Symbol::intern("match"), Token::Match);
        map.insert(Symbol::intern("type"), Token::Type);
        map.insert(Symbol::intern("case"), Token::Case);
        map.insert(Symbol::intern("except"), Token::Except);
        map.insert(Symbol::intern("guard"), Token::Guard);
        map
    };
}

fn is_escapechar(c: char) -> bool {
    c == 'b' || c == 'd' || c == 'e'
        || c == 'f' || c == 'n' || c == 'r'
        || c == 's' || c == 't' || c == 'v'
        || c == '"' || c == '\'' || c == '\\'
}
fn is_control(c: char) -> bool {
    c >= '\u{0000}' && c <= '\u{001f}'
}
fn is_inputchar(c: char) -> bool {
    c != '\n' && c != '\r'
}
fn is_digit(c: char) -> bool {
    c >= '0' && c <= '9'
}
fn is_octal(c: char) -> bool {
    c >= '0' && c <= '7'
}
fn is_uppercase(c: char) -> bool {
    (c >= 'A' && c <= 'Z')
        || (c >= '\u{00c0}' && c <= '\u{00d6}')
        || (c >= '\u{00d8}' && c <= '\u{00de}')
}
fn is_lowercase(c: char) -> bool {
    (c >= 'a' && c <= 'z')
        || (c >= '\u{00df}' && c <= '\u{00f6}')
        || (c >= '\u{00f8}' && c <= '\u{00ff}')
}
fn is_namechar(c: char) -> bool {
    is_uppercase(c) || is_lowercase(c) || is_digit(c)
        || (c == '@') || (c == '_')
}

pub struct Lexer<S> {
    scanner: Scanner<S>,
    token: Token,
    token_start: ByteIndex,
    token_end: ByteIndex,
    eof: bool,
}

impl<S> Lexer<S>
where
    S: Source,
{

    pub fn new(scanner: Scanner<S>) -> Self {
        let start = scanner.start();
        let mut lexer = Lexer {
            scanner,
            token: Token::EOF,
            token_start: start + ByteOffset(0),
            token_end: start + ByteOffset(0),
            eof: false,
        };
        lexer.advance();
        lexer
    }

    pub fn lex(&mut self) -> Option<<Self as Iterator>::Item> {
        if self.eof && self.token == Token::EOF {
            return None;
        }

        let token = std::mem::replace(&mut self.token, Token::EOF);
        //let result = if let Token::Error(err) = token {
        //    Some(Err(err))
        //} else {
        let result = Some(Ok((
            self.token_start.clone(),
            token,
            self.token_end.clone(),
        )));
        //};

        self.advance();

        result
    }

    fn advance(&mut self) {
        self.advance_start();
        self.token = self.tokenize();
    }

    fn advance_start(&mut self) {
        let mut position: ByteIndex;
        loop {
            let (pos, c) = self.scanner.read();
            position = pos;

            if c == '\0' {
                self.eof = true;
                return;
            }

            if c.is_whitespace() {
                self.scanner.advance();
                continue;
            }

            if c == '!' {
                'inner: loop {
                    let (pos, c) = self.scanner.read();

                    if c == '\n' {
                        break 'inner;
                    }

                    if c == '\0' {
                        self.eof = true;
                        return;
                    }

                    self.skip();
                }
                continue;
            }

            break;
        }

        self.token_start = position;
    }

    fn pop(&mut self) -> char {
        let (pos, c) = self.scanner.pop();
        self.token_end = pos + ByteOffset::from_char_utf8(c);
        c
    }
    fn peek(&mut self) -> char {
        self.scanner.peek().1
    }
    fn peek_next(&mut self) -> char {
        self.scanner.peek_next().1
    }
    fn read(&mut self) -> char {
        self.scanner.read().1
    }
    fn skip(&mut self) {
        self.pop();
    }
    pub fn span(&self) -> ByteSpan {
        ByteSpan::new(self.token_start, self.token_end)
    }
    fn slice(&self) -> &str {
        self.scanner.slice(self.span())
    }
    fn slice_span(&self, span: ByteSpan) -> &str {
        self.scanner.slice(span)
    }
    fn ident(&self) -> Ident {
        let symbol = Symbol::intern(self.slice());
        Ident::new(symbol, self.span())
    }
    fn skip_whitespace(&mut self) {
        let mut c: char;
        while self.read().is_whitespace() {
            self.skip();
        }
    }

    fn tokenize(&mut self) -> Token {
        let mut c = self.read();

        if c == '\0' {
            self.eof = true;
            return Token::EOF;
        }

        if c.is_whitespace() {
            self.skip_whitespace();
        }

        match self.read() {
            '(' => pop!(self, Token::ParenOpen),
            ')' => pop!(self, Token::ParenClose),
            '{' => pop!(self, Token::CurlyOpen),
            '}' => pop!(self, Token::CurlyClose),
            '[' => pop!(self, Token::SquareOpen),
            ']' => pop!(self, Token::SquareClose),
            '<' => pop!(self, Token::Less),
            '>' => pop!(self, Token::Greater),
            '%' => match self.peek() {
                '{' => pop2!(self, Token::MapOpen),
                c if c.is_alphanumeric() => self.lex_variable(),
                _ => unimplemented!(),
            },
            ',' => pop!(self, Token::Comma),
            ':' => pop!(self, Token::Colon),
            ';' => pop!(self, Token::Semicolon),
            '/' => pop!(self, Token::ForwardSlash),
            '|' => pop!(self, Token::Pipe),
            '=' => match self.peek() {
                '>' => pop2!(self, Token::FatArrow),
                '=' => pop2!(self, Token::EqualsEquals),
                _ => pop!(self, Token::Equals),
            },
            '_' => pop!(self, Token::Underscore),
            '@' => pop!(self, Token::At),
            c if c == 'a' => {
                match self.peek() {
                    '\'' => self.lex_atom(),
                    _ => self.lex_ident(),
                }
            },
            c if c.is_alphabetic() =>
                self.lex_ident(),
            c if c.is_numeric() =>
                self.lex_integer(),
            c => unimplemented!("{}", c),
        }
    }

    fn lex_ident(&mut self) -> Token {
        let c = self.pop();
        debug_assert!(c.is_alphabetic());

        loop {
            match self.read() {
                '_' => self.skip(),
                '0'..='9' => self.skip(),
                c if c.is_alphabetic() => self.skip(),
                _ => break,
            }
        }

        let ident = self.ident();

        if let Some(tok) = KEYWORDS.get(&ident.name) {
            tok.clone()
        } else {
            Token::Ident(ident)
        }
    }

    fn lex_variable(&mut self) -> Token {
        let c = self.pop();
        debug_assert!(c == '%');

        loop {
            match self.read() {
                '_' => self.skip(),
                '0'..='9' => self.skip(),
                c if c.is_alphabetic() => self.skip(),
                _ => break,
            }
        }

        let symbol = Symbol::intern(&self.slice()[1..]);
        let ident = Ident::new(symbol, self.span());

        Token::Variable(ident)
    }

    fn lex_integer(&mut self) -> Token {
        let c = self.pop();
        debug_assert!(c.is_numeric());

        loop {
            match self.read() {
                c if c.is_numeric() => self.skip(),
                _ => break,
            }
        }

        let int = self.slice().parse().unwrap();
        Token::Integer(int)
    }

    fn lex_atom(&mut self) -> Token {
        debug_assert!(self.pop() == 'a');
        debug_assert!(self.pop() == '\'');

        loop {
            match self.read() {
                '\'' => {
                    self.skip();
                    break
                },
                '\\' => {
                    self.skip();
                    self.skip();
                }
                c => {
                    self.skip();
                },
            }
        }

        let slice = self.slice();
        let inner_slice = &self.slice()[2..(slice.len() - 1)];

        let ident = Ident::new(Symbol::intern(inner_slice), self.span());

        Token::Atom(ident)
    }

}

impl<S> Iterator for Lexer<S>
where
    S: Source,
{
    type Item = Result<(ByteIndex, Token, ByteIndex), ParserError>;

    fn next(&mut self) -> Option<Self::Item> {
        let t = self.lex();
        t
    }
}
