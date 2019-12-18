use std::str::FromStr;
use std::cmp::Ordering;

use libeir_ir::Integer;
use libeir_intern::{Symbol, Ident};
use libeir_diagnostics::{ByteSpan, ByteIndex, ByteOffset};
use libeir_util_parse::{Source, Scanner};

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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    EOF,

    Comma,
    Dot,
    Pipe,
    SquareOpen,
    SquareClose,
    CurlyOpen,
    CurlyClose,

    Atom(Symbol),
    String(Symbol),
    Integer(Integer),
    Float(Float),
}

#[derive(Debug, Clone, PartialEq, PartialOrd)]
pub struct Float(pub f64);
impl Eq for Float {}

pub struct Lexer<S> {
    scanner: Scanner<S>,
    token: Token,
    token_start: ByteIndex,
    token_end: ByteIndex,
    eof: bool,

    str_buf: String,
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
            token_start: start,
            token_end: start,
            eof: false,

            str_buf: String::new(),
        };
        lexer.advance();
        lexer
    }

    pub fn lex(&mut self) -> Option<<Self as Iterator>::Item> {
        if self.eof && self.token == Token::EOF {
            return None;
        }

        let token = std::mem::replace(&mut self.token, Token::EOF);
        let result = Some(Ok((
            self.token_start.clone(),
            token,
            self.token_end.clone(),
        )));

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

    fn lex_unquoted_atom(&mut self) -> Token {
        let c = self.pop();
        debug_assert!(c.is_ascii_lowercase());

        loop {
            match self.read() {
                '_' => self.skip(),
                '@' => self.skip(),
                '0'..='9' => self.skip(),
                c if c.is_alphanumeric() => self.skip(),
                _ => break,
            }
        }

        Token::Atom(Symbol::intern(self.slice()))
    }

    fn lex_quoted_atom(&mut self) -> Token {
        let c = self.pop();
        debug_assert!(c == '\'');

        self.str_buf.clear();

        loop {
            match self.read() {
                '\\' => {
                    unimplemented!()
                },
                '\'' => {
                    self.skip();
                    break;
                }
                c => {
                    self.skip();
                    self.str_buf.push(c);
                },
            }
        }

        Token::Atom(Symbol::intern(&self.str_buf))
    }

    fn lex_string(&mut self) -> Token {
        let c = self.pop();
        debug_assert!(c == '"');

        self.str_buf.clear();

        loop {
            match self.read() {
                '\\' => {
                    unimplemented!()
                },
                '"' => {
                    self.skip();
                    break;
                },
                c => {
                    self.skip();
                    self.str_buf.push(c);
                },
            }
        }

        Token::String(Symbol::intern(&self.str_buf))
    }

    fn lex_number(&mut self) -> Token {
        let c = self.pop();
        debug_assert!(c == '-' || c == '+' || c.is_digit(10));
        let negative = c == '-';

        while self.read().is_digit(10) {
            self.skip();
        }

        let c = self.read();
        if c == '.' {
            if self.peek().is_digit(10) {
                self.skip();
                return self.lex_float();
            }
            return Token::Integer(
                Integer::from_string_radix(self.slice(), 10).unwrap()
            );
        }

        // TODO Float

        return Token::Integer(
            Integer::from_string_radix(self.slice(), 10).unwrap()
        );
    }

    fn lex_float(&mut self) -> Token {
        let c = self.pop();
        println!("{}", c);
        debug_assert!(c.is_digit(10));

        while self.read().is_digit(10) {
            self.pop();
        }

        match f64::from_str(self.slice()) {
            Ok(f) => Token::Float(Float(f)),
            Err(e) => unimplemented!(),
        }
    }

    fn tokenize(&mut self) -> Token {
        let c = self.read();

        if c == '\0' {
            self.eof = true;
            return Token::EOF;
        }

        if c.is_whitespace() {
            self.skip_whitespace();
        }

        match self.read() {
            '{' => pop!(self, Token::CurlyOpen),
            '}' => pop!(self, Token::CurlyClose),
            '[' => pop!(self, Token::SquareOpen),
            ']' => pop!(self, Token::SquareClose),
            ',' => pop!(self, Token::Comma),
            '.' => pop!(self, Token::Dot),
            '|' => pop!(self, Token::Pipe),
            'a'..='z' | 'A'..='Z' =>
                self.lex_unquoted_atom(),
            '0'..='9' =>
                self.lex_number(),
            '\'' =>
                self.lex_quoted_atom(),
            '"' =>
                self.lex_string(),
            c => unimplemented!("{}", c),
        }
    }

}

impl<S> Iterator for Lexer<S>
where
    S: Source,
{
    type Item = Result<(ByteIndex, Token, ByteIndex), ()>;

    fn next(&mut self) -> Option<Self::Item> {
        self.lex()
    }
}
