//! A fairly basic lexer for Erlang

pub use libeir_intern::symbol::{symbols, SYMBOL_TABLE};
pub use libeir_intern::symbol::{Ident, InternedString, LocalInternedString, Symbol};

mod errors;
mod lexer;
mod scanner;
mod source;
mod token;

pub use self::errors::{LexicalError, TokenConvertError};
pub use self::lexer::Lexer;
pub use self::scanner::Scanner;
pub use self::source::{FileMapSource, Source, SourceError};
pub use self::token::{AtomToken, IdentToken, StringToken, SymbolToken, TokenType};
pub use self::token::{LexicalToken, Token};

/// The type that serves as an `Item` for the lexer iterator.
pub type Lexed = Result<LexicalToken, LexicalError>;

/// The result type produced by TryFrom<LexicalToken>
pub type TokenConvertResult<T> = Result<T, TokenConvertError>;
