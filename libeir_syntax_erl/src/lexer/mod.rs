//! A fairly basic lexer for Erlang

pub use libeir_intern::symbol::{symbols, SYMBOL_TABLE};
pub use libeir_intern::symbol::{Ident, InternedString, LocalInternedString, Symbol};

mod errors;
mod lexer;
mod token;

pub use self::errors::{LexicalError, TokenConvertError};
pub use self::lexer::Lexer;
pub use self::token::{AtomToken, IdentToken, StringToken, SymbolToken, IntegerToken, TokenType};
pub use self::token::{LexicalToken, Token, DelayedSubstitution};

/// The type that serves as an `Item` for the lexer iterator.
pub type Lexed = Result<LexicalToken, LexicalError>;

/// The result type produced by TryFrom<LexicalToken>
pub type TokenConvertResult<T> = Result<T, TokenConvertError>;
