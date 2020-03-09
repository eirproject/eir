use snafu::Snafu;

use libeir_diagnostics::{ByteIndex, ByteSpan, Diagnostic, Label};
use libeir_util_parse::{SourceError, ToDiagnostic};

use super::lexer::Token;
use crate::text::ast::LowerError;

pub type ParseError = lalrpop_util::ParseError<ByteIndex, Token, ParserError>;

pub type Errors = libeir_util_parse::Errors<ParserError, ParserError>;

#[derive(Debug, Snafu)]
#[snafu(visibility = "pub(super)")]
pub enum ParserError {

    #[snafu(display("{}", source))]
    Source {
        source: SourceError,
    },

    Lower {
        source: LowerError,
    },

    InvalidToken {
        span: ByteSpan,
    },

    UnexpectedEOF {
        span: ByteSpan,
        expected: Vec<String>,
    },

    UnexpectedToken {
        span: ByteSpan,
        expected: Vec<String>,
    },

    UnrecognizedToken {
        span: ByteSpan,
        expected: Vec<String>,
    },

    ExtraToken {
        span: ByteSpan,
    },

    ShowDiagnostic {
        diagnostic: Diagnostic,
    },

}

impl ParserError {

    pub fn span(&self) -> Option<ByteSpan> {
        match self {
            ParserError::InvalidToken { span } => Some(*span),
            ParserError::UnrecognizedToken { span, .. } => Some(*span),
            ParserError::ExtraToken { span } => Some(*span),
            _ => None,
        }
    }

}

impl ToDiagnostic for ParserError {
    fn to_diagnostic(&self) -> Diagnostic {
        let span = self.span();
        let msg = self.to_string();
        match self {
            ParserError::ShowDiagnostic { diagnostic } => diagnostic.clone(),
            ParserError::Source { source } => source.to_diagnostic(),
            ParserError::Lower { source } => source.to_diagnostic(),
            ParserError::UnrecognizedToken { expected, .. } => {
                Diagnostic::new_error(format!("expected: {}", expected.join(", ")))
                    .with_label(Label::new_primary(span.unwrap()).with_message(msg))
            },
            _ if span.is_some() => {
                Diagnostic::new_error(msg).with_label(Label::new_primary(span.unwrap()))
            },
            _ => Diagnostic::new_error(msg),
        }
    }
}

impl From<LowerError> for ParserError {
    fn from(err: LowerError) -> ParserError {
        ParserError::Lower { source: err }
    }
}
impl From<ParseError> for ParserError {
    fn from(err: ParseError) -> ParserError {
        use lalrpop_util::ParseError as E;
        match err {
            E::InvalidToken { location } => {
                ParserError::InvalidToken {
                    span: ByteSpan::new(location, location),
                }
            },
            E::UnrecognizedEOF { location, expected } => {
                ParserError::UnexpectedEOF {
                    span: ByteSpan::new(location, location),
                    expected,
                }
            },
            E::UnrecognizedToken { token: (start, _tok, end), expected } => {
                ParserError::UnrecognizedToken {
                    span: ByteSpan::new(start, end),
                    expected,
                }
            },
            E::ExtraToken { token: (start, _, end) } => {
                ParserError::ExtraToken {
                    span: ByteSpan::new(start, end),
                }
            },
            E::User { error } => error,
        }
    }
}
impl From<Diagnostic> for ParserError {
    fn from(diagnostic: Diagnostic) -> ParserError {
        ParserError::ShowDiagnostic { diagnostic }
    }
}
