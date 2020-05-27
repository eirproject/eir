use snafu::Snafu;

use libeir_diagnostics::{Diagnostic, Label, SourceIndex, SourceSpan, ToDiagnostic};
use libeir_util_parse::SourceError;

use super::lexer::Token;
use crate::text::ast::LowerError;

pub type ParseError = lalrpop_util::ParseError<SourceIndex, Token, ParserError>;

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
        span: SourceSpan,
    },

    UnexpectedEOF {
        span: SourceSpan,
        expected: Vec<String>,
    },

    UnexpectedToken {
        span: SourceSpan,
        expected: Vec<String>,
    },

    UnrecognizedToken {
        span: SourceSpan,
        expected: Vec<String>,
    },

    ExtraToken {
        span: SourceSpan,
    },

    ShowDiagnostic {
        diagnostic: Diagnostic,
    },
}

impl ParserError {
    pub fn span(&self) -> Option<SourceSpan> {
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
                let span = span.unwrap();
                Diagnostic::error()
                    .with_message(format!("expected: {}", expected.join(", ")))
                    .with_labels(vec![
                        Label::primary(span.source_id(), span).with_message(msg)
                    ])
            }
            _ if span.is_some() => {
                let span = span.unwrap();
                Diagnostic::error()
                    .with_message(msg)
                    .with_labels(vec![Label::primary(span.source_id(), span)])
            }
            _ => Diagnostic::error().with_message(msg),
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
            E::InvalidToken { location } => ParserError::InvalidToken {
                span: SourceSpan::new(location, location),
            },
            E::UnrecognizedEOF { location, expected } => ParserError::UnexpectedEOF {
                span: SourceSpan::new(location, location),
                expected,
            },
            E::UnrecognizedToken {
                token: (start, _tok, end),
                expected,
            } => ParserError::UnrecognizedToken {
                span: SourceSpan::new(start, end),
                expected,
            },
            E::ExtraToken {
                token: (start, _, end),
            } => ParserError::ExtraToken {
                span: SourceSpan::new(start, end),
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
