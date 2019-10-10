use std::convert::From;

use libeir_util_parse::SourceError;
use libeir_diagnostics::{ByteIndex, ByteSpan, Diagnostic, Label};
use snafu::Snafu;

use crate::lexer::{Token};
use crate::preprocessor::PreprocessorError;

pub type ParseError = lalrpop_util::ParseError<ByteIndex, Token, ParserError>;

#[derive(Debug, Snafu)]
pub enum ParserError {

    #[snafu(visibility(pub), display("{}", source))]
    Preprocessor {
        source: PreprocessorError,
    },

    #[snafu(display("{}", source))]
    Source {
        source: SourceError,
    },

    #[snafu(display("{}", diagnostic))]
    ShowDiagnostic {
        diagnostic: Diagnostic,
    },

    #[snafu(display("invalid token"))]
    InvalidToken {
        span: ByteSpan,
    },

    #[snafu(display("unrecognized token"))]
    UnrecognizedToken {
        span: ByteSpan,
        expected: Vec<String>,
    },

    #[snafu(display("extra token"))]
    ExtraToken {
        span: ByteSpan,
    },

    #[snafu(display("unexpected eof"))]
    UnexpectedEOF {
        span: ByteSpan,
        expected: Vec<String>,
    },

}
impl From<ParseError> for ParserError {
    fn from(err: ParseError) -> ParserError {
        match err {
            lalrpop_util::ParseError::InvalidToken { location } => ParserError::InvalidToken {
                span: ByteSpan::new(location, location),
            },
            lalrpop_util::ParseError::UnrecognizedEOF {
                location,
                expected,
            } => ParserError::UnexpectedEOF {
                span: ByteSpan::new(location, location),
                expected,
            },
            lalrpop_util::ParseError::UnrecognizedToken {
                token: (start, _, end),
                expected,
            } => ParserError::UnrecognizedToken {
                span: ByteSpan::new(start, end),
                expected,
            },
            lalrpop_util::ParseError::ExtraToken {
                token: (start, _, end),
            } => ParserError::ExtraToken {
                span: ByteSpan::new(start, end),
            },
            lalrpop_util::ParseError::User { error: err } => err.into(),
        }
    }
}
impl ParserError {
    pub fn span(&self) -> Option<ByteSpan> {
        match self {
            ParserError::Preprocessor{ source } => source.span(),
            ParserError::InvalidToken { span, .. } => Some(span.clone()),
            ParserError::UnrecognizedToken { span, .. } => Some(span.clone()),
            ParserError::ExtraToken { span, .. } => Some(span.clone()),
            _ => None,
        }
    }

    pub fn to_diagnostic(&self) -> Diagnostic {
        let span = self.span();
        let msg = self.to_string();
        match self {
            ParserError::ShowDiagnostic { diagnostic } => diagnostic.clone(),
            ParserError::Preprocessor { source } => source.to_diagnostic(),
            ParserError::Source { source } => source.to_diagnostic(),
            //ParserError::IO { .. } => Diagnostic::new_error("i/o failed")
            //    .with_label(Label::new_primary(span.unwrap()).with_message(msg)),
            ParserError::UnrecognizedToken { ref expected, .. } => {
                Diagnostic::new_error(format!("expected: {}", expected.join(", ")))
                    .with_label(Label::new_primary(span.unwrap()).with_message(msg))
            }
            _ if span.is_some() => {
                Diagnostic::new_error(msg).with_label(Label::new_primary(span.unwrap()))
            }
            _ => Diagnostic::new_error(msg),
        }
    }
}

impl From<SourceError> for ParserError {
    fn from(source: SourceError) -> Self {
        ParserError::Source {
            source,
        }
    }
}
impl From<PreprocessorError> for ParserError {
    fn from(source: PreprocessorError) -> Self {
        ParserError::Preprocessor {
            source,
        }
    }
}
