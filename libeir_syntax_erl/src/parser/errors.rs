use std::convert::From;

use libeir_diagnostics::*;
use libeir_util_parse::SourceError;
use snafu::Snafu;

use crate::lexer::Token;
use crate::preprocessor::PreprocessorError;

pub type ParseError = lalrpop_util::ParseError<SourceIndex, Token, ()>;

#[derive(Debug, Snafu)]
pub enum ParserError {
    #[snafu(visibility(pub), display("{}", source))]
    Preprocessor { source: PreprocessorError },

    #[snafu(display("{}", source))]
    Source { source: SourceError },

    #[snafu(display("{}", diagnostic.message))]
    ShowDiagnostic { diagnostic: Diagnostic },

    #[snafu(display("invalid token"))]
    InvalidToken { location: SourceIndex },

    #[snafu(display("unrecognized token"))]
    UnrecognizedToken {
        span: SourceSpan,
        expected: Vec<String>,
    },

    #[snafu(display("extra token"))]
    ExtraToken { span: SourceSpan },

    #[snafu(display("unexpected eof"))]
    UnexpectedEOF {
        location: SourceIndex,
        expected: Vec<String>,
    },
}
impl From<ParseError> for ParserError {
    fn from(err: ParseError) -> Self {
        use lalrpop_util::ParseError::*;
        match err {
            InvalidToken { location } => Self::InvalidToken { location },
            UnrecognizedEOF { location, expected } => Self::UnexpectedEOF { location, expected },
            UnrecognizedToken {
                token: (l, _, r),
                expected,
            } => Self::UnrecognizedToken {
                span: SourceSpan::new(l, r),
                expected,
            },
            ExtraToken { token: (l, _, r) } => Self::ExtraToken {
                span: SourceSpan::new(l, r),
            },
            User { .. } => panic!(),
        }
    }
}

impl ToDiagnostic for ParserError {
    fn to_diagnostic(&self) -> Diagnostic {
        match self {
            Self::ShowDiagnostic { diagnostic } => diagnostic.clone(),
            Self::Preprocessor { source } => source.to_diagnostic(),
            Self::Source { source } => source.to_diagnostic(),
            Self::UnrecognizedToken {
                ref span,
                ref expected,
            } => Diagnostic::error()
                .with_message("unrecognized token")
                .with_labels(vec![Label::primary(span.source_id(), *span)
                    .with_message(format!("expected: {}", expected.join(", ")))]),
            Self::InvalidToken { location } => {
                let index = *location;
                Diagnostic::error()
                    .with_message("unexpected token")
                    .with_labels(vec![Label::primary(
                        index.source_id(),
                        SourceSpan::new(index, index),
                    )
                    .with_message("did not expect this token")])
            }
            Self::UnexpectedEOF {
                location,
                ref expected,
            } => {
                let index = *location;
                Diagnostic::error()
                    .with_message("unexpected end of file")
                    .with_labels(vec![Label::primary(
                        index.source_id(),
                        SourceSpan::new(index, index),
                    )
                    .with_message(format!("expected: {}", expected.join(", ")))])
            }
            Self::ExtraToken { span } => Diagnostic::error()
                .with_message("unexpected token")
                .with_labels(vec![Label::primary(span.source_id(), *span)
                    .with_message("did not expect this token")]),
        }
    }
}

impl From<SourceError> for ParserError {
    fn from(source: SourceError) -> Self {
        Self::Source { source }
    }
}
impl From<PreprocessorError> for ParserError {
    fn from(source: PreprocessorError) -> Self {
        Self::Preprocessor { source }
    }
}
