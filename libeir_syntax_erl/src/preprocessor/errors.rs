use std;
use std::path::PathBuf;

use itertools::Itertools;
use snafu::Snafu;

use libeir_diagnostics::*;
use libeir_intern::Symbol;
use libeir_util_parse::SourceError;

use crate::lexer::{LexicalError, LexicalToken, TokenConvertError};
use crate::parser::ParserError;

use super::directive::Directive;
use super::directives::DirectiveError;
use super::macros::{MacroCall, MacroDef, Stringify};

#[derive(Debug, Snafu)]
pub enum PreprocessorError {
    #[snafu(visibility(pub), display("{}", source))]
    Lexical { source: LexicalError },

    #[snafu(visibility(pub), display("{}", source))]
    Source { source: SourceError },

    #[snafu(
        visibility(pub),
        display("{} occurred while including {:?}", source, path)
    )]
    IncludeError {
        source: std::io::Error,
        path: PathBuf,
        span: SourceSpan,
    },

    #[snafu(display("unable to parse constant expression"))]
    ParseError {
        span: SourceSpan,
        inner: Box<ParserError>,
    },

    #[snafu(display("{}", reason))]
    CompilerError {
        span: Option<SourceSpan>,
        reason: String,
    },

    #[snafu(display("invalid constant expression found in preprocessor directive"))]
    InvalidConstExpression { span: SourceSpan },

    #[snafu(visibility(pub))]
    BadDirective { source: DirectiveError },

    #[snafu(display("invalid conditional expression"))]
    InvalidConditional { span: SourceSpan },

    #[snafu(visibility(pub), display("call to builtin function failed"))]
    BuiltinFailed {
        span: SourceSpan,
        source: Box<dyn std::error::Error>,
    },

    #[snafu(display("found orphaned '-end.' directive"))]
    OrphanedEnd { directive: Directive },

    #[snafu(display("found orphaned '-else.' directive"))]
    OrphanedElse { directive: Directive },

    #[snafu(display("undefined macro"))]
    UndefinedStringifyMacro { call: Stringify },

    #[snafu(display("undefined macro"))]
    UndefinedMacro { call: MacroCall },

    #[snafu(display("invalid macro invocation"))]
    BadMacroCall {
        call: MacroCall,
        def: MacroDef,
        reason: String,
    },

    #[snafu(display("{}", diagnostic.message))]
    ShowDiagnostic { diagnostic: Diagnostic },

    #[snafu(display("unexpected token"))]
    InvalidTokenType {
        token: LexicalToken,
        expected: String,
    },

    #[snafu(display("unexpected token"))]
    UnexpectedToken {
        token: LexicalToken,
        expected: Vec<String>,
    },

    #[snafu(display("unexpected eof"))]
    UnexpectedEOF,

    #[snafu(display("warning directive: {}", message))]
    WarningDirective {
        span: SourceSpan,
        message: Symbol,
        as_error: bool,
    },
}
impl PreprocessorError {
    pub fn to_diagnostic(&self) -> Diagnostic {
        //let span = self.span();
        //let msg = self.to_string();
        match self {
            PreprocessorError::Lexical { source } => source.to_diagnostic(),
            PreprocessorError::Source { source } => source.to_diagnostic(),
            PreprocessorError::IncludeError { source, span, .. } => {
                Diagnostic::error()
                    .with_message(self.to_string())
                    .with_labels(vec![
                        Label::primary(span.source_id(), *span)
                        .with_message("while processing include directive"),
                    ])
            },
            PreprocessorError::ParseError { span, inner } => {
                let err = inner.to_diagnostic();
                let mut labels = vec![
                    Label::primary(span.source_id(), *span)
                        .with_message("invalid constant expression")
                ];
                for label in err.labels {
                    labels.push(label);
                }
                Diagnostic::error()
                    .with_message(self.to_string())
                    .with_labels(labels)
            }
            PreprocessorError::CompilerError { span: Some(span), reason } =>
                Diagnostic::error()
                    .with_message("found error directive")
                    .with_labels(vec![
                        Label::primary(span.source_id(), *span)
                            .with_message(reason)
                    ]),
            PreprocessorError::CompilerError { span: None, reason } =>
                Diagnostic::error().with_message(reason),
            PreprocessorError::InvalidConstExpression { span, .. } =>
                Diagnostic::error()
                    .with_message(self.to_string())
                    .with_labels(vec![
                        Label::primary(span.source_id(), *span)
                            .with_message("expected valid constant expression (example: `?OTP_VERSION >= 21`)")
                    ]),
            PreprocessorError::BadDirective { source } => source.to_diagnostic(),
            PreprocessorError::InvalidConditional { span, .. } =>
                Diagnostic::error()
                    .with_message(self.to_string())
                    .with_labels(vec![
                        Label::primary(span.source_id(), *span)
                            .with_message("expected 'true', 'false', or an expression which can be evaluated to 'true' or 'false'")
                    ]),
            PreprocessorError::BuiltinFailed { span, source } =>
                Diagnostic::error()
                    .with_message(self.to_string())
                    .with_labels(vec![
                        Label::primary(span.source_id(), *span)
                            .with_message(source.to_string())
                    ]),
            PreprocessorError::OrphanedEnd { directive } => {
                let span = directive.span();
                Diagnostic::error()
                    .with_message(self.to_string())
                    .with_labels(vec![
                        Label::primary(span.source_id(), span)
                    ])
            }
            PreprocessorError::OrphanedElse { directive } => {
                let span = directive.span();
                Diagnostic::error()
                    .with_message(self.to_string())
                    .with_labels(vec![
                        Label::primary(span.source_id(), span)
                    ])
            }
            PreprocessorError::UndefinedStringifyMacro { call } => {
                let span = call.span();
                Diagnostic::error()
                    .with_message(self.to_string())
                    .with_labels(vec![
                        Label::primary(span.source_id(), span)
                    ])
            }
            PreprocessorError::UndefinedMacro { call } => {
                let span = call.span();
                Diagnostic::error()
                    .with_message(self.to_string())
                    .with_labels(vec![
                        Label::primary(span.source_id(), span)
                    ])
            }
            PreprocessorError::BadMacroCall { call, def: MacroDef::String(_), reason, .. } => {
                let span = call.span();
                Diagnostic::error()
                    .with_message(self.to_string())
                    .with_labels(vec![
                        Label::primary(span.source_id(), span)
                            .with_message(reason.to_owned())
                    ])
            }
            PreprocessorError::BadMacroCall { call, def, reason, .. } => {
                let secondary_span = match def {
                    MacroDef::Static(ref define) => define.span(),
                    MacroDef::Dynamic(ref tokens) => {
                        assert!(tokens.len() > 0);
                        SourceSpan::new(
                            tokens[0].span().start(),
                            tokens.last().unwrap().span().end()
                        )
                    },
                    _ => unreachable!()
                };
                let call_span = call.span();
                Diagnostic::error()
                    .with_message(self.to_string())
                    .with_labels(vec![
                        Label::primary(call_span.source_id(), call_span)
                            .with_message("this macro call does not match its definition"),
                        Label::secondary(secondary_span.source_id(), secondary_span)
                            .with_message(reason.to_owned())
                    ])
            }
            PreprocessorError::ShowDiagnostic { diagnostic } => diagnostic.clone(),
            PreprocessorError::InvalidTokenType { token, expected } => {
                let token_span = token.span();
                Diagnostic::error()
                    .with_message(self.to_string())
                    .with_labels(vec![
                        Label::primary(token_span.source_id(), token_span)
                            .with_message(format!("expected \"{}\"", expected))
                    ])
            }
            PreprocessorError::UnexpectedToken { token, expected } => {
                let token_span = token.span();
                if expected.len() > 0 {
                    let expected = expected.iter()
                        .map(|t| format!("\"{}\"", t))
                        .join(", ");
                    Diagnostic::error()
                        .with_message(self.to_string())
                        .with_labels(vec![
                            Label::primary(token_span.source_id(), token_span)
                                .with_message(format!("expected one of {}", expected))
                        ])
                } else {
                    Diagnostic::error()
                        .with_message(self.to_string())
                        .with_labels(vec![
                            Label::primary(token_span.source_id(), token_span)
                        ])
                }
            }
            PreprocessorError::UnexpectedEOF =>
                Diagnostic::error().with_message(self.to_string()),
            PreprocessorError::WarningDirective { span, message, as_error } => {
                let message_str = message.as_str().get();
                if *as_error { Diagnostic::error() } else { Diagnostic::warning() }
                    .with_message("found warning directive")
                    .with_labels(vec![
                        Label::primary(span.source_id(), *span).with_message(message_str),
                    ])
            }
        }
    }
}
impl From<LexicalError> for PreprocessorError {
    fn from(source: LexicalError) -> PreprocessorError {
        PreprocessorError::Lexical { source }
    }
}
impl From<SourceError> for PreprocessorError {
    fn from(source: SourceError) -> PreprocessorError {
        PreprocessorError::Source { source }
    }
}
impl From<TokenConvertError> for PreprocessorError {
    fn from(err: TokenConvertError) -> PreprocessorError {
        let span = err.span;
        let token = LexicalToken(span.start(), err.token, span.end());
        PreprocessorError::InvalidTokenType {
            token,
            expected: err.expected.to_string(),
        }
    }
}
impl From<Diagnostic> for PreprocessorError {
    fn from(diagnostic: Diagnostic) -> Self {
        PreprocessorError::ShowDiagnostic { diagnostic }
    }
}
