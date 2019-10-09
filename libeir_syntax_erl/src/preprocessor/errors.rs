use std;

use libeir_diagnostics::{ByteSpan, Diagnostic, Label};
use itertools::Itertools;
use snafu::Snafu;

use libeir_util_parse::{PathVariableSubstituteError, SourceError};
use crate::lexer::{LexicalError, LexicalToken, TokenConvertError};

use crate::parser::ParserError;

use super::directive::Directive;
use super::directives::DirectiveError;
use super::macros::{MacroCall, MacroDef, Stringify};

#[derive(Debug, Snafu)]
pub enum PreprocessorError {

    #[snafu(visibility(pub), display("{}", source))]
    Lexical {
        source: LexicalError,
    },

    #[snafu(visibility(pub), display("{}", source))]
    Source {
        source: SourceError,
    },

    #[snafu(display("unable to parse constant expression"))]
    ParseError {
        span: ByteSpan,
        errors: Vec<ParserError>,
    },

    #[snafu(display("{}", reason))]
    CompilerError {
        span: Option<ByteSpan>,
        reason: String,
    },

    #[snafu(display("invalid constant expression found in preprocessor directive"))]
    InvalidConstExpression {
        span: ByteSpan,
    },

    #[snafu(visibility(pub))]
    BadDirective {
        source: DirectiveError,
    },

    #[snafu(display("invalid conditional expression"))]
    InvalidConditional {
        span: ByteSpan,
    },

    #[snafu(visibility(pub), display("call to builtin function failed"))]
    BuiltinFailed {
        span: ByteSpan,
        source: Box<dyn std::error::Error>,
    },

    #[snafu(display("found orphaned '-end.' directive"))]
    OrphanedEnd {
        directive: Directive,
    },

    #[snafu(display("found orphaned '-else.' directive"))]
    OrphanedElse {
        directive: Directive,
    },

    #[snafu(display("undefined macro"))]
    UndefinedStringifyMacro {
        call: Stringify,
    },

    #[snafu(display("undefined macro"))]
    UndefinedMacro {
        call: MacroCall
    },

    #[snafu(display("invalid macro invocation"))]
    BadMacroCall {
        call: MacroCall,
        def: MacroDef,
        reason: String
    },

    #[snafu(display("orphaned else"))]
    OrphanedBranchElse,

    #[snafu(visibility(pub), display("{}", source))]
    PathSubstitute {
        source: PathVariableSubstituteError,
    },

    #[snafu(display("{}", diagnostic))]
    ShowDiagnostic {
        diagnostic: Diagnostic,
    },

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
}
impl PreprocessorError {
    pub fn span(&self) -> Option<ByteSpan> {
        match self {
            PreprocessorError::Lexical { source } => Some(source.span()),
            PreprocessorError::ParseError { span, .. } => Some(span.clone()),
            PreprocessorError::CompilerError { span: Some(ref span), .. } =>
                Some(span.clone()),
            PreprocessorError::InvalidConstExpression { span } => Some(span.clone()),
            PreprocessorError::InvalidConditional { span } => Some(span.clone()),
            PreprocessorError::BuiltinFailed { span, .. } => Some(span.clone()),
            PreprocessorError::OrphanedEnd { directive } => Some(directive.span()),
            PreprocessorError::OrphanedElse { directive } => Some(directive.span()),
            PreprocessorError::UndefinedStringifyMacro { call } => Some(call.span()),
            PreprocessorError::UndefinedMacro { call } => Some(call.span()),
            PreprocessorError::BadMacroCall { call, .. } => Some(call.span()),
            PreprocessorError::InvalidTokenType { token, .. } => Some(token.span()),
            PreprocessorError::UnexpectedToken { token, .. } => Some(token.span()),
            _ => None,
        }
    }

    pub fn to_diagnostic(&self) -> Diagnostic {
        let span = self.span();
        let msg = self.to_string();
        match self {
            PreprocessorError::BadDirective { source } => source.to_diagnostic(),
            PreprocessorError::ShowDiagnostic { diagnostic } => diagnostic.clone(),
            PreprocessorError::Lexical { source } => source.to_diagnostic(),
            PreprocessorError::Source { source } => source.to_diagnostic(),
            PreprocessorError::ParseError { errors, .. } => {
                let mut d = Diagnostic::new_error(msg)
                    .with_label(Label::new_primary(span.unwrap())
                        .with_message("invalid constant expression"));
                for err in errors.iter() {
                    let err = err.to_diagnostic();
                    for label in err.labels {
                        d = d.with_label(label);
                    }
                }
                d
            }
            PreprocessorError::CompilerError { span: Some(_), .. } =>
                Diagnostic::new_error("found error directive")
                    .with_label(Label::new_primary(span.unwrap())
                        .with_message(msg)),
            PreprocessorError::InvalidConstExpression { .. } =>
                Diagnostic::new_error(msg)
                    .with_label(Label::new_primary(span.unwrap())
                        .with_message("expected valid constant expression (example: `?OTP_VERSION >= 21`)")),
            PreprocessorError::InvalidConditional { .. } =>
                Diagnostic::new_error(msg)
                    .with_label(Label::new_primary(span.unwrap())
                        .with_message("expected 'true', 'false', or an expression which can be evaluated to 'true' or 'false'")),
            PreprocessorError::BuiltinFailed { source, .. } =>
                Diagnostic::new_error(msg)
                    .with_label(Label::new_primary(span.unwrap())
                        .with_message(source.to_string())),
            PreprocessorError::BadMacroCall { def: MacroDef::String(_), reason, .. } =>
                Diagnostic::new_error(msg)
                    .with_label(Label::new_primary(span.unwrap())
                        .with_message(reason.to_owned())),
            PreprocessorError::BadMacroCall { def, reason, .. } => {
                let d = Diagnostic::new_error(msg)
                            .with_label(Label::new_primary(span.unwrap())
                                .with_message("this macro call does not match its definition"));
                let secondary_span = match def {
                    MacroDef::Static(ref define) =>
                        define.span(),
                    MacroDef::Dynamic(ref tokens) => {
                        assert!(tokens.len() > 0);
                        ByteSpan::new(
                            tokens[0].span().start(),
                            tokens.last().unwrap().span().end()
                        )
                    },
                    _ => unreachable!()
                };
                d.with_label(Label::new_secondary(secondary_span)
                    .with_message(reason.to_owned()))
            }
            //PreprocessorError::IO(_) =>
            //    Diagnostic::new_error("i/o failed")
            //        .with_label(Label::new_primary(span.unwrap())
            //            .with_message(msg)),
            PreprocessorError::InvalidTokenType { expected, .. } =>
                Diagnostic::new_error(msg)
                    .with_label(Label::new_primary(span.unwrap())
                        .with_message(format!("expected \"{}\"", expected))),
            PreprocessorError::UnexpectedToken { expected, .. } => {
                if expected.len() > 0 {
                    let expected = expected.iter()
                        .map(|t| format!("\"{}\"", t))
                        .join(", ");
                    Diagnostic::new_error(msg)
                        .with_label(Label::new_primary(span.unwrap())
                            .with_message(format!("expected one of {}", expected)))
                } else {
                    Diagnostic::new_error(msg)
                        .with_label(Label::new_primary(span.unwrap()))
                }
            }
            _ if span.is_some() =>
                Diagnostic::new_error("preprocessor error")
                    .with_label(Label::new_primary(span.unwrap()).with_message(msg)),
            _ =>
                Diagnostic::new_error(format!("preprocessor error: {}", msg)),
        }
    }
}
//impl From<LexicalError> for PreprocessorError {
//    fn from(err: LexicalError) -> PreprocessorError {
//        PreprocessorError::Lexical(err)
//    }
//}
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
            expected: err.expected.to_string()
        }
    }
}
//impl From<std::io::Error> for PreprocessorError {
//    fn from(err: std::io::Error) -> Self {
//        PreprocessorError::IO(err)
//    }
//}
//impl From<glob::GlobError> for PreprocessorError {
//    fn from(err: glob::GlobError) -> Self {
//        PreprocessorError::Diagnostic(Diagnostic::new_error(err.to_string()))
//    }
//}
//impl From<glob::PatternError> for PreprocessorError {
//    fn from(err: glob::PatternError) -> Self {
//        PreprocessorError::Diagnostic(Diagnostic::new_error(err.to_string()))
//    }
//}
impl From<Diagnostic> for PreprocessorError {
    fn from(diagnostic: Diagnostic) -> Self {
        PreprocessorError::ShowDiagnostic { diagnostic }
    }
}
