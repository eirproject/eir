use libeir_diagnostics::{Diagnostic, Label, SourceIndex, SourceSpan, ToDiagnostic};

use super::expr::BinaryTypeName;
use crate::lower::strings::StringError;

use snafu::Snafu;

#[derive(Debug, Snafu)]
pub enum LowerError {
    #[snafu(display("illegal expression"))]
    IllegalExpression {
        span: SourceSpan,
    },

    /// An invalid expression occurred in a pattern
    #[snafu(display("an invalid expression occurred in a pattern"))]
    NotAllowedInPattern {
        span: SourceSpan,
    },

    /// Equality in a pattern caused two nodes to be merged.
    /// It has been shown to be unmatchable.
    #[snafu(display("patterns are fully disjoint and can never be matched"))]
    DisjointPatternUnionWarning {
        left: Option<SourceSpan>,
        right: Option<SourceSpan>,
    },
    /// Pattern is constrained by two different constant
    /// values, or is matched against an unmatchable value
    #[snafu(display("pattern can never be matched"))]
    UnmatchablePatternWarning {
        pat: Option<SourceSpan>,
        reason: Option<SourceSpan>,
    },

    /// Equality in a pattern caused two nodes to be merged,
    /// but merging these two nodes is not supported.
    /// Happens when trying to merge two binary patterns.
    #[snafu(display("patterns cannot be merged"))]
    UnsupportedPatternUnion {
        left: Option<SourceSpan>,
        right: SourceSpan,
    },

    #[snafu(display("invalid const expression in pattern"))]
    PatternConst {
        source: crate::evaluator::EvalError,
        span: SourceSpan,
    },

    /// When parsing a string, an invalid character escape
    /// was encountered.
    #[snafu(display("invalid character escape in string"))]
    InvalidStringEscape {
        span: SourceSpan,
    },

    /// Unable to resolve a variable in scope.
    #[snafu(display("could not resolve variable"))]
    UnresolvedVariable {
        span: SourceSpan,
    },

    /// Unable to bind a variable in a scope, it is already bound.
    #[snafu(display("variable was already bound in scope"))]
    AlreadyBound {
        new: SourceSpan,
        old: SourceSpan,
    },
    /// Variable binding shadowed other binding
    #[snafu(display("binding shadowed previously bound variable"))]
    ShadowingBind {
        new: SourceSpan,
        old: SourceSpan,
    },

    // Binary specifier parsing
    #[snafu(display("unknown specifier in binary entry"))]
    BinaryUnknownSpecifier {
        span: SourceSpan,
    },
    #[snafu(display("conflicting specifiers in binary entry"))]
    BinaryConflictingSpecifier {
        new: SourceSpan,
        old: SourceSpan,
    },
    #[snafu(display("invalid specifier for {} in binary entry", typ))]
    BinaryInvalidSpecifier {
        span: SourceSpan,
        typ: BinaryTypeName,
    },
    #[snafu(display("{} does not support size in binary entry", typ))]
    BinaryInvalidSize {
        span: SourceSpan,
        typ: BinaryTypeName,
    },
    #[snafu(display("binary construction can never succeed"))]
    BinaryConstructEntryTypeWarning {
        span: SourceSpan,
    },

    String {
        source: StringError,
    },

    // Records
    #[snafu(display("record field specified more than once"))]
    DuplicateRecordField {
        new: SourceSpan,
        old: SourceSpan,
    },
    #[snafu(display("record is not defined"))]
    UndefinedRecord {
        span: SourceSpan,
    },

    // Maps
    #[snafu(display("only map put (=>) allowed in map construction"))]
    MapUpdateInConstruction {
        map: SourceSpan,
        field: SourceSpan,
    },
    #[snafu(display("map update performed on a non map"))]
    MapUpdateOnNonMap {
        map: SourceSpan,
    },
}

impl From<StringError> for LowerError {
    fn from(err: StringError) -> LowerError {
        LowerError::String { source: err }
    }
}

impl ToDiagnostic for LowerError {
    fn to_diagnostic(&self) -> Diagnostic {
        let msg = self.to_string();
        match self {
            LowerError::IllegalExpression { span } => Diagnostic::error()
                .with_message(msg)
                .with_labels(vec![
                    Label::primary(span.source_id(), *span).with_message("illegal expression")
                ]),
            LowerError::NotAllowedInPattern { span } => Diagnostic::error()
                .with_message(msg)
                .with_labels(vec![Label::primary(span.source_id(), *span)
                    .with_message("disallowed expression in pattern")]),
            LowerError::DisjointPatternUnionWarning { left, right } => {
                let dig = Diagnostic::warning().with_message(msg);
                let mut labels = vec![];
                if let Some(left) = left {
                    labels
                        .push(Label::primary(left.source_id(), *left).with_message("left pattern"));
                }
                if let Some(right) = right {
                    labels.push(
                        Label::primary(right.source_id(), *right).with_message("right pattern"),
                    );
                }
                dig.with_labels(labels)
            }
            LowerError::UnmatchablePatternWarning { pat, reason } => {
                let dig = Diagnostic::warning().with_message(msg);
                let mut labels = vec![];
                if let Some(pat) = pat {
                    labels.push(
                        Label::primary(pat.source_id(), *pat)
                            .with_message("pattern can never be matched"),
                    );
                }
                if let Some(reason) = reason {
                    labels.push(
                        Label::primary(reason.source_id(), *reason)
                            .with_message("matched against this"),
                    );
                }
                dig.with_labels(labels)
            }
            LowerError::UnsupportedPatternUnion { left, right } => {
                let dig = Diagnostic::warning().with_message(msg);
                let mut labels = vec![];
                if let Some(left) = left {
                    labels
                        .push(Label::primary(left.source_id(), *left).with_message("left pattern"));
                }
                labels
                    .push(Label::primary(right.source_id(), *right).with_message("right pattern"));
                dig.with_labels(labels)
            }
            LowerError::PatternConst { source, .. } => source.to_diagnostic(),
            LowerError::InvalidStringEscape { span } => Diagnostic::error()
                .with_message(msg)
                .with_labels(vec![
                    Label::primary(span.source_id(), *span).with_message("invalid string escape")
                ]),
            LowerError::UnresolvedVariable { span } => Diagnostic::error()
                .with_message(msg)
                .with_labels(vec![
                    Label::primary(span.source_id(), *span).with_message("not bound in scope")
                ]),
            LowerError::AlreadyBound { new, old } => {
                Diagnostic::error().with_message(msg).with_labels(vec![
                    Label::primary(new.source_id(), *new)
                        .with_message("variable was already bound in scope"),
                    Label::secondary(old.source_id(), *old).with_message("previously bound here"),
                ])
            }
            LowerError::ShadowingBind { new, old } => {
                Diagnostic::warning().with_message(msg).with_labels(vec![
                    Label::primary(new.source_id(), *new)
                        .with_message("variable was already bound in scope"),
                    Label::secondary(old.source_id(), *old).with_message("previously bound here"),
                ])
            }
            LowerError::BinaryUnknownSpecifier { span } => Diagnostic::error()
                .with_message(msg)
                .with_labels(vec![
                    Label::primary(span.source_id(), *span).with_message("specifier is not known")
                ]),
            LowerError::BinaryConflictingSpecifier { new, old } => {
                Diagnostic::error().with_message(msg).with_labels(vec![
                    Label::primary(new.source_id(), *new).with_message("specifier 1"),
                    Label::primary(old.source_id(), *old).with_message("specifier 2"),
                ])
            }
            LowerError::BinaryInvalidSpecifier { span, typ } => Diagnostic::error()
                .with_message(msg)
                .with_labels(vec![Label::primary(span.source_id(), *span)
                    .with_message(format!("specifier is not valid for {} entries", typ))]),
            LowerError::BinaryInvalidSize { span, typ } => Diagnostic::error()
                .with_message(msg)
                .with_labels(vec![Label::primary(span.source_id(), *span).with_message(
                    format!("size specifier not valid for {} binary entries", typ),
                )]),
            LowerError::BinaryConstructEntryTypeWarning { span } => Diagnostic::error()
                .with_message(msg)
                .with_labels(vec![
                    Label::primary(span.source_id(), *span).with_message("can never succeed")
                ]),
            LowerError::String { source } => source.to_diagnostic(),
            LowerError::DuplicateRecordField { new, old } => {
                Diagnostic::error().with_message(msg).with_labels(vec![
                    Label::primary(new.source_id(), *old)
                        .with_message("duplicate field definition in record"),
                    Label::secondary(old.source_id(), *old).with_message("previously defined here"),
                ])
            }
            LowerError::UndefinedRecord { span } => Diagnostic::error()
                .with_message(msg)
                .with_labels(vec![Label::primary(span.source_id(), *span)]),
            LowerError::MapUpdateInConstruction { map, field } => {
                Diagnostic::error().with_message(msg).with_labels(vec![
                    Label::primary(field.source_id(), *field)
                        .with_message("map update (:=) attempted"),
                    Label::secondary(map.source_id(), *map)
                        .with_message("expression is map construction"),
                ])
            }
            LowerError::MapUpdateOnNonMap { map } => Diagnostic::warning()
                .with_message(msg)
                .with_labels(vec![Label::primary(map.source_id(), *map).with_message(
                    "updated value is not a map, this will fail at runtime",
                )]),
        }
    }
}
