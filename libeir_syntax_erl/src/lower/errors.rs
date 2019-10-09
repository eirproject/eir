use libeir_diagnostics::{ ByteSpan, Diagnostic, Label };

use super::expr::BinaryTypeName;

use snafu::Snafu;

#[derive(Debug, Snafu)]
pub enum LowerError {

    /// An invalid expression occurred in a pattern
    #[snafu(display("an invalid expression occurred in a pattern"))]
    NotAllowedInPattern { span: ByteSpan },

    /// Equality in a pattern caused two nodes to be merged.
    /// It has been shown to be unmatchable.
    #[snafu(display("patterns are fully disjoint and can never be matched"))]
    DisjointPatternUnionWarning { left: Option<ByteSpan>, right: Option<ByteSpan> },
    /// Pattern is constrained by two different constant
    /// values, or is matched against an unmatchable value
    #[snafu(display("pattern can never be matched"))]
    UnmatchablePatternWarning { pat: Option<ByteSpan>, reason: Option<ByteSpan> },

    /// Equality in a pattern caused two nodes to be merged,
    /// but merging these two nodes is not supported.
    /// Happens when trying to merge two binary patterns.
    #[snafu(display("patterns cannot be merged"))]
    UnsupportedPatternUnion { left: Option<ByteSpan>, right: ByteSpan },

    /// When parsing a string, an invalid character escape
    /// was encountered.
    #[snafu(display("invalid character escape in string"))]
    InvalidStringEscape { span: ByteSpan },

    /// Unable to resolve a variable in scope.
    #[snafu(display("could not resolve variable"))]
    UnresolvedVariable { span: ByteSpan },

    /// Unable to bind a variable in a scope, it is already bound.
    #[snafu(display("variable was already bound in scope"))]
    AlreadyBound { new: ByteSpan, old: ByteSpan },
    /// Variable binding shadowed other binding
    #[snafu(display("binding shadowed previously bound variable"))]
    ShadowingBind { new: ByteSpan, old: ByteSpan },

    // Binary specifier parsing
    #[snafu(display("unknown specifier in binary entry"))]
    BinaryUnknownSpecifier { span: ByteSpan },
    #[snafu(display("conflicting specifiers in binary entry"))]
    BinaryConflictingSpecifier { new: ByteSpan, old: ByteSpan },
    #[snafu(display("invalid specifier for type in binary entry"))]
    BinaryInvalidSpecifier { span: ByteSpan, typ: BinaryTypeName },
    #[snafu(display("type does not support size in binary entry"))]
    BinaryInvalidSize { span: ByteSpan, typ: BinaryTypeName },

    // Records
    #[snafu(display("record field specified more than once"))]
    DuplicateRecordField { new: ByteSpan, old: ByteSpan },
    #[snafu(display("record is not defined"))]
    UndefinedRecord { span: ByteSpan },

}

impl LowerError {

    pub fn to_diagnostic(&self) -> Diagnostic {
        let msg = self.to_string();
        match self {
            LowerError::NotAllowedInPattern { span } => {
                Diagnostic::new_error(msg)
                    .with_label(
                        Label::new_primary(*span)
                            .with_message("disallowed expression in pattern")
                    )
            }
            LowerError::DisjointPatternUnionWarning { left, right } => {
                let mut dig = Diagnostic::new_warning(msg);
                if let Some(left) = left {
                    dig = dig.with_label(
                        Label::new_primary(*left)
                            .with_message("left pattern")
                    );
                }
                if let Some(right) = right {
                    dig = dig.with_label(
                        Label::new_primary(*right)
                            .with_message("right pattern")
                    );
                }
                dig
            }
            LowerError::UnsupportedPatternUnion { left, right } => {
                let mut dig = Diagnostic::new_warning(msg);
                if let Some(left) = left {
                    dig = dig
                        .with_label(
                            Label::new_primary(*left)
                                .with_message("left pattern")
                        );
                }
                dig.with_label(
                    Label::new_primary(*right)
                        .with_message("right pattern")
                )
            }
            LowerError::InvalidStringEscape { span } => {
                Diagnostic::new_error(msg)
                    .with_label(
                        Label::new_primary(*span)
                            .with_message("invalid string escape")
                    )
            }
            LowerError::UnresolvedVariable { span } => {
                Diagnostic::new_error(msg)
                    .with_label(
                        Label::new_primary(*span)
                            .with_message("not bound in scope")
                    )
            }
            LowerError::AlreadyBound { new, old } => {
                Diagnostic::new_error(msg)
                    .with_label(
                        Label::new_primary(*new)
                            .with_message("variable was already bound in scope")
                    )
                    .with_label(
                        Label::new_secondary(*old)
                            .with_message("previously bound here")
                    )
            }
            LowerError::ShadowingBind { new, old } => {
                Diagnostic::new_warning(msg)
                    .with_label(
                        Label::new_primary(*new)
                            .with_message("variable was already bound in scope")
                    )
                    .with_label(
                        Label::new_secondary(*old)
                            .with_message("previously bound here")
                    )
            }
            _ => unimplemented!()
        }
    }

}
