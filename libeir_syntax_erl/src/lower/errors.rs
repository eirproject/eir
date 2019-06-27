use libeir_diagnostics::{ ByteSpan, Diagnostic, Label };

use super::expr::BinaryTypeName;

use failure::{ Error, Fail };

#[derive(Fail, Debug)]
pub enum LowerError {

    /// An invalid expression occurred in a pattern
    #[fail(display = "an invalid expression occurred in a pattern")]
    NotAllowedInPattern { span: ByteSpan },

    /// Equality in a pattern caused two nodes to be merged.
    /// It has been shown to be unmatchable.
    #[fail(display = "patterns are fully disjoint and can never be matched")]
    DisjointPatternUnionWarning { left: ByteSpan, right: ByteSpan },

    /// Equality in a pattern caused two nodes to be merged,
    /// but merging these two nodes is not supported.
    /// Happens when trying to merge two binary patterns.
    #[fail(display = "patterns cannot be merged")]
    UnsupportedPatternUnion { left: ByteSpan, right: ByteSpan },

    /// When parsing a string, an invalid character escape
    /// was encountered.
    #[fail(display = "invalid character escape in string")]
    InvalidStringEscape { span: ByteSpan },

    /// Unable to resolve a variable in scope.
    #[fail(display = "could not resolve variable")]
    UnresolvedVariable { span: ByteSpan },

    /// Unable to bind a variable in a scope, it is already bound.
    #[fail(display = "variable was already bound in scope")]
    AlreadyBound { new: ByteSpan, old: ByteSpan },

    // Binary specifier parsing
    #[fail(display = "unknown specifier in binary entry")]
    BinaryUnknownSpecifier { span: ByteSpan },
    #[fail(display = "conflicting specifiers in binary entry")]
    BinaryConflictingSpecifier { new: ByteSpan, old: ByteSpan },
    #[fail(display = "invalid specifier for type in binary entry")]
    BinaryInvalidSpecifier { span: ByteSpan, typ: BinaryTypeName },
    #[fail(display = "type does not support size in binary entry")]
    BinaryInvalidSize { span: ByteSpan, typ: BinaryTypeName }

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
                Diagnostic::new_warning(msg)
                    .with_label(
                        Label::new_primary(*left)
                            .with_message("left pattern")
                    )
                    .with_label(
                        Label::new_primary(*right)
                            .with_message("right pattern")
                    )
            }
            LowerError::UnsupportedPatternUnion { left, right } => {
                Diagnostic::new_error(msg)
                    .with_label(
                        Label::new_primary(*left)
                            .with_message("left pattern")
                    )
                    .with_label(
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
            _ => unimplemented!()
        }
    }

}
