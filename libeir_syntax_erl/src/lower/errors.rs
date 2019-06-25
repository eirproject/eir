use libeir_diagnostics::{ ByteSpan };

pub enum LowerError {
    /// An invalid expression occurred in a pattern
    NotAllowedInPattern { span: ByteSpan },

    /// Equality in a pattern caused two nodes to be merged.
    /// It has been shown to be unmatchable.
    DisjointPatternUnionWarning { left: ByteSpan, right: ByteSpan },

    /// Equality in a pattern caused two nodes to be merged,
    /// but merging these two nodes is not supported.
    /// Happens when trying to merge two binary patterns.
    UnsupportedPatternUnion { left: ByteSpan, right: ByteSpan },

    /// When parsing a string, an invalid character escape
    /// was encountered.
    InvalidStringEscape { span: ByteSpan },

    /// Unable to resolve a variable in scope.
    UnresolvedVariable { span: ByteSpan },

    /// Unable to bind a variable in a scope, it is already bound.
    AlreadyBound { new: ByteSpan, old: ByteSpan },

}

