#![feature(crate_visibility_modifier)]

mod codemap;
mod filename;
mod index;
mod source;
mod span;

pub use codespan::{ByteIndex, ByteOffset};
pub use codespan::{ColumnIndex, ColumnNumber, ColumnOffset};
pub use codespan::{Index, Offset};
pub use codespan::{LineIndex, LineNumber, LineOffset};
pub use codespan::{LineIndexOutOfBoundsError, LocationError, SpanOutOfBoundsError};
pub use codespan::{Location, Span};
pub use codespan::{RawIndex, RawOffset};

pub use codespan_reporting::diagnostic::{Label, LabelStyle, Severity};
pub use codespan_reporting::files::Files;
pub use codespan_reporting::term;

pub use self::codemap::CodeMap;
pub use self::filename::FileName;
pub use self::index::SourceIndex;
pub use self::source::{SourceFile, SourceId};
pub use self::span::SourceSpan;

pub type Diagnostic = codespan_reporting::diagnostic::Diagnostic<SourceId>;

pub trait ToDiagnostic {
    fn to_diagnostic(&self) -> Diagnostic;
}
