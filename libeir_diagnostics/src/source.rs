use std::convert::Into;
use std::num::NonZeroU32;
use std::ops::Range;

use super::*;

/// A handle that points to a file in the database.
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SourceId(crate NonZeroU32);
impl SourceId {
    crate const UNKNOWN_SOURCE_ID: u32 = u32::max_value();

    pub const UNKNOWN: Self = Self(unsafe { NonZeroU32::new_unchecked(Self::UNKNOWN_SOURCE_ID) });

    crate fn new(index: u32) -> Self {
        assert!(index > 0);
        assert!(index < Self::UNKNOWN_SOURCE_ID);
        Self(NonZeroU32::new(index).unwrap())
    }

    #[inline]
    crate fn get(self) -> u32 {
        self.0.get()
    }
}

/// The representation of a source file in the database.
#[derive(Debug, Clone)]
pub struct SourceFile {
    id: SourceId,
    name: FileName,
    source: String,
    line_starts: Vec<ByteIndex>,
}
impl SourceFile {
    crate fn new(id: SourceId, name: FileName, source: String) -> Self {
        let line_starts = codespan_reporting::files::line_starts(source.as_str())
            .map(|i| ByteIndex::from(i as u32))
            .collect();

        Self {
            id,
            name,
            source,
            line_starts,
        }
    }

    pub fn name(&self) -> &FileName {
        &self.name
    }

    pub fn id(&self) -> SourceId {
        self.id
    }

    pub fn line_start(
        &self,
        line_index: LineIndex,
    ) -> Result<ByteIndex, LineIndexOutOfBoundsError> {
        use std::cmp::Ordering;

        match line_index.cmp(&self.last_line_index()) {
            Ordering::Less => Ok(self.line_starts[line_index.to_usize()]),
            Ordering::Equal => Ok(self.source_span().end_index()),
            Ordering::Greater => Err(LineIndexOutOfBoundsError {
                given: line_index,
                max: self.last_line_index(),
            }),
        }
    }

    pub fn last_line_index(&self) -> LineIndex {
        LineIndex::from(self.line_starts.len() as RawIndex)
    }

    pub fn line_span(&self, line_index: LineIndex) -> Result<Span, LineIndexOutOfBoundsError> {
        let line_start = self.line_start(line_index)?;
        let next_line_start = self.line_start(line_index + LineOffset::from(1))?;

        Ok(Span::new(line_start, next_line_start))
    }

    pub fn line_index(&self, byte_index: ByteIndex) -> LineIndex {
        match self.line_starts.binary_search(&byte_index) {
            // Found the start of a line
            Ok(line) => LineIndex::from(line as u32),
            Err(next_line) => LineIndex::from(next_line as u32 - 1),
        }
    }

    pub fn location(&self, byte_index: ByteIndex) -> Result<Location, LocationError> {
        let line_index = self.line_index(byte_index);
        let line_start_index =
            self.line_start(line_index)
                .map_err(|_| LocationError::OutOfBounds {
                    given: byte_index,
                    span: self.source_span().as_span(),
                })?;
        let line_src = self
            .source
            .as_str()
            .get(line_start_index.to_usize()..byte_index.to_usize())
            .ok_or_else(|| {
                let given = byte_index;
                if given >= self.source_span().end_index() {
                    let span = self.source_span();
                    LocationError::OutOfBounds {
                        given,
                        span: span.as_span(),
                    }
                } else {
                    LocationError::InvalidCharBoundary { given }
                }
            })?;

        Ok(Location {
            line: line_index,
            column: ColumnIndex::from(line_src.chars().count() as u32),
        })
    }

    #[inline(always)]
    pub fn source(&self) -> &str {
        self.source.as_str()
    }

    pub fn source_span(&self) -> SourceSpan {
        SourceSpan {
            source_id: self.id,
            start: ByteIndex(0),
            end: ByteIndex(self.source.len() as u32),
        }
    }

    pub fn source_slice(
        &self,
        span: impl Into<Range<usize>>,
    ) -> Result<&str, SpanOutOfBoundsError> {
        let span = span.into();
        let start = span.start;
        let end = span.end;

        self.source().get(start..end).ok_or_else(|| {
            let given = Span::new(start as u32, end as u32);
            let span = Span::from_str(self.source());
            SpanOutOfBoundsError { given, span }
        })
    }
}
