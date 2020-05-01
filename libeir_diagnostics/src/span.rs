use std::ops::Range;

use codespan::{ByteIndex, ByteOffset, Span};

use super::{SourceId, SourceIndex};

#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct SourceSpan {
    crate source_id: SourceId,
    crate start: ByteIndex,
    crate end: ByteIndex,
}
impl SourceSpan {
    pub const UNKNOWN: Self = Self {
        source_id: SourceId::UNKNOWN,
        start: ByteIndex(0),
        end: ByteIndex(0),
    };

    #[inline]
    pub fn new(start: SourceIndex, end: SourceIndex) -> Self {
        let source_id = start.source_id();
        assert_eq!(
            source_id,
            end.source_id(),
            "source spans cannot start and end in different files!"
        );
        let start = start.index();
        let end = end.index();

        Self {
            source_id,
            start,
            end,
        }
    }

    #[inline(always)]
    pub fn source_id(&self) -> SourceId {
        self.source_id
    }

    #[inline(always)]
    pub fn start(&self) -> SourceIndex {
        SourceIndex::new(self.source_id, self.start)
    }

    #[inline(always)]
    pub fn start_index(&self) -> ByteIndex {
        self.start
    }

    pub fn shrink_front(mut self, offset: ByteOffset) -> Self {
        self.start += offset;
        self
    }

    #[inline(always)]
    pub fn end(&self) -> SourceIndex {
        SourceIndex::new(self.source_id, self.end)
    }

    #[inline(always)]
    pub fn end_index(&self) -> ByteIndex {
        self.end
    }

    pub fn as_span(&self) -> Span {
        Span::new(self.start, self.end)
    }
}

impl From<SourceSpan> for Range<usize> {
    fn from(span: SourceSpan) -> Range<usize> {
        span.start.into()..span.end.into()
    }
}

impl From<SourceSpan> for Range<SourceIndex> {
    fn from(span: SourceSpan) -> Range<SourceIndex> {
        let start = SourceIndex::new(span.source_id, span.start);
        let end = SourceIndex::new(span.source_id, span.end);
        start..end
    }
}
