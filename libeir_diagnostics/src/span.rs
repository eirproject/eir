use std::ops::Range;

use codespan::{ByteIndex, ByteOffset, Span};

use super::{CodeMap, SourceId, SourceIndex};

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

    pub fn new_align<F>(
        start: SourceIndex,
        end: SourceIndex,
        get_codemap: &dyn Fn(&mut dyn FnOnce(&CodeMap)),
    ) -> SourceSpan {
        let start_source = start.source_id();
        let end_source = end.source_id();

        if start_source == end_source {
            Self::new(start, end)
        } else {
            let mut result = None;
            get_codemap(&mut |codemap: &CodeMap| {
                let mut idx = start_source;
                loop {
                    if let Some(parent) = codemap.parent(idx) {
                        if idx == end_source {
                            result = Some(Self::new(parent.start(), end));
                            return;
                        }
                        idx = parent.source_id();
                    } else {
                        break;
                    }
                }

                let mut idx = end_source;
                loop {
                    if let Some(parent) = codemap.parent(idx) {
                        if idx == start_source {
                            result = Some(Self::new(start, parent.end()));
                            return;
                        }
                        idx = parent.source_id();
                    } else {
                        break;
                    }
                }
            });
            result.expect("source spans cannot be aligned!")
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
