use std::ops::Range;
use std::path::PathBuf;
use std::sync::atomic::{AtomicU32, Ordering};
use std::sync::Arc;

use dashmap::DashMap;

use super::*;

#[derive(Debug)]
pub struct CodeMap {
    files: DashMap<SourceId, Arc<SourceFile>>,
    seen: DashMap<PathBuf, SourceId>,
    next_file_id: AtomicU32,
}
impl CodeMap {
    /// Creates an empty `CodeMap`.
    pub fn new() -> Self {
        Self {
            files: DashMap::new(),
            seen: DashMap::new(),
            next_file_id: AtomicU32::new(1),
        }
    }

    /// Add a file to the map, returning the handle that can be used to
    /// refer to it again.
    pub fn add(&self, name: impl Into<FileName>, source: String) -> SourceId {
        // De-duplicate real files on add; it _may_ be possible for concurrent
        // adds to add the same file more than once, since we're working across
        // two maps; but since DashMap uses read/write locks internally to lock
        // buckets, the sequence of locks required here should prevent that from
        // happening
        //
        // We don't de-duplicate virtual files, because the same name could be used
        // for different content, and its unlikely that we'd be adding the same content
        // over and over again with the same virtual file name
        let name = name.into();
        if let FileName::Real(ref path) = name {
            let seen_ref = self
                .seen
                .entry(path.clone())
                .or_insert_with(|| self.insert_file(name, source, None));
            *seen_ref.value()
        } else {
            self.insert_file(name, source, None)
        }
    }

    /// Add a file to the map with the given source span as a parent.
    /// This will not deduplicate the file in the map.
    pub fn add_child(
        &self,
        name: impl Into<FileName>,
        source: String,
        parent: SourceSpan,
    ) -> SourceId {
        self.insert_file(name.into(), source, Some(parent))
    }

    fn insert_file(&self, name: FileName, source: String, parent: Option<SourceSpan>) -> SourceId {
        let file_id = self.next_file_id();
        self.files.insert(
            file_id,
            Arc::new(SourceFile::new(file_id, name.into(), source, parent)),
        );
        file_id
    }

    /// Get the file corresponding to the given id.
    pub fn get(&self, file_id: SourceId) -> Option<Arc<SourceFile>> {
        if file_id == SourceId::UNKNOWN {
            None
        } else {
            self.files.get(&file_id).map(|r| r.value().clone())
        }
    }

    pub fn parent(&self, file_id: SourceId) -> Option<SourceSpan> {
        self.get(file_id).and_then(|f| f.parent())
    }

    pub fn name(&self, file_id: SourceId) -> Option<FileName> {
        self.get(file_id).map(|f| f.name().clone())
    }

    pub fn iter<'a>(&'a self) -> impl Iterator<Item = Arc<SourceFile>> + 'a {
        self.files.iter().map(|r| r.value().clone())
    }

    pub fn line_span(
        &self,
        file_id: SourceId,
        line_index: impl Into<LineIndex>,
    ) -> Option<Result<Span, LineIndexOutOfBoundsError>> {
        let f = self.get(file_id)?;
        Some(f.line_span(line_index.into()))
    }

    pub fn line_index(
        &self,
        file_id: SourceId,
        byte_index: impl Into<ByteIndex>,
    ) -> Option<LineIndex> {
        let f = self.get(file_id)?;
        Some(f.line_index(byte_index.into()))
    }

    pub fn location(
        &self,
        file_id: SourceId,
        byte_index: impl Into<ByteIndex>,
    ) -> Option<Result<Location, LocationError>> {
        let f = self.get(file_id)?;
        Some(f.location(byte_index.into()))
    }

    pub fn source_span(&self, file_id: SourceId) -> Option<SourceSpan> {
        let f = self.get(file_id)?;
        Some(f.source_span())
    }

    pub fn source_slice<'a>(
        &'a self,
        file_id: SourceId,
        span: impl Into<Span>,
    ) -> Option<Result<&'a str, SpanOutOfBoundsError>> {
        let f = self.get(file_id)?;
        match f.source_slice(span.into()) {
            Err(err) => Some(Err(err)),
            Ok(slice) => unsafe { Some(Ok(std::mem::transmute::<&str, &'a str>(slice))) },
        }
    }

    #[inline(always)]
    fn next_file_id(&self) -> SourceId {
        let id = self.next_file_id.fetch_add(1, Ordering::Relaxed);
        SourceId::new(id)
    }
}
impl Default for CodeMap {
    fn default() -> Self {
        Self::new()
    }
}
impl<'a> Files<'a> for CodeMap {
    type FileId = SourceId;
    type Name = String;
    type Source = &'a str;

    fn name(&self, file_id: Self::FileId) -> Option<Self::Name> {
        Some(format!("{}", self.get(file_id)?.name()))
    }

    fn source(&self, file_id: Self::FileId) -> Option<&'a str> {
        use std::mem;

        let f = self.get(file_id)?;
        Some(unsafe { mem::transmute::<&str, &'a str>(f.source()) })
    }

    fn line_index(&self, file_id: Self::FileId, byte_index: usize) -> Option<usize> {
        Some(self.line_index(file_id, byte_index as u32)?.to_usize())
    }

    fn line_range(&self, file_id: Self::FileId, line_index: usize) -> Option<Range<usize>> {
        let span = self.line_span(file_id, line_index as u32)?.ok()?;

        Some(span.start().to_usize()..span.end().to_usize())
    }
}
