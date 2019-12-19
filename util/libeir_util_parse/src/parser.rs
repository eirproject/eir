use std::sync::{RwLock, Arc};
use std::borrow::Cow;
use std::path::Path;

use libeir_diagnostics::{CodeMap, FileName};

use crate::{Source, SourceError, FileMapSource};

pub struct Parser<C> {
    pub config: C,
}

impl<C> Parser<C> {

    pub fn new(config: C) -> Self {
        Parser {
            config,
        }
    }

}

impl<C> Parser<C> where C: ParserConfig {

    pub fn parse_string<S, T>(&self, source: S) -> Result<T, <T as Parse>::Error>
    where
        S: AsRef<str>,
        T: Parse<Config = C>,
    {
        let codemap = self.config.codemap();
        let filemap = {
            codemap.write().unwrap().add_filemap(
                FileName::Virtual(Cow::Borrowed("nofile")),
                source.as_ref().to_owned(),
            )
        };
        <T as Parse<T>>::parse(&self.config, FileMapSource::new(filemap))
    }

    pub fn parse_file<P, T>(&self, path: P) -> Result<T, <T as Parse>::Error>
    where
        P: AsRef<Path>,
        T: Parse<Config = C>,
    {
        match FileMapSource::from_path(self.config.codemap().clone(), path) {
            Err(err) => return Err(<T as Parse<T>>::file_map_error(err)),
            Ok(source) => <T as Parse<T>>::parse(&self.config, source),
        }
    }

}

pub trait ParserConfig {
    fn codemap(&self) -> &Arc<RwLock<CodeMap>>;
}

pub trait Parse<T = Self> {
    type Parser;
    type Error;
    type Config;
    type Token;

    fn file_map_error(err: SourceError) -> Self::Error;

    /// Initializes a token stream for the underlying parser and invokes parse_tokens
    fn parse<S>(config: &Self::Config, source: S) -> Result<T, Self::Error>
    where
        S: Source;

    /// Implemented by each parser, which should parse the token stream and produce a T
    fn parse_tokens<S>(tokens: S) -> Result<T, Self::Error>
    where
        S: IntoIterator<Item = Self::Token>;
}
