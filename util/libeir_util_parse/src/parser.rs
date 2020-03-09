use std::sync::{RwLock, Arc};
use std::borrow::Cow;
use std::path::Path;

use libeir_diagnostics::{CodeMap, FileName};

use crate::{Source, SourceError, FileMapSource};
use crate::ErrorReceiver;

pub type ArcCodemap = Arc<RwLock<CodeMap>>;

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

impl<C> Parser<C> {

    pub fn parse_string<'a, S, T>(
        &self,
        errors: &'a mut (dyn ErrorReceiver<E = <T as Parse>::Error, W = <T as Parse>::Error> + 'a),
        codemap: &ArcCodemap,
        source: S,
    ) -> Result<T, ()>
    where
        S: AsRef<str>,
        T: Parse<Config = C>,
    {
        let filemap = {
            codemap.write().unwrap().add_filemap(
                FileName::Virtual(Cow::Borrowed("nofile")),
                source.as_ref().to_owned(),
            )
        };
        <T as Parse<T>>::parse(&self.config, codemap, errors, FileMapSource::new(filemap))
    }

    pub fn parse_file<'a, P, T>(
        &self,
        errors: &'a mut (dyn ErrorReceiver<E = <T as Parse>::Error, W = <T as Parse>::Error> + 'a),
        codemap: &ArcCodemap,
        path: P,
    ) -> Result<T, ()>
    where
        P: AsRef<Path>,
        T: Parse<Config = C>,
    {
        match FileMapSource::from_path(codemap.clone(), path) {
            Err(err) => {
                errors.error(<T as Parse<T>>::file_map_error(err));
                Err(())
            },
            Ok(source) => <T as Parse<T>>::parse(&self.config, codemap, errors, source),
        }
    }

}

pub trait Parse<T = Self> {
    type Parser;
    type Error;
    type Config;
    type Token;

    fn file_map_error(err: SourceError) -> Self::Error;

    /// Initializes a token stream for the underlying parser and invokes parse_tokens
    fn parse<'a, S>(
        config: &Self::Config,
        codemap: &ArcCodemap,
        errors: &'a mut (dyn ErrorReceiver<E = Self::Error, W = Self::Error> + 'a),
        source: S,
    ) -> Result<T, ()>
    where
        S: Source;

    /// Implemented by each parser, which should parse the token stream and produce a T
    fn parse_tokens<'a, S>(
        errors: &'a mut (dyn ErrorReceiver<E = Self::Error, W = Self::Error> + 'a),
        tokens: S,
    ) -> Result<T, ()>
    where
        S: IntoIterator<Item = Self::Token>;
}
