use std::path::{Path, PathBuf};
use std::sync::Arc;

use libeir_diagnostics::*;

use crate::ErrorReceiver;
use crate::{FileMapSource, Source, SourceError};

pub struct Parser<C> {
    pub config: C,
    pub codemap: Arc<CodeMap>,
}

impl<C> Parser<C> {
    pub fn new(config: C, codemap: Arc<CodeMap>) -> Self {
        Self { config, codemap }
    }
}

impl<C> Parser<C> {
    pub fn parse<'a, T>(
        &self,
        errors: &'a mut (dyn ErrorReceiver<E = <T as Parse>::Error, W = <T as Parse>::Error> + 'a),
        source: Arc<SourceFile>,
    ) -> Result<T, ()>
    where
        T: Parse<Config = C>,
    {
        <T as Parse<T>>::parse(&self, errors, FileMapSource::new(source))
    }

    pub fn parse_string<'a, T, S>(
        &self,
        errors: &'a mut (dyn ErrorReceiver<E = <T as Parse>::Error, W = <T as Parse>::Error> + 'a),
        source: S,
    ) -> Result<T, ()>
    where
        T: Parse<Config = C>,
        S: AsRef<str>,
    {
        let id = self.codemap.add("nofile", source.as_ref().to_string());
        let file = self.codemap.get(id).unwrap();
        self.parse(errors, file)
    }

    pub fn parse_file<'a, T, S>(
        &self,
        errors: &'a mut (dyn ErrorReceiver<E = <T as Parse>::Error, W = <T as Parse>::Error> + 'a),
        source: S,
    ) -> Result<T, ()>
    where
        T: Parse<Config = C>,
        S: AsRef<Path>,
    {
        let path = source.as_ref();
        match std::fs::read_to_string(path) {
            Err(err) => {
                errors.error(<T as Parse<T>>::root_file_error(err, path.to_owned()));
                Err(())
            }
            Ok(content) => {
                let id = self.codemap.add(path, content);
                let file = self.codemap.get(id).unwrap();
                self.parse(errors, file)
            }
        }
    }
}

pub trait Parse<T = Self> {
    type Parser;
    type Error;
    type Config;
    type Token;

    fn root_file_error(err: std::io::Error, path: PathBuf) -> Self::Error;

    /// Initializes a token stream for the underlying parser and invokes parse_tokens
    fn parse<'a, S>(
        parser: &Parser<Self::Config>,
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
