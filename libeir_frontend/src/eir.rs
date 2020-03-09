use std::path::Path;
use std::borrow::Cow;

use libeir_diagnostics::{Diagnostic, FileName};
use libeir_util_parse::{Parse, ArcCodemap, Source, FileMapSource, ToDiagnostic, error_tee};
use libeir_ir::{
    Module,
    text::{
        parser::ParserError,
        ast::{LowerError, Module as ModuleAst},
    },
};

use super::{Frontend, FrontendErrorReceiver};

pub enum Error {
    Parser(ParserError),
    Lower(LowerError),
}
impl ToDiagnostic for Error {
    fn to_diagnostic(&self) -> Diagnostic {
        match self {
            Error::Parser(err) => err.to_diagnostic(),
            Error::Lower(err) => err.to_diagnostic(),
        }
    }
}
impl From<ParserError> for Error {
    fn from(err: ParserError) -> Self {
        Error::Parser(err)
    }
}
impl From<LowerError> for Error {
    fn from(err: LowerError) -> Self {
        Error::Lower(err)
    }
}

pub struct EirFrontend {}

impl EirFrontend {
    pub fn new() -> Self {
        EirFrontend {}
    }
}

impl Frontend for EirFrontend {
    type Error = Error;

    fn parse_source<'a>(
        &self,
        errors: &'a mut FrontendErrorReceiver<'a, Self::Error>,
        codemap: ArcCodemap,
        source: FileMapSource,
    ) -> Result<Module, ()> {
        error_tee(errors, |mut errors| {
            let ast = ModuleAst::parse(&(), &codemap, &mut errors.make_into_adapter(), source)?;
            let eir = ast.lower(&mut errors.make_into_adapter())?;
            Ok(eir)
        })
    }

    fn parse_string<'a>(
        &self,
        errors: &'a mut FrontendErrorReceiver<'a, Self::Error>,
        codemap: ArcCodemap,
        source: &str,
    ) -> Result<Module, ()> {
        let filemap = {
            codemap.write().unwrap().add_filemap(
                FileName::Virtual(Cow::Borrowed("nofile")),
                source.to_owned(),
            )
        };
        self.parse_source(errors, codemap, FileMapSource::new(filemap))
    }

    fn parse_file<'a>(
        &self,
        errors: &'a mut FrontendErrorReceiver<'a, Self::Error>,
        codemap: ArcCodemap,
        source: &Path,
    ) -> Result<Module, ()> {
        match FileMapSource::from_path(codemap.clone(), source) {
            Err(err) => {
                errors.error(<ModuleAst as Parse<ModuleAst>>::file_map_error(err).into());
                Err(())
            },
            Ok(source) => self.parse_source(errors, codemap, source),
        }
    }

}

impl_dyn_frontend!(EirFrontend);
