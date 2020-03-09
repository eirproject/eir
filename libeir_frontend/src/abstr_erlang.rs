use std::path::Path;
use std::borrow::Cow;

use libeir_util_parse_listing::{parser::ParseError, ast::Root};
use libeir_util_parse::{Parse, ArcCodemap, Source, FileMapSource, ToDiagnostic, error_tee};
use libeir_ir::Module;
use libeir_diagnostics::{Diagnostic, FileName};
use libeir_syntax_erl::{lower_module, LowerError, lower_abstr};

use super::{Frontend, FrontendErrorReceiver};

pub enum Error {
    Parse(ParseError),
    Lower(LowerError),
}
impl ToDiagnostic for Error {
    fn to_diagnostic(&self) -> Diagnostic {
        match self {
            Error::Parse(err) => err.to_diagnostic(),
            Error::Lower(err) => err.to_diagnostic(),
        }
    }
}
impl From<ParseError> for Error {
    fn from(err: ParseError) -> Error {
        Error::Parse(err)
    }
}
impl From<LowerError> for Error {
    fn from(err: LowerError) -> Error {
        Error::Lower(err)
    }
}

pub struct AbstrErlangFrontend {}

impl AbstrErlangFrontend {
    pub fn new() -> Self {
        AbstrErlangFrontend {}
    }
}

impl Frontend for AbstrErlangFrontend {
    type Error = Error;

    fn parse_source<'a>(
        &self,
        errors: &'a mut FrontendErrorReceiver<'a, Self::Error>,
        codemap: ArcCodemap,
        source: FileMapSource,
    ) -> Result<Module, ()> {
        error_tee(errors, |mut errors| {
            let root: Root = Root::parse(&(), &codemap, &mut errors.make_into_adapter(), source)?;
            let ast = lower_abstr(&root);
            let eir = lower_module(&mut errors.make_into_adapter(), &codemap, &ast)?;
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
                errors.error(<Root as Parse<Root>>::file_map_error(err).into());
                Err(())
            },
            Ok(source) => self.parse_source(errors, codemap, source),
        }
    }

}

impl_dyn_frontend!(AbstrErlangFrontend);
