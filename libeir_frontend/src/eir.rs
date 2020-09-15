use std::path::Path;
use std::sync::Arc;

use libeir_diagnostics::*;
use libeir_ir::{
    text::{ast::Module as ModuleAst, parser::ParserError, LowerError},
    Module,
};
use libeir_util_parse::{error_tee, Parse, Parser};

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

pub struct EirFrontend {
    parser: Parser<()>,
}
impl EirFrontend {
    pub fn new(codemap: Arc<CodeMap>) -> Self {
        Self {
            parser: Parser::new((), codemap),
        }
    }
}
impl Frontend for EirFrontend {
    type Error = Error;

    fn parse_source<'a>(
        &self,
        errors: &'a mut FrontendErrorReceiver<'a, Self::Error>,
        source: Arc<SourceFile>,
    ) -> Result<Module, ()> {
        error_tee(errors, |mut errors| {
            let ast = self
                .parser
                .parse::<ModuleAst>(&mut errors.make_into_adapter(), source)?;
            let eir = ast.lower(&mut errors.make_into_adapter())?;
            Ok(eir)
        })
    }

    fn parse_string<'a>(
        &self,
        errors: &'a mut FrontendErrorReceiver<'a, Self::Error>,
        source: &str,
    ) -> Result<Module, ()> {
        let id = self.parser.codemap.add("nofile", source.to_owned());
        let file = self.parser.codemap.get(id).unwrap();
        self.parse_source(errors, file)
    }

    fn parse_file<'a>(
        &self,
        errors: &'a mut FrontendErrorReceiver<'a, Self::Error>,
        path: &Path,
    ) -> Result<Module, ()> {
        match std::fs::read_to_string(path) {
            Err(err) => {
                errors.error(<ModuleAst as Parse<ModuleAst>>::root_file_error(err, path.to_owned()).into());
                Err(())
            }
            Ok(content) => {
                let id = self.parser.codemap.add(path, content);
                let file = self.parser.codemap.get(id).unwrap();
                self.parse_source(errors, file)
            }
        }
    }
}
