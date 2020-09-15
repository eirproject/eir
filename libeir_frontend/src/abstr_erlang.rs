use std::path::Path;
use std::sync::Arc;

use libeir_diagnostics::*;
use libeir_ir::Module;
use libeir_syntax_erl::{lower_abstr, lower_module, LowerError};
use libeir_util_parse::{error_tee, Parse, Parser};
use libeir_util_parse_listing::{ast::Root, parser::ParseError};

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

pub struct AbstrErlangFrontend {
    parser: Parser<()>,
}
impl AbstrErlangFrontend {
    pub fn new(codemap: Arc<CodeMap>) -> Self {
        Self {
            parser: Parser::new((), codemap),
        }
    }
}
impl Frontend for AbstrErlangFrontend {
    type Error = Error;

    fn parse_source<'a>(
        &self,
        errors: &'a mut FrontendErrorReceiver<'a, Self::Error>,
        source: Arc<SourceFile>,
    ) -> Result<Module, ()> {
        error_tee(errors, |mut errors| {
            let root = self
                .parser
                .parse::<Root>(&mut errors.make_into_adapter(), source)?;
            let ast = lower_abstr(&root);
            let eir = lower_module(
                &mut errors.make_into_adapter(),
                self.parser.codemap.clone(),
                &ast,
            )?;
            Ok(eir)
        })
    }

    fn parse_string<'a>(
        &self,
        errors: &'a mut FrontendErrorReceiver<'a, Self::Error>,
        source: &str,
    ) -> Result<Module, ()> {
        let id = self.parser.codemap.add("nofile", source.to_string());
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
                errors.error(<Root as Parse<Root>>::root_file_error(err, path.to_owned()).into());
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
