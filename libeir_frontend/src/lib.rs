#[cfg(feature = "frontend_abstr_erlang")]
pub mod abstr_erlang;
#[cfg(feature = "frontend_eir")]
pub mod eir;
#[cfg(feature = "frontend_erlang")]
pub mod erlang;

use std::path::Path;
use std::sync::Arc;

use libeir_diagnostics::{Diagnostic, ToDiagnostic, SourceFile};
use libeir_ir::Module;
use libeir_util_parse::ErrorReceiver;

pub type FrontendErrorReceiver<'a, E> = dyn ErrorReceiver<E = E, W = E> + 'a;

pub trait DynFrontend {
    fn parse_source_dyn<'a>(&self, source: Arc<SourceFile>) -> (Result<Module, ()>, Vec<Diagnostic>);

    fn parse_string_dyn<'a>(&self, source: &str) -> (Result<Module, ()>, Vec<Diagnostic>);

    fn parse_file_dyn<'a>(&self, source: &Path) -> (Result<Module, ()>, Vec<Diagnostic>);
}

pub trait Frontend {
    type Error;

    fn parse_source<'a>(
        &self,
        errors: &'a mut FrontendErrorReceiver<'a, Self::Error>,
        source: Arc<SourceFile>,
    ) -> Result<Module, ()>;

    fn parse_string<'a>(
        &self,
        errors: &'a mut FrontendErrorReceiver<'a, Self::Error>,
        source: &str,
    ) -> Result<Module, ()>;

    fn parse_file<'a>(
        &self,
        errors: &'a mut FrontendErrorReceiver<'a, Self::Error>,
        source: &Path,
    ) -> Result<Module, ()>;
}

impl<F, E> DynFrontend for F
where
    F: Frontend<Error = E>,
    E: ToDiagnostic,
{
    fn parse_source_dyn<'a>(&self, source: Arc<SourceFile>) -> (Result<Module, ()>, Vec<Diagnostic>) {
        let mut errors = libeir_util_parse::Errors::new();
        let res = self.parse_source(&mut errors, source);
        (res, errors.iter_diagnostics().collect())
    }

    fn parse_string_dyn<'a>(&self, source: &str) -> (Result<Module, ()>, Vec<Diagnostic>) {
        let mut errors = libeir_util_parse::Errors::new();
        let res = self.parse_string(&mut errors, source);
        (res, errors.iter_diagnostics().collect())
    }

    fn parse_file_dyn<'a>(&self, source: &Path) -> (Result<Module, ()>, Vec<Diagnostic>) {
        let mut errors = libeir_util_parse::Errors::new();
        let res = self.parse_file(&mut errors, source);
        (res, errors.iter_diagnostics().collect())
    }
}

pub enum AnyFrontend {
    #[cfg(feature = "frontend_erlang")]
    Erlang(erlang::ErlangFrontend),
    #[cfg(feature = "frontend_abstr_erlang")]
    AbstrErlang(abstr_erlang::AbstrErlangFrontend),
    #[cfg(feature = "frontend_eir")]
    Eir(eir::EirFrontend),
}
impl DynFrontend for AnyFrontend {
    fn parse_source_dyn<'a>(&self, source: Arc<SourceFile>) -> (Result<Module, ()>, Vec<Diagnostic>) {
        match self {
            #[cfg(feature = "frontend_erlang")]
            AnyFrontend::Erlang(front) => front.parse_source_dyn(source),
            #[cfg(feature = "frontend_abstr_erlang")]
            AnyFrontend::AbstrErlang(front) => front.parse_source_dyn(source),
            #[cfg(feature = "frontend_eir")]
            AnyFrontend::Eir(front) => front.parse_source_dyn(source),
        }
    }

    fn parse_string_dyn<'a>(&self, source: &str) -> (Result<Module, ()>, Vec<Diagnostic>) {
        match self {
            #[cfg(feature = "frontend_erlang")]
            AnyFrontend::Erlang(front) => front.parse_string_dyn(source),
            #[cfg(feature = "frontend_abstr_erlang")]
            AnyFrontend::AbstrErlang(front) => front.parse_string_dyn(source),
            #[cfg(feature = "frontend_eir")]
            AnyFrontend::Eir(front) => front.parse_string_dyn(source),
        }
    }

    fn parse_file_dyn<'a>(&self, source: &Path) -> (Result<Module, ()>, Vec<Diagnostic>) {
        match self {
            #[cfg(feature = "frontend_erlang")]
            AnyFrontend::Erlang(front) => front.parse_file_dyn(source),
            #[cfg(feature = "frontend_abstr_erlang")]
            AnyFrontend::AbstrErlang(front) => front.parse_file_dyn(source),
            #[cfg(feature = "frontend_eir")]
            AnyFrontend::Eir(front) => front.parse_file_dyn(source),
        }
    }
}
impl From<erlang::ErlangFrontend> for AnyFrontend {
    fn from(f: erlang::ErlangFrontend) -> Self {
        AnyFrontend::Erlang(f)
    }
}
impl From<abstr_erlang::AbstrErlangFrontend> for AnyFrontend {
    fn from(f: abstr_erlang::AbstrErlangFrontend) -> Self {
        AnyFrontend::AbstrErlang(f)
    }
}
impl From<eir::EirFrontend> for AnyFrontend {
    fn from(f: eir::EirFrontend) -> Self {
        AnyFrontend::Eir(f)
    }
}
