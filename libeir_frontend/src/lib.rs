use std::path::Path;

use libeir_ir::Module;
use libeir_util_parse::{ArcCodemap, FileMapSource, ErrorReceiver};
use libeir_diagnostics::Diagnostic;

macro_rules! impl_dyn_frontend {
    ($struct:ident) => {
        impl crate::DynFrontend for $struct {

            fn parse_source_dyn<'a>(
                &self,
                codemap: libeir_util_parse::ArcCodemap,
                source: libeir_util_parse::FileMapSource,
            ) -> (Result<Module, ()>, Vec<libeir_diagnostics::Diagnostic>)
            {
                let mut errors = libeir_util_parse::Errors::new();
                let res = self.parse_source(&mut errors, codemap, source);
                (res, errors.iter_diagnostics().collect())
            }

            fn parse_string_dyn<'a>(
                &self,
                codemap: libeir_util_parse::ArcCodemap,
                source: &str,
            ) -> (Result<Module, ()>, Vec<libeir_diagnostics::Diagnostic>)
            {
                let mut errors = libeir_util_parse::Errors::new();
                let res = self.parse_string(&mut errors, codemap, source);
                (res, errors.iter_diagnostics().collect())
            }

            fn parse_file_dyn<'a>(
                &self,
                codemap: libeir_util_parse::ArcCodemap,
                source: &std::path::Path,
            ) -> (Result<Module, ()>, Vec<libeir_diagnostics::Diagnostic>)
            {
                let mut errors = libeir_util_parse::Errors::new();
                let res = self.parse_file(&mut errors, codemap, source);
                (res, errors.iter_diagnostics().collect())
            }

        }
    };
}

#[cfg(feature = "frontend_erlang")]
pub mod erlang;

#[cfg(feature = "frontend_abstr_erlang")]
pub mod abstr_erlang;

#[cfg(feature = "frontend_eir")]
pub mod eir;

pub type FrontendErrorReceiver<'a, E> = dyn ErrorReceiver<E = E, W = E> + 'a;

pub trait DynFrontend {

    fn parse_source_dyn<'a>(
        &self,
        codemap: ArcCodemap,
        source: FileMapSource,
    ) -> (Result<Module, ()>, Vec<Diagnostic>);

    fn parse_string_dyn<'a>(
        &self,
        codemap: ArcCodemap,
        source: &str,
    ) -> (Result<Module, ()>, Vec<Diagnostic>);

    fn parse_file_dyn<'a>(
        &self,
        codemap: ArcCodemap,
        source: &Path,
    ) -> (Result<Module, ()>, Vec<Diagnostic>);

}

pub trait Frontend: DynFrontend {
    type Error;

    fn parse_source<'a>(
        &self,
        errors: &'a mut FrontendErrorReceiver<'a, Self::Error>,
        codemap: ArcCodemap,
        source: FileMapSource,
    ) -> Result<Module, ()>;

    fn parse_string<'a>(
        &self,
        errors: &'a mut FrontendErrorReceiver<'a, Self::Error>,
        codemap: ArcCodemap,
        source: &str,
    ) -> Result<Module, ()>;

    fn parse_file<'a>(
        &self,
        errors: &'a mut FrontendErrorReceiver<'a, Self::Error>,
        codemap: ArcCodemap,
        source: &Path,
    ) -> Result<Module, ()>;

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

    fn parse_source_dyn<'a>(
        &self,
        codemap: ArcCodemap,
        source: FileMapSource,
    ) -> (Result<Module, ()>, Vec<Diagnostic>)
    {
        match self {
            #[cfg(feature = "frontend_erlang")]
            AnyFrontend::Erlang(front) => front.parse_source_dyn(codemap, source),
            #[cfg(feature = "frontend_abstr_erlang")]
            AnyFrontend::AbstrErlang(front) => front.parse_source_dyn(codemap, source),
            #[cfg(feature = "frontend_eir")]
            AnyFrontend::Eir(front) => front.parse_source_dyn(codemap, source),
        }
    }

    fn parse_string_dyn<'a>(
        &self,
        codemap: ArcCodemap,
        source: &str,
    ) -> (Result<Module, ()>, Vec<Diagnostic>)
    {
        match self {
            #[cfg(feature = "frontend_erlang")]
            AnyFrontend::Erlang(front) => front.parse_string_dyn(codemap, source),
            #[cfg(feature = "frontend_abstr_erlang")]
            AnyFrontend::AbstrErlang(front) => front.parse_string_dyn(codemap, source),
            #[cfg(feature = "frontend_eir")]
            AnyFrontend::Eir(front) => front.parse_string_dyn(codemap, source),
        }
    }

    fn parse_file_dyn<'a>(
        &self,
        codemap: ArcCodemap,
        source: &Path,
    ) -> (Result<Module, ()>, Vec<Diagnostic>)
    {
        match self {
            #[cfg(feature = "frontend_erlang")]
            AnyFrontend::Erlang(front) => front.parse_file_dyn(codemap, source),
            #[cfg(feature = "frontend_abstr_erlang")]
            AnyFrontend::AbstrErlang(front) => front.parse_file_dyn(codemap, source),
            #[cfg(feature = "frontend_eir")]
            AnyFrontend::Eir(front) => front.parse_file_dyn(codemap, source),
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
