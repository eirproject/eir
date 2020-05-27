use std::marker::PhantomData;
use std::rc::Rc;

use scoped_cell::{scoped_cell, ScopedCell};

use libeir_diagnostics::*;

// TODO: I would like to do a lot of things differently here, but things are
// tricky without GATs. Revisit once it lands.

#[derive(Debug, Clone)]
pub enum ErrorOrWarning<E, W> {
    Error(E),
    Warning(W),
}
impl<E, W> ToDiagnostic for ErrorOrWarning<E, W>
where
    E: ToDiagnostic,
    W: ToDiagnostic,
{
    fn to_diagnostic(&self) -> Diagnostic {
        match self {
            ErrorOrWarning::Error(err) => err.to_diagnostic(),
            ErrorOrWarning::Warning(warn) => warn.to_diagnostic(),
        }
    }
}

#[derive(Clone)]
pub struct Errors<E, W> {
    pub errors: Vec<ErrorOrWarning<E, W>>,
    failed: bool,
}

impl<E, W> Errors<E, W> {
    pub fn new() -> Self {
        Errors {
            errors: Vec::new(),
            failed: false,
        }
    }

    pub fn failed(&self) -> bool {
        self.failed
    }

    pub fn print(&self, codemap: &CodeMap)
    where
        E: ToDiagnostic,
        W: ToDiagnostic,
    {
        use term::termcolor::{ColorChoice, StandardStream};
        use term::Config;
        let config = Config::default();
        let mut out = StandardStream::stderr(ColorChoice::Auto);
        for diag in self.iter_diagnostics() {
            term::emit(&mut out, &config, codemap, &diag).unwrap();
        }
    }

    pub fn iter_diagnostics<'a>(&'a self) -> impl Iterator<Item = Diagnostic> + 'a
    where
        E: ToDiagnostic,
        W: ToDiagnostic,
    {
        self.errors.iter().map(|e| e.to_diagnostic())
    }

    pub fn errors_from<IE, IW>(&mut self, other: Errors<IE, IW>)
    where
        IE: Into<E>,
        IW: Into<W>,
    {
        self.failed = self.failed | other.failed;
        for ew in other.errors {
            match ew {
                ErrorOrWarning::Error(err) => self.errors.push(ErrorOrWarning::Error(err.into())),
                ErrorOrWarning::Warning(warn) => {
                    self.errors.push(ErrorOrWarning::Warning(warn.into()))
                }
            }
        }
    }
}

impl<E, W> ErrorReceiver for Errors<E, W> {
    type E = E;
    type W = W;

    fn is_failed(&self) -> bool {
        self.failed
    }

    fn warning(&mut self, warning: W) {
        self.errors.push(ErrorOrWarning::Warning(warning));
    }

    fn error(&mut self, error: E) {
        self.errors.push(ErrorOrWarning::Error(error));
        self.failed = true;
    }
}

pub struct MessageIgnore<E, W> {
    failed: bool,
    _phantom: PhantomData<(E, W)>,
}

impl<E, W> MessageIgnore<E, W> {
    pub fn new() -> Self {
        MessageIgnore {
            failed: false,
            _phantom: PhantomData,
        }
    }

    pub fn failed(&self) -> bool {
        self.failed
    }
}

impl<E, W> ErrorReceiver for MessageIgnore<E, W> {
    type E = E;
    type W = W;

    fn is_failed(&self) -> bool {
        self.failed
    }
    fn warning(&mut self, _warning: W) {}
    fn error(&mut self, _error: E) {
        self.failed = true;
    }
}

pub trait ErrorReceiver {
    type E;
    type W;

    fn is_failed(&self) -> bool;
    fn warning(&mut self, warning: Self::W);
    fn error(&mut self, error: Self::E);
}

impl<E, W> ErrorReceiver for &mut dyn ErrorReceiver<E = E, W = W> {
    type E = E;
    type W = W;
    fn is_failed(&self) -> bool {
        (**self).is_failed()
    }
    fn warning(&mut self, warning: Self::W) {
        (**self).warning(warning)
    }
    fn error(&mut self, error: Self::E) {
        (**self).error(error)
    }
}

pub fn error_tee<'a, E, W, F, R>(
    receiver: &'a mut (dyn ErrorReceiver<E = E, W = W> + 'a),
    fun: F,
) -> R
where
    F: FnOnce(ErrorReceiverTee<E, W>) -> R,
{
    scoped_cell(receiver, |cell| {
        let tee = ErrorReceiverTee { cell };
        fun(tee)
    })
}

pub struct ErrorReceiverTee<'a, E, W> {
    cell: ScopedCell<dyn ErrorReceiver<E = E, W = W> + 'a>,
}
impl<'a, E, W> Clone for ErrorReceiverTee<'a, E, W> {
    fn clone(&self) -> Self {
        ErrorReceiverTee {
            cell: self.cell.clone(),
        }
    }
}

impl<'a, E, W> ErrorReceiverTee<'a, E, W> {
    pub fn make_adapter<NE, NW, FE, FW>(
        &mut self,
        error_adapter: FE,
        warning_adapter: FW,
    ) -> ErrorReceiverTeeAdapter<'a, E, W, NE, NW>
    where
        FE: Fn(NE) -> E + 'static,
        FW: Fn(NW) -> W + 'static,
    {
        ErrorReceiverTeeAdapter {
            cell: self.cell.clone(),
            error_adapter: Rc::new(error_adapter),
            warning_adapter: Rc::new(warning_adapter),
            phantom: PhantomData,
        }
    }

    pub fn make_into_adapter<NE, NW>(&mut self) -> ErrorReceiverTeeAdapter<'a, E, W, NE, NW>
    where
        NE: Into<E>,
        NW: Into<W>,
    {
        ErrorReceiverTeeAdapter {
            cell: self.cell.clone(),
            error_adapter: Rc::new(|o| o.into()),
            warning_adapter: Rc::new(|o| o.into()),
            phantom: PhantomData,
        }
    }
}

impl<'a, E, W> ErrorReceiver for ErrorReceiverTee<'a, E, W> {
    type E = E;
    type W = W;

    fn is_failed(&self) -> bool {
        self.cell.borrow_mut(|inner| (*inner).is_failed())
    }
    fn error(&mut self, error: E) {
        self.cell.borrow_mut(|inner| {
            inner.error(error);
        });
    }
    fn warning(&mut self, warning: W) {
        self.cell.borrow_mut(|inner| {
            inner.warning(warning);
        });
    }
}

pub struct ErrorReceiverTeeAdapter<'a, IE, IW, OE, OW> {
    cell: ScopedCell<dyn ErrorReceiver<E = IE, W = IW> + 'a>,
    error_adapter: Rc<dyn Fn(OE) -> IE>,
    warning_adapter: Rc<dyn Fn(OW) -> IW>,
    phantom: PhantomData<(OE, OW)>,
}

impl<'a, IE, IW, OE, OW> ErrorReceiver for ErrorReceiverTeeAdapter<'a, IE, IW, OE, OW> {
    type E = OE;
    type W = OW;

    fn is_failed(&self) -> bool {
        self.cell.borrow_mut(|inner| (*inner).is_failed())
    }
    fn error(&mut self, error: OE) {
        self.cell.borrow_mut(|inner| {
            inner.error((self.error_adapter)(error));
        });
    }
    fn warning(&mut self, warning: OW) {
        self.cell.borrow_mut(|inner| {
            inner.warning((self.warning_adapter)(warning));
        });
    }
}

impl<'a, IE, IW, OE, OW> Clone for ErrorReceiverTeeAdapter<'a, IE, IW, OE, OW> {
    fn clone(&self) -> Self {
        Self {
            cell: self.cell.clone(),
            error_adapter: self.error_adapter.clone(),
            warning_adapter: self.warning_adapter.clone(),
            phantom: PhantomData,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{error_tee, ErrorReceiver, Errors};

    #[test]
    fn basic_usage() {
        fn inner2(recv: &mut dyn ErrorReceiver<E = (), W = ()>) {
            error_tee(recv, |_tee| {});
        }

        fn inner(recv: &mut dyn ErrorReceiver<E = (), W = ()>) {
            error_tee(recv, |mut tee| {
                let mut adapter = tee.make_into_adapter();
                inner2(&mut adapter);
            });
        }

        let mut errors = Errors::new();
        inner(&mut errors);
    }
}
