use std::sync::{Arc, Mutex};

use libeir_diagnostics::{Diagnostic, Emitter, StandardStreamEmitter, ColorChoice, CodeMap};

pub struct ParserErrors<E> {
    codemap: Arc<Mutex<CodeMap>>,
    errors: Vec<E>,
    failed: bool,
}

impl<E> ParserErrors<E> {

    pub fn new(codemap: Arc<Mutex<CodeMap>>) -> Self {
        ParserErrors {
            codemap: codemap,
            errors: Vec::new(),
            failed: false,
        }
    }

    pub fn failed(&self) -> bool {
        self.failed
    }

    pub fn print(&self)
    where
        E: ToDiagnostic,
    {
        let emitter = StandardStreamEmitter::new(ColorChoice::Auto)
            .set_codemap(self.codemap.clone());
        for err in self.errors.iter() {
            emitter.diagnostic(&err.to_diagnostic()).unwrap();
        }
    }

}

pub struct MessageIgnore {
    failed: bool,
}

impl MessageIgnore {

    pub fn new() -> Self {
        MessageIgnore {
            failed: false,
        }
    }

    pub fn failed(&self) -> bool {
        self.failed
    }

}

pub trait ToDiagnostic {
    fn to_diagnostic(&self) -> Diagnostic;
}

pub trait ErrorReceiver<E, W> {
    fn is_failed(&self) -> bool;
    fn warning(&mut self, warning: W);
    fn error(&mut self, error: E);
}

impl<E, W, I> ErrorReceiver<E, W> for ParserErrors<I>
where
    E: Into<I>,
    W: Into<I>,
{
    fn is_failed(&self) -> bool {
        self.failed
    }

    fn warning(&mut self, warning: W) {
        self.errors.push(warning.into());
    }

    fn error(&mut self, error: E) {
        self.errors.push(error.into());
        self.failed = true;
    }
}

impl<E, W> ErrorReceiver<E, W> for MessageIgnore {
    fn is_failed(&self) -> bool {
        self.failed
    }
    fn warning(&mut self, _warning: W) {}
    fn error(&mut self, _error: E) {
        self.failed = true;
    }
}
