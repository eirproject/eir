pub mod printer;
pub use printer::{ ToEirText, ToEirTextFun, ToEirTextContext };

pub mod dot_printer;
pub use dot_printer::function_to_dot;

pub trait TextFormatter {
    // TODO add result
    fn write(&mut self, text: &str);
    fn newline(&mut self);

    fn enter_indent(&mut self, dist: usize);
    fn exit_indent(&mut self, dist: usize);
}

pub struct BufferTextFormatter {
    indent: usize,
    buf: String,
}

impl BufferTextFormatter {

    pub fn new() -> Self {
        BufferTextFormatter {
            indent: 0,
            buf: String::new(),
        }
    }

    pub fn clear(&mut self) {
        self.indent = 0;
        self.buf.clear();
    }

}

impl TextFormatter for BufferTextFormatter {

    fn write(&mut self, text: &str) {
        self.buf.extend(text.chars());
    }
    fn newline(&mut self) {
        self.buf.push('\n');
        for _ in 0..self.indent {
            self.buf.push(' ');
        }
    }

    fn enter_indent(&mut self, dist: usize) {
        self.indent += 1;
    }
    fn exit_indent(&mut self, dist: usize) {
        self.indent -= 1;
    }

}

pub mod parser;
