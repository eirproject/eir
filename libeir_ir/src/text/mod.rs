//pub mod printer;
//pub use printer::{ ToEirText, ToEirTextFun, ToEirTextContext };

pub mod printer;

pub mod dot_printer;
pub use dot_printer::function_to_dot;

mod lower;
pub use lower::{LowerContext, LowerError, LowerMap};

pub mod parse_dyn;

//pub trait TextFormatter {
//    // TODO add result
//    fn write(&mut self, text: &str);
//    fn newline(&mut self);
//
//    fn enter_indent(&mut self, dist: usize);
//    fn exit_indent(&mut self, dist: usize);
//}
//
//pub struct BufferTextFormatter {
//    indent: usize,
//    buf: String,
//}
//
//impl Default for BufferTextFormatter {
//    fn default() -> Self {
//        BufferTextFormatter {
//            indent: 0,
//            buf: String::new(),
//        }
//    }
//}
//
//impl BufferTextFormatter {
//
//    pub fn new() -> Self {
//        Self::default()
//    }
//
//    pub fn clear(&mut self) {
//        self.indent = 0;
//        self.buf.clear();
//    }
//
//}
//
//impl TextFormatter for BufferTextFormatter {
//
//    fn write(&mut self, text: &str) {
//        self.buf.push_str(text);
//    }
//    fn newline(&mut self) {
//        self.buf.push('\n');
//        for _ in 0..self.indent {
//            self.buf.push(' ');
//        }
//    }
//
//    fn enter_indent(&mut self, _dist: usize) {
//        self.indent += 1;
//    }
//    fn exit_indent(&mut self, _dist: usize) {
//        self.indent -= 1;
//    }
//
//}

pub mod parser;
pub use parser::{
    function as parse_function, function_map as parse_function_map,
    function_map_unwrap as parse_function_map_unwrap, function_unwrap as parse_function_unwrap,
    module as parse_module, module_unwrap as parse_module_unwrap,
};

pub mod ast;
