use super::FunctionPass;

use libeir_ir::{FunctionBuilder, ValidationError};

pub struct ValidatePass {
    err_buf: Vec<ValidationError>,
}

impl ValidatePass {
    pub fn new() -> Self {
        ValidatePass {
            err_buf: Vec::new(),
        }
    }
}

impl FunctionPass for ValidatePass {
    fn run_function_pass(&mut self, b: &mut FunctionBuilder) {
        self.err_buf.clear();
        b.fun().validate(&mut self.err_buf);
        assert!(self.err_buf.len() == 0);
    }
}
