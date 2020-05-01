use libeir_diagnostics::SourceSpan;
use libeir_ir::{Block, FunctionBuilder, Value};

pub struct ExceptionHandlerStack {
    stack: Vec<(Value, bool)>,
}
impl ExceptionHandlerStack {
    pub fn new() -> Self {
        ExceptionHandlerStack { stack: vec![] }
    }

    pub fn push_handler(&mut self, handler: Value) {
        self.stack.push((handler, true));
    }

    pub fn pop_handler(&mut self) {
        self.stack.pop();
    }

    pub fn make_error_jump_trace(
        &self,
        b: &mut FunctionBuilder,
        block: Block,
        typ: Value,
        error: Value,
        trace: Value,
    ) {
        let (handler, has_arg) = self.stack.last().unwrap();
        if *has_arg {
            b.op_call_flow(block, *handler, &[typ, error, trace])
        } else {
            b.op_call_flow(block, *handler, &[])
        }
    }

    pub fn make_error_jump(
        &self,
        b: &mut FunctionBuilder,
        span: SourceSpan,
        block: Block,
        typ: Value,
        error: Value,
    ) {
        let cont = b.op_trace_capture_raw(span, block);
        let trace = b.block_args(cont)[0];
        self.make_error_jump_trace(b, cont, typ, error, trace);
    }

    pub fn finish(&self) {
        assert!(self.stack.len() == 0);
    }

    pub fn len(&self) -> usize {
        self.stack.len()
    }
}
