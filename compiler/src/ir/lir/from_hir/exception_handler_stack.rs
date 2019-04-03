use ::ssa::SSAVariable;
//use ::eir::cfg::{ FunctionCfgBuilder, LabelN };
use ::eir::{ FunctionBuilder, Value, Ebb, EbbCall };

pub struct ExceptionHandlerStack {
    stack: Vec<(Ebb, bool)>,
}
impl ExceptionHandlerStack {

    pub fn new(root_handler: Ebb) -> Self {
        ExceptionHandlerStack {
            stack: vec![(root_handler, true)],
        }
    }

    pub fn push_handler(&mut self, ebb: Ebb) {
        self.stack.push((ebb, true));
    }
    //pub fn push_handler_novalue(&mut self, ebb: Ebb) {
    //    self.stack.push((ebb, false));
    //}

    pub fn pop_handler(&mut self) {
        assert!(self.stack.len() > 1);
        self.stack.pop();
    }

    pub fn make_error_jump(&self, b: &mut FunctionBuilder, value: Value) -> EbbCall {
        let (handler, has_arg) = self.stack.last().unwrap();
        if *has_arg {
            b.create_ebb_call(*handler, &[value])
        } else {
            b.create_ebb_call(*handler, &[])
        }
    }

    pub fn finish(&self) {
        assert!(self.stack.len() == 1);
    }

}
