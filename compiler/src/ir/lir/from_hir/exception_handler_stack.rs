use ::ir::SSAVariable;
use ::ir::lir::LabelN;
use ::ir::lir::cfg::FunctionCfgBuilder;

pub struct ExceptionHandlerStack {
    stack: Vec<(LabelN, SSAVariable)>,
}
impl ExceptionHandlerStack {

    pub fn new(root_label: LabelN, root_ssa: SSAVariable) -> Self {
        ExceptionHandlerStack{
            stack: vec![(root_label, root_ssa)],
        }
    }

    pub fn push_catch(&mut self, label: LabelN, phi_ssa: SSAVariable) {
        self.stack.push((label, phi_ssa));
    }
    pub fn pop_catch(&mut self) {
        assert!(self.stack.len() > 1);
        self.stack.pop();
    }

    pub fn add_error_jump(&self, b: &mut FunctionCfgBuilder,
                      from_label: LabelN, exception_ssa: SSAVariable) {
        let (handler_label, handler_ssa) = self.stack.last().unwrap();
        b.add_jump(from_label, *handler_label);
        b.add_phi(from_label, exception_ssa,
                  *handler_label, *handler_ssa);
    }

    pub fn finish(&self) {
        assert!(self.stack.len() == 1);
    }

}
