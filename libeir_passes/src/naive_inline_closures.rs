use libeir_ir::FunctionBuilder;
use libeir_ir::OpKind;

use super::FunctionPass;

pub struct NaiveInlineClosuresPass {
}

impl NaiveInlineClosuresPass {

    pub fn new() -> Self {
        NaiveInlineClosuresPass {
        }
    }

}

impl FunctionPass for NaiveInlineClosuresPass {
    fn run_function_pass(&mut self, b: &mut FunctionBuilder) {
        self.inline_closures(b);
    }
}

impl NaiveInlineClosuresPass {

    pub fn inline_closures(&mut self, b: &mut FunctionBuilder) {
        let mut dfs = b.fun().block_graph().dfs_post_order();

        loop {
            dfs.reset(&b.fun().block_graph());
            while let Some(block) = dfs.next(&b.fun().block_graph()) {
                if let OpKind::Call = b.fun().block_kind(block).unwrap() {
                }
            }
            break;
        }
    }

}
