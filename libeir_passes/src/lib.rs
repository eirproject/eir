#![deny(warnings)]

use libeir_ir::{ Module, FunctionBuilder };

mod compile_pattern;
pub use self::compile_pattern::CompilePatternPass;

mod naive_inline_closures;
pub use self::naive_inline_closures::NaiveInlineClosuresPass;

mod simplify_cfg;
pub use self::simplify_cfg::SimplifyCfgPass;

mod validate;
pub use self::validate::ValidatePass;

pub trait FunctionPass {
    fn run_function_pass(&mut self, b: &mut FunctionBuilder);
}

enum PassType {
    Function(Box<dyn FunctionPass>),
}

pub struct PassManager {
    passes: Vec<PassType>,
}

impl PassManager {

    pub fn new() -> Self {
        PassManager {
            passes: Vec::new(),
        }
    }

    pub fn push_function_pass<P>(&mut self, pass: P) where P: FunctionPass + 'static {
        self.passes.push(PassType::Function(Box::new(pass)));
    }

    pub fn run(&mut self, module: &mut Module) {
        for fun_def in module.function_iter_mut() {
            let fun = fun_def.function_mut();
            let mut b = FunctionBuilder::new(fun);
            b.fun().graph_validate_global();
            for pass in self.passes.iter_mut() {
                match pass {
                    PassType::Function(fun_pass) => {
                        fun_pass.run_function_pass(&mut b);
                    }
                }
                b.fun().graph_validate_global();
            }
        }
    }

}

impl Default for PassManager {
    fn default() -> Self {
        let mut man = PassManager::new();
        man.push_function_pass(CompilePatternPass::new());
        man.push_function_pass(NaiveInlineClosuresPass::new());
        man.push_function_pass(SimplifyCfgPass::new());
        man.push_function_pass(NaiveInlineClosuresPass::new());
        man
    }
}
