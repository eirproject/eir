use libeir_ir::{ Module, FunctionBuilder };

mod compile_pattern;
pub use self::compile_pattern::CompilePatternPass;

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
        for (ident, fun) in module.functions.iter_mut() {
            println!("============ {}", ident);
            let mut b = FunctionBuilder::new(fun);
            for pass in self.passes.iter_mut() {
                match pass {
                    PassType::Function(fun_pass) => {
                        fun_pass.run_function_pass(&mut b);
                    }
                }
            }
        }
    }

}

impl Default for PassManager {
    fn default() -> Self {
        let mut man = PassManager::new();
        man.push_function_pass(CompilePatternPass::new());
        man
    }
}
