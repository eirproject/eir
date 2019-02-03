use ::ir::{ FunctionDefinition, FunctionIdent, FunctionVisibility };
use ::ir::hir::Function;
use ::ir::hir::EachSingleExpression;
use ::eir::LambdaEnvIdx;

pub struct LambdaCollector {
    num: u32,
    lambdas: Vec<FunctionDefinition>,
}
impl LambdaCollector {

    pub fn new() -> LambdaCollector {
        LambdaCollector {
            num: 0,
            lambdas: Vec::new(),
        }
    }

    pub fn collect(&mut self, env_idx: LambdaEnvIdx, ident: FunctionIdent,
                   mut fun: Function) {
        self.num += 1;

        extract_lambdas(&mut fun.body, self);
        self.lambdas.push(FunctionDefinition {
            ident: ident.clone(),
            hir_fun: fun,
            lir_function: None,
            visibility: FunctionVisibility::Lambda,
            lambda_env_idx: Some(env_idx),
        });
    }

    pub fn finish(self) -> Vec<FunctionDefinition> {
        self.lambdas
    }

}

use ::ir::hir::SingleExpressionKind as SEK;
pub fn extract_lambdas<T>(func: &mut T,
                          lambdas: &mut LambdaCollector) where T: EachSingleExpression {

    func.each_single_expression_mut(&mut |expr| {
        match expr.kind {
            SEK::BindClosure { ref mut closure, lambda_env, .. } => {
                let fun = closure.fun.take().unwrap();
                lambdas.collect(lambda_env.unwrap(),
                                closure.ident.clone().unwrap(),
                                *fun);
            },
            SEK::BindClosures { ref mut closures, lambda_env, .. } => {
                for closure in closures.iter_mut() {
                    let fun = closure.fun.take().unwrap();
                    lambdas.collect(lambda_env.unwrap(),
                                    closure.ident.clone().unwrap(),
                                    *fun);
                }
            }
            _ => (),
        }
    }, false);

}
