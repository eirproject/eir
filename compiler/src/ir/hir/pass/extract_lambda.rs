use ::ir::{ FunctionDefinition, FunctionIdent, FunctionVisibility };
use ::ir::hir::Function;
use ::ir::hir::EachSingleExpression;
use ::ir::hir::LambdaEnvIdx;

pub struct LambdaCollector {
    num: u32,
    base_ident: Option<FunctionIdent>,
    lambdas: Vec<FunctionDefinition>,
}
impl LambdaCollector {

    pub fn new() -> LambdaCollector {
        LambdaCollector {
            num: 0,
            base_ident: None,
            lambdas: Vec::new(),
        }
    }

    pub fn set_ident(&mut self, ident: FunctionIdent) {
        self.num = 0;
        self.base_ident = Some(ident);
    }

    pub fn collect(&mut self, env_idx: LambdaEnvIdx, mut fun: Function)
                   -> FunctionIdent {
        let mut ident = self.base_ident.clone().unwrap();
        ident.lambda = Some(self.num);
        ident.arity = fun.args.len() as u32;
        self.num += 1;

        extract_lambdas(&mut fun.body, self);
        self.lambdas.push(FunctionDefinition {
            ident: ident.clone(),
            hir_fun: fun,
            lir_function: None,
            visibility: FunctionVisibility::Lambda,
            lambda_env_idx: Some(env_idx),
        });

        ident
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
                let name = lambdas.collect(lambda_env.unwrap(), *fun);
                closure.ident = Some(name);
            },
            SEK::BindClosures { ref mut closures, lambda_env, .. } => {
                for closure in closures.iter_mut() {
                    let fun = closure.fun.take().unwrap();
                    let name = lambdas.collect(lambda_env.unwrap(), *fun);
                    closure.ident = Some(name);
                }
            }
            _ => (),
        }
    }, true);

}
