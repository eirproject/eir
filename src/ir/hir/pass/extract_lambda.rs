use ::ir::{ FunctionDefinition, FunctionIdent, FunctionVisibility };
use ::ir::hir::{ SingleExpression, Expression, SingleExpressionKind, Function };

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

    pub fn collect(&mut self, mut fun: Function) -> FunctionIdent {
        let mut ident = self.base_ident.clone().unwrap();
        ident.lambda = Some(self.num);
        ident.arity = fun.args.len() as u32;
        self.num += 1;

        extract_lambdas_single_expression(&mut fun.body, self);
        self.lambdas.push(FunctionDefinition {
            ident: ident.clone(),
            hir_fun: fun,
            lir_function: None,
            visibility: FunctionVisibility::Lambda,
        });

        ident
    }

    pub fn finish(self) -> Vec<FunctionDefinition> {
        self.lambdas
    }

}

pub fn extract_lambdas(func: &mut FunctionDefinition,
                       lambdas: &mut LambdaCollector) {
    extract_lambdas_single_expression(
        &mut func.hir_fun.body, lambdas);
}

fn extract_lambdas_expression(expr: &mut Expression,
                              lambdas: &mut LambdaCollector) {
    for value in expr.values.iter_mut() {
        extract_lambdas_single_expression(value, lambdas);
    }
}

use self::SingleExpressionKind as SEK;
fn extract_lambdas_single_expression(expr: &mut SingleExpression,
                                     lambdas: &mut LambdaCollector) {
    match expr.kind {
        SEK::Atomic(_) => (),
        SEK::Variable(_) => (),
        SEK::NamedFunction { .. } => (),
        SEK::ApplyCall { ref mut args, .. } => {
            for arg in args.iter_mut() {
                extract_lambdas_single_expression(arg, lambdas);
            }
        },
        SEK::InterModuleCall { ref mut args, .. } => {
            for arg in args.iter_mut() {
                extract_lambdas_single_expression(arg, lambdas);
            }
        },
        SEK::Let { ref mut val, ref mut body, .. } => {
            extract_lambdas_expression(val, lambdas);
            extract_lambdas_single_expression(body, lambdas);
        },
        SEK::Try { ref mut body, ref mut then, ref mut catch, .. } => {
            extract_lambdas_expression(body, lambdas);
            extract_lambdas_single_expression(then, lambdas);
            extract_lambdas_single_expression(catch, lambdas);
        },
        SEK::Case { ref mut val, ref mut clauses } => {
            extract_lambdas_expression(val, lambdas);
            for clause in clauses.iter_mut() {
                extract_lambdas_single_expression(&mut clause.body, lambdas);
            }
        },
        SEK::Tuple(ref mut vals) => {
            for val in vals.iter_mut() {
                extract_lambdas_single_expression(val, lambdas);
            }
        },
        SEK::List { ref mut head, ref mut tail } => {
            for val in head.iter_mut() {
                extract_lambdas_single_expression(val, lambdas);
            }
            extract_lambdas_single_expression(tail, lambdas);
        },
        SEK::PrimOp { ref mut args, .. } => {
            for arg in args.iter_mut() {
                extract_lambdas_single_expression(arg, lambdas);
            }
        },
        SEK::Do(ref mut d1, ref mut d2) => {
            extract_lambdas_expression(d1, lambdas);
            extract_lambdas_single_expression(d2, lambdas);
        },
        SEK::Receive { ref mut timeout_time, ref mut timeout_body,
                       ref mut clauses } => {
            extract_lambdas_single_expression(timeout_time, lambdas);
            extract_lambdas_single_expression(timeout_body, lambdas);
            for clause in clauses.iter_mut() {
                extract_lambdas_single_expression(&mut clause.body, lambdas);
            }
        },
        SEK::BindClosure { ref mut closure, .. } => {
            let fun = closure.fun.take().unwrap();
            let name = lambdas.collect(*fun);
            closure.ident = Some(name);
        },
        SEK::BindClosures { ref mut closures, .. } => {
            for closure in closures.iter_mut() {
                let fun = closure.fun.take().unwrap();
                let name = lambdas.collect(*fun);
                closure.ident = Some(name);
            }
        }
        ref k => panic!("{:?}", k),
    }
}
