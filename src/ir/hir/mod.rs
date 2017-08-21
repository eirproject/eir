use ::std::collections::HashMap;
use super::{ AVariable, AFunctionName, SSAVariable, FunctionIdent };
use ::parser;
use ::{ Atom, Variable };

pub mod from_parsed;
pub mod pass;

#[derive(Debug, Clone)]
pub struct Function {
    pub args: Vec<AVariable>,
    pub body: SingleExpression,
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub values: Vec<SingleExpression>,
}

#[derive(Debug, Clone)]
pub struct SingleExpression {
    pub ssa: SSAVariable,
    pub kind: SingleExpressionKind,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct LambdaEnvIdx(pub usize);

#[derive(Debug, Clone)]
pub enum SingleExpressionKind {
    Variable(AVariable),
    NamedFunction{
        name: AFunctionName,
        is_lambda: bool,
    },

    // Functions
    BindClosure { closure: Closure, lambda_env: Option<LambdaEnvIdx>,
                  env_ssa: SSAVariable },
    BindClosures { closures: Vec<Closure>, lambda_env: Option<LambdaEnvIdx>,
                   body: Box<SingleExpression>, env_ssa: SSAVariable },

    // Value constructors
    Atomic(parser::AtomicLiteral),
    Tuple(Vec<SingleExpression>),
    List { head: Vec<SingleExpression>, tail: Box<SingleExpression> },
    Map(Vec<(SingleExpression, SingleExpression)>),

    // Calls
    PrimOp { name: Atom, args: Vec<SingleExpression> },
    ApplyCall { fun: Box<SingleExpression>, args: Vec<SingleExpression> },
    InterModuleCall { module: Box<SingleExpression>, name: Box<SingleExpression>,
                      args: Vec<SingleExpression> },

    // Combinators
    Let { vars: Vec<AVariable>, val: Expression, body: Box<SingleExpression> },
    /// then and catch must have the same amount of items
    Try { body: Expression, then_vars: Vec<AVariable>, then: Box<SingleExpression>,
          catch_vars: Vec<AVariable>, catch: Box<SingleExpression> },
    Case { val: Expression, clauses: Vec<Clause>, values: Vec<SingleExpression> },
    Test { tests: Vec<TestEntry> },
    Do(Expression, Box<SingleExpression>),
    Receive { clauses: Vec<Clause>, timeout_time: Box<SingleExpression>,
              timeout_body: Box<SingleExpression>,
              pattern_values: Vec<SingleExpression> },
}

#[derive(Debug, Clone)]
pub enum TestEntry {
}

#[derive(Debug, Clone)]
pub struct Closure {
    pub alias: Option<AFunctionName>,
    pub ident: Option<FunctionIdent>,
    pub fun: Option<Box<Function>>,
    pub env: Option<LambdaEnvIdx>,
}

#[derive(Debug, Clone)]
pub struct Clause {
    pub patterns: Vec<Pattern>,
    pub guard: SingleExpression,
    pub body: SingleExpression,
}

#[derive(Debug, Clone)]
pub struct Pattern {
    pub bindings: Vec<(Variable, SSAVariable)>,
    pub node: PatternNode,
}

#[derive(Debug, Clone)]
pub enum PatternNode {
    Variable(Variable),
    Bind(Variable, Box<PatternNode>),
    Atomic(parser::AtomicLiteral),
    Tuple(Vec<PatternNode>),
    List(Vec<PatternNode>, Box<PatternNode>),
    Map(Vec<(usize, Box<PatternNode>)>),
}
impl PatternNode {
    pub fn collect_bindings(&self, bindings: &mut Vec<Variable>) {
        match *self {
            PatternNode::Variable(ref v) => bindings.push(v.clone()),
            PatternNode::Bind(ref v, ref p) => {
                bindings.push(v.clone());
                p.collect_bindings(bindings);
            },
            PatternNode::Atomic(_) => (),
            PatternNode::Tuple(ref pats) => {
                for pat in pats {
                    pat.collect_bindings(bindings);
                }
            }
            PatternNode::List(ref pats, ref tail) => {
                for pat in pats {
                    pat.collect_bindings(bindings);
                }
                tail.collect_bindings(bindings);
            }
            PatternNode::Map(ref kvs) => {
                for kv in kvs {
                    kv.1.collect_bindings(bindings);
                }
            }
        }
    }
}

