use ::std::collections::HashMap;
use ::std::fmt;
use super::{ AVariable, AFunctionName, SSAVariable, FunctionIdent };
use ::parser;
use ::{ Atom, Variable };

pub mod from_parsed;
pub mod pass;

pub trait EachSingleExpression {
    fn each_single_expression_mut<F>(&mut self, f: &mut F, enter_lambdas: bool) where F: FnMut(&mut SingleExpression);
}

#[derive(Debug, Clone)]
pub struct Function {
    pub args: Vec<AVariable>,
    pub body: SingleExpression,
}

impl EachSingleExpression for Function {
    fn each_single_expression_mut<F>(&mut self, f: &mut F, enter_lambdas: bool)
        where F: FnMut(&mut SingleExpression) {

        self.body.each_single_expression_mut(f, enter_lambdas)
    }
}

#[derive(Debug, Clone)]
pub struct Expression {
    pub values: Vec<SingleExpression>,
}

impl EachSingleExpression for Expression {
    fn each_single_expression_mut<F>(&mut self, f: &mut F, enter_lambdas: bool)
        where F: FnMut(&mut SingleExpression) {

        for value in self.values.iter_mut() {
            value.each_single_expression_mut(f, enter_lambdas)
        }
    }
}

#[derive(Debug, Clone)]
pub struct SingleExpression {
    pub ssa: SSAVariable,
    pub kind: SingleExpressionKind,
}

impl EachSingleExpression for SingleExpression {
    fn each_single_expression_mut<F>(&mut self, f: &mut F, enter_lambdas: bool)
        where F: FnMut(&mut SingleExpression) {

        use self::SingleExpressionKind as SEK;
        match self.kind {
            SEK::Atomic(_) => (),
            SEK::Variable(_) => (),
            SEK::NamedFunction { .. } => (),
            SEK::ExternalNamedFunction { .. } => (),
            SEK::ApplyCall { ref mut args, .. } => {
                for arg in args.iter_mut() {
                    arg.each_single_expression_mut(f, enter_lambdas);
                }
            },
            SEK::InterModuleCall { ref mut args, .. } => {
                for arg in args.iter_mut() {
                    arg.each_single_expression_mut(f, enter_lambdas);
                }
            },
            SEK::Let { ref mut val, ref mut body, .. } => {
                val.each_single_expression_mut(f, enter_lambdas);
                body.each_single_expression_mut(f, enter_lambdas);
            },
            SEK::Try { ref mut body, ref mut then, ref mut catch, .. } => {
                body.each_single_expression_mut(f, enter_lambdas);
                then.each_single_expression_mut(f, enter_lambdas);
                catch.each_single_expression_mut(f, enter_lambdas);
            },
            SEK::Case { ref mut val, ref mut clauses, ref mut values } => {
                val.each_single_expression_mut(f, enter_lambdas);

                // Pattern values should strictly not contain any advanced
                // control flow, but support for uniformity.
                for value in values.iter_mut() {
                    value.each_single_expression_mut(f, enter_lambdas);
                }

                for clause in clauses.iter_mut() {
                    clause.body.each_single_expression_mut(f, enter_lambdas);
                }
            },
            SEK::Tuple(ref mut vals) => {
                for val in vals.iter_mut() {
                    val.each_single_expression_mut(f, enter_lambdas);
                }
            },
            SEK::List { ref mut head, ref mut tail } => {
                for val in head.iter_mut() {
                    val.each_single_expression_mut(f, enter_lambdas);
                }
                tail.each_single_expression_mut(f, enter_lambdas);
            },
            SEK::Map { ref mut values, ref mut merge } => {
                for &mut (ref mut key, ref mut val) in values.iter_mut() {
                    key.each_single_expression_mut(f, enter_lambdas);
                    val.each_single_expression_mut(f, enter_lambdas);
                }
                merge.as_mut().map(|m| m.each_single_expression_mut(f, enter_lambdas));
            },
            SEK::PrimOp { ref mut args, .. } => {
                for arg in args.iter_mut() {
                    arg.each_single_expression_mut(f, enter_lambdas);
                }
            },
            SEK::Do(ref mut d1, ref mut d2) => {
                d1.each_single_expression_mut(f, enter_lambdas);
                d2.each_single_expression_mut(f, enter_lambdas);
            },
            SEK::Receive { ref mut timeout_time, ref mut timeout_body,
                           ref mut clauses, ref mut pattern_values } => {
                // Pattern values should strictly not contain any advanced
                // control flow, but support for uniformity.
                for value in pattern_values {
                    value.each_single_expression_mut(f, enter_lambdas);
                }
                timeout_time.each_single_expression_mut(f, enter_lambdas);
                timeout_body.each_single_expression_mut(f, enter_lambdas);
                for clause in clauses.iter_mut() {
                    clause.body.each_single_expression_mut(f, enter_lambdas);
                }
            },
            SEK::BindClosure { ref mut closure, .. } => {
                if enter_lambdas {
                    closure.fun.as_mut().unwrap().each_single_expression_mut(f, enter_lambdas);
                }
            },
            SEK::BindClosures { ref mut closures, .. } => {
                if enter_lambdas {
                    for closure in closures {
                        closure.fun.as_mut().unwrap().each_single_expression_mut(f, enter_lambdas);
                    }
                }
            },
            SEK::Test { .. } => {},
        }

        f(self)
    }
}

use ::pretty::{ BoxDoc, Doc };
use ::std::ops::Deref;
impl ::ToDoc for SingleExpression {
    fn to_doc<'a>(&'a self) -> Doc<'a, BoxDoc> {
        use self::SingleExpressionKind as SEK;

        let comma_space = || Doc::text(",").append(Doc::space());

        let main = match self.kind {
            SEK::Atomic(ref inner) => Doc::text(format!("{:?}", inner)),
            SEK::Variable(ref var) => Doc::text(format!("{:?}", var)),
            SEK::NamedFunction { ref name, ref is_lambda } =>
                Doc::text(format!("{:?} lambda: {}", name, is_lambda)),
            SEK::ExternalNamedFunction { ref module, ref name } =>
                Doc::text(format!("{:?}:{:?}", module, name)),
            SEK::InterModuleCall { ref module, ref name, ref args } => {
                let args_doc = Doc::intersperse(
                    args.iter().map(|arg| Doc::newline().append(arg.to_doc())), 
                    comma_space()).group();

                Doc::concat(vec![
                    Doc::text("InterModuleCall("), Doc::space(),
                    Doc::intersperse(vec![
                        module.to_doc(), name.to_doc(), 
                        Doc::text("[").append(args_doc).append(Doc::text("]")),
                    ], comma_space()).nest(2),
                    Doc::text(")")
                ]).group()
            }
            ref e => Doc::text(format!("UNIMPL {:?}", e)),
        };

        Doc::concat(vec![
            Doc::text(format!("{:?}:", self.ssa)),
            main
        ])
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct LambdaEnvIdx(pub usize);

#[derive(Debug, Clone)]
pub enum SingleExpressionKind {
    Variable(AVariable),
    NamedFunction {
        name: AFunctionName,
        is_lambda: bool,
    },
    ExternalNamedFunction {
        module: Atom,
        name: AFunctionName,
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
    Map { values: Vec<(SingleExpression, SingleExpression)>,
          merge: Option<Box<SingleExpression>> },

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
    pub binds: Vec<(Variable, SSAVariable)>,
    pub node: PatternNode,
}
impl Pattern {
    pub fn wildcard() -> Self {
        Pattern {
            binds: vec![],
            node: PatternNode::Wildcard,
        }
    }
}

#[derive(Debug, Clone)]
pub enum PatternNode {
    Wildcard,
    BindVar(Variable, Box<PatternNode>),
    Atomic(parser::AtomicLiteral),
    Tuple(Vec<PatternNode>),
    List(Vec<PatternNode>, Box<PatternNode>),
    Map(Vec<(usize, Box<PatternNode>)>),
}
impl fmt::Display for PatternNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            PatternNode::Wildcard =>
                write!(f, "_")?,
            PatternNode::BindVar(var, pat) =>
                write!(f, "({} = {})", var, pat)?,
            PatternNode::Atomic(lit) =>
                write!(f, "{:?}", lit)?,
            PatternNode::Tuple(nodes) => {
                write!(f, "{{")?;
                for node in nodes {
                    write!(f, "{}, ", node)?;
                }
                write!(f, "}}")?;
            },
            PatternNode::List(head_values, tail) => {
                write!(f, "[")?;
                for val in head_values {
                    write!(f, "{}, ", val)?;
                }
                write!(f, " | {}", tail)?;
                write!(f, "]")?;
            },
            PatternNode::Map(values) => {
                write!(f, "~{{")?;
                for (key_num, val) in values {
                    write!(f, "{}; {}, ", key_num, val)?;
                }
                write!(f, "}}~")?;
            }
        }
        Ok(())
    }
}
impl PatternNode {

    pub fn get_bind_vars(&self) -> Vec<Variable> {
        let mut matches = Vec::new();
        self.traverse_pattern(&mut |node| {
            match *node {
                PatternNode::BindVar(ref v, _) =>
                    matches.push(v.clone()),
                _ => (),
            }
        });
        matches
    }

    pub fn traverse_pattern<F>(&self, fun: &mut F) where F: FnMut(&PatternNode) {
        fun(self);
        match *self {
            PatternNode::Wildcard => (),
            PatternNode::BindVar(ref v, ref p) =>
                p.traverse_pattern(fun),
            PatternNode::Atomic(_) => (),
            PatternNode::Tuple(ref pats) => {
                for pat in pats {
                    pat.traverse_pattern(fun);
                }
            }
            PatternNode::List(ref pats, ref tail) => {
                for pat in pats {
                    pat.traverse_pattern(fun);
                }
                tail.traverse_pattern(fun);
            }
            PatternNode::Map(ref kvs) => {
                for kv in kvs {
                    kv.1.traverse_pattern(fun);
                }
            }
        }
    }

}

