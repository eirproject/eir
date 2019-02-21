use std::collections::HashMap;
use ::std::fmt;
use super::{ AVariable, AFunctionName, SSAVariable, FunctionIdent };
use ::{ Atom, Variable };
use ::eir::LambdaEnvIdx;
use ::parser::{ MapExactAssoc, ConstantOrVariable };

pub mod from_parsed;
pub mod pass;
pub mod scope_tracker;

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

impl Expression {
    pub fn ssa_vars<'a>(&'a self) -> impl Iterator<Item = SSAVariable> + 'a {
        self.values
            .iter()
            .map(|v| v.ssa)
    }
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
            SEK::Catch { ref mut body } => {
                body.each_single_expression_mut(f, enter_lambdas);
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
            SEK::ValueList(ref mut values) => {
                for val in values.iter_mut() {
                    val.each_single_expression_mut(f, enter_lambdas);
                }
            },
            SEK::Map { ref mut values, ref mut merge } => {
                for &mut (ref mut key, ref mut val, _assoc) in values.iter_mut() {
                    key.each_single_expression_mut(f, enter_lambdas);
                    val.each_single_expression_mut(f, enter_lambdas);
                }
                merge.as_mut().map(|m| m.each_single_expression_mut(f, enter_lambdas));
            },
            SEK::Binary(ref mut elems) => {
                for (ref mut value, ref mut opts) in elems.iter_mut() {
                    value.each_single_expression_mut(f, enter_lambdas);
                    for mut opt in opts.iter_mut() {
                        opt.each_single_expression_mut(f, enter_lambdas);
                    }
                }
            }
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
impl ::ToDoc for SingleExpression {
    fn to_doc<'a>(&'a self) -> Doc<'a, BoxDoc> {
        use self::SingleExpressionKind as SEK;

        let comma_space = || Doc::text(",").append(Doc::space());

        let main = match self.kind {
            SEK::Atomic(ref inner) => Doc::text(format!("{:?}", inner)),
            SEK::Variable(ref var) => Doc::text(format!("{:?}", var)),
            SEK::NamedFunction { ref name, ref is_lambda } =>
                Doc::text(format!("{:?} lambda: {}", name, is_lambda)),
            SEK::ExternalNamedFunction { ref name } =>
                Doc::text(format!("{:?}", name)),
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

#[derive(Debug, Clone)]
pub enum SingleExpressionKind {
    Variable(AVariable),
    NamedFunction {
        name: AFunctionName,
        is_lambda: bool,
    },
    ExternalNamedFunction {
        name: AFunctionName,
    },

    // Functions
    BindClosure { closure: Closure, lambda_env: Option<LambdaEnvIdx>,
                  env_ssa: SSAVariable },
    BindClosures { closures: Vec<Closure>, lambda_env: Option<LambdaEnvIdx>,
                   body: Box<SingleExpression>, env_ssa: SSAVariable },

    // Value constructors
    Atomic(::eir::AtomicTerm),
    Tuple(Vec<SingleExpression>),
    List { head: Vec<SingleExpression>, tail: Box<SingleExpression> },
    Map { values: Vec<(SingleExpression, SingleExpression, MapExactAssoc)>,
          merge: Option<Box<SingleExpression>> },
    Binary(Vec<(SingleExpression, Vec<SingleExpression>)>),
    ValueList(Vec<SingleExpression>),

    // Calls
    PrimOp { name: Atom, args: Vec<SingleExpression> },
    ApplyCall { fun: Box<SingleExpression>, args: Vec<SingleExpression> },
    InterModuleCall { module: Box<SingleExpression>, name: Box<SingleExpression>,
                      args: Vec<SingleExpression> },

    // Combinators
    Let { vars: Vec<AVariable>, val: Box<SingleExpression>, body: Box<SingleExpression> },
    /// then and catch must have the same amount of items
    Catch { body: Box<SingleExpression> },
    Try { body: Box<SingleExpression>, then_vars: Vec<AVariable>, then: Box<SingleExpression>,
          catch_vars: Vec<AVariable>, catch: Box<SingleExpression> },

    Case {
        /// The value to match on. Each pattern in the clauses will
        /// match on the values of this value list.
        val: Box<SingleExpression>,
        /// The clauses in the pattern are matched on in order.
        clauses: Vec<Clause>,
        /// Values that are utilized to match on in the pattern.
        /// They should be constructed and available before the pattern
        /// structure starts.
        values: Vec<SingleExpression>,
    },

    Test { tests: Vec<TestEntry> },
    Do(Box<SingleExpression>, Box<SingleExpression>),
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
    pub parent_ident: FunctionIdent,
    pub ident: Option<FunctionIdent>,
    pub fun: Option<Box<Function>>,
    pub env: Option<LambdaEnvIdx>,
}
impl Closure {
    fn gen_ident(&mut self, env_idx: LambdaEnvIdx, lambda_num: usize) {
        assert!(self.ident.is_none());
        assert!(self.env.is_none());

        let mut ident = self.parent_ident.clone();
        // + 1 for lambda env
        ident.arity = self.fun.as_ref().unwrap().args.len() + 1;
        ident.lambda = Some((env_idx, lambda_num));

        self.ident = Some(ident);
        self.env = Some(env_idx);
    }
}

#[derive(Debug, Clone)]
pub struct Clause {
    /// Each of these patterns represents a single entry in the
    /// value list we match on.
    pub patterns: Vec<Pattern>,
    /// When a pattern matches, the guard clause is evaluated to
    /// determine if the match should be accepted or rejected.
    /// In strict erlang, the guard clause can only contain
    /// function calls from a strict set, but we support all
    /// calls in the guard clause for uniformity.
    pub guard: SingleExpression,
    /// The body is evaluated and returned if the clause matches
    /// and the guard passes.
    pub body: SingleExpression,
}

#[derive(Debug, Clone)]
pub struct Pattern {
    /// If the parent clause matches, these variables identifiers
    /// in the pattern should be bound to these SSA variables.
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
    pub fn to_eir(&self) -> ::eir::pattern::Pattern {
        let binds_map: HashMap<_, _> = self.binds.iter().cloned().collect();
        ::eir::pattern::Pattern {
            binds: self.binds.iter().map(|(_var, ssa)| *ssa).collect(),
            node: self.node.to_eir(&binds_map),
        }
    }
}

#[derive(Debug, Clone)]
pub enum PatternNode {
    Wildcard,
    BindVar(Variable, Box<PatternNode>),
    Atomic(::eir::AtomicTerm),
    Binary(Vec<(PatternNode, Vec<ConstantOrVariable>)>),
    Tuple(Vec<PatternNode>),
    List(Vec<PatternNode>, Box<PatternNode>),
    Map(Vec<(usize, Box<PatternNode>)>),
}
impl PatternNode {
    fn to_eir(&self, binds: &HashMap<Variable, SSAVariable>) -> ::eir::pattern::PatternNode {
        use ::eir::pattern::PatternNode as EPN;
        match self {
            PatternNode::Wildcard => EPN::Wildcard,
            PatternNode::BindVar(var, node) =>
                EPN::Bind(binds[var], Box::new(node.to_eir(binds))),
            PatternNode::Atomic(atomic) => EPN::Atomic(atomic.clone()),
            PatternNode::Binary(entries) => {
                let entries_n = entries.iter()
                    .map(|(pat, args)| {
                        ::eir::pattern::BinaryPatternElem {
                            node: pat.to_eir(binds),
                            args: args.iter()
                                .map(|a| {
                                    match a {
                                        ConstantOrVariable::Variable(var) =>
                                            ::eir::pattern::ConstantOrSSA::SSA(var.ssa),
                                        ConstantOrVariable::Constant(constant) =>
                                            ::eir::pattern::ConstantOrSSA::Constant(constant.to_eir()),
                                    }
                                }).collect(),
                        }
                    }).collect();
                EPN::Binary(entries_n)
            },
            PatternNode::Tuple(nodes) =>
                EPN::Tuple(nodes.iter().map(|n| n.to_eir(binds)).collect()),
            PatternNode::List(head, tail) =>
                EPN::List(
                    head.iter().map(|n| n.to_eir(binds)).collect(),
                    Box::new(tail.to_eir(binds))),
            PatternNode::Map(nodes) =>
                EPN::Map(nodes.iter()
                         .map(|n| (n.0, Box::new(n.1.to_eir(binds))))
                         .collect()),
        }
    }
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
            },
            PatternNode::Binary(elems) => {
                write!(f, "#<")?;

                for elem in elems {
                    write!(f, "#<{}>(", elem.0)?;
                    for attr in &elem.1 {
                        write!(f, "{:?}, ", attr)?;
                    }
                    write!(f, ")#")?;
                }

                write!(f, ">#")?;
            },
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
            PatternNode::BindVar(_, ref p) =>
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
            PatternNode::Binary(ref elems) => {
                for elem in elems {
                    elem.0.traverse_pattern(fun);
                }
            }
        }
    }

}

