use std::collections::HashSet;
use super::{ SSAVariable, ConstantTerm, AtomicTerm };

mod fmt;

#[derive(Debug, Clone)]
pub struct Clause {
    pub patterns: Vec<Pattern>,
}

#[derive(Debug, Clone)]
pub struct Pattern {
    pub binds: HashSet<SSAVariable>,
    pub node: PatternNode,
}

#[derive(Debug, Clone)]
pub enum ConstantOrSSA {
    Constant(ConstantTerm),
    SSA(SSAVariable),
}

#[derive(Debug, Clone)]
pub struct BinaryPatternElem {
    pub node: PatternNode,
    pub args: Vec<ConstantOrSSA>,
}

#[derive(Debug, Clone)]
pub enum PatternNode {
    Wildcard,
    Bind(SSAVariable, Box<PatternNode>),
    Atomic(AtomicTerm),
    Binary(Vec<BinaryPatternElem>),
    //Binary(Vec<(PatternNode, Vec<usize>)>),
    Tuple(Vec<PatternNode>),
    List(Vec<PatternNode>, Box<PatternNode>),
    Map(Vec<(usize, Box<PatternNode>)>),
}
