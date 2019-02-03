use std::collections::HashSet;
use super::{ SSAVariable, AtomicTerm };

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
pub enum PatternNode {
    Wildcard,
    Bind(SSAVariable, Box<PatternNode>),
    Atomic(AtomicTerm),
    //Binary(Vec<(PatternNode, Vec<usize>)>),
    Tuple(Vec<PatternNode>),
    List(Vec<PatternNode>, Box<PatternNode>),
    Map(Vec<(usize, Box<PatternNode>)>),
}
