use std::collections::HashSet;
use super::{ AtomicTerm };

mod fmt;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct ValueAssign(pub usize);

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct ValueRef(pub usize);

#[derive(Debug, Clone)]
pub struct Clause {
    pub patterns: Vec<Pattern>,
    pub assigns: Vec<ValueAssign>,
}

#[derive(Debug, Clone)]
pub struct Pattern {
    pub node: PatternNode,
}

#[derive(Debug, Clone)]
pub struct BinaryPatternElem {
    pub node: PatternNode,
    pub args: Vec<ValueRef>,
}

#[derive(Debug, Clone)]
pub enum PatternNode {
    Wildcard,
    Assign(ValueAssign, Box<PatternNode>),
    Atomic(AtomicTerm),
    Binary(Vec<BinaryPatternElem>),
    Tuple(Vec<PatternNode>),
    List(Vec<PatternNode>, Box<PatternNode>),
    Map(Vec<(ValueRef, Box<PatternNode>)>),
}
