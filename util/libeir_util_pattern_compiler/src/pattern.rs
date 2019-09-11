use std::hash::Hash;
use std::fmt::Debug;

/// Length of `nodes` should ALWAYS be `variables.len() * clauses`
#[derive(Debug)]
pub struct ExpandedClauseNodes<V, K> {
    pub variables: Vec<V>,
    pub clauses: usize,
    pub nodes: Vec<K>,
}

pub trait PatternProvider: Debug {

    /// A reference to a unique node in the pattern graph.
    /// Every `PatternNodeKey` should belong to ONE and ONLY one
    /// `PatternNodeKind`.
    type PatternNodeKey: Copy + Hash + Debug + PartialEq + Eq;

    /// The type of pattern node.
    type PatternNodeKind: Copy + Hash + Debug + PartialEq + Eq;

    /// A variable in the output CFG.
    /// The provider is responsible for creating these as specializations
    /// are performed by `expand_clause_nodes`.
    type CfgVariable: Copy + Hash + Debug + PartialEq + Eq;

    const WILDCARD: Self::PatternNodeKind;

    fn get_root(&self)
                -> ExpandedClauseNodes<
            Self::CfgVariable, Self::PatternNodeKey>;

    /// Used to determine if the given `key` should be included in the
    /// specialization on `kind`.
    ///
    /// When passed a wildcard as kind, we are specializing on the default
    /// matrix. It should match wildcards ONLY.
    ///
    /// ## Invariants
    /// * Every `PatternNodeKey` should match on one and ONLY one
    /// `PatternNodeKind`.
    fn kind_includes(&self, kind: Self::PatternNodeKind,
                     key: Self::PatternNodeKey) -> bool;

    /// After clauses have been selected for specialization, this will
    /// be called with the set of all nodes that should be specialized on.
    ///
    fn expand_clause_nodes(&mut self, clause_nodes: Vec<Self::PatternNodeKey>, kind: Self::PatternNodeKind)
                           -> ExpandedClauseNodes<
            Self::CfgVariable, Self::PatternNodeKey>;

    fn get_wildcard_node(&self) -> Self::PatternNodeKey;

    /// Every `PatternNodeKey` should belong to one and only one
    /// `PatternNodeKind`.
    fn get_kind(&self, key: Self::PatternNodeKey) -> Self::PatternNodeKind;

    fn is_wildcard(&self, kind: Self::PatternNodeKind) -> bool {
        kind == Self::WILDCARD
    }
    fn get_wildcard(&self) -> Self::PatternNodeKind {
        Self::WILDCARD
    }

}
