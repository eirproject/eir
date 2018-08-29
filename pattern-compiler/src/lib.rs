// Implements a variant of
// http://www.cs.tufts.edu/~nr/cs257/archive/luc-maranget/jun08.pdf

extern crate petgraph;
extern crate prettytable;
extern crate either;
extern crate util;
#[macro_use] extern crate derivative;

use ::std::collections::HashMap;

mod pattern;
pub use self::pattern::{ PatternProvider, ExpandedClauseNodes };

mod cfg;
pub use self::cfg::{ PatternCfg, CfgEdge };

mod matrix;

pub mod simple_pattern;

use ::petgraph::graph::NodeIndex;

#[derive(Debug)]
pub struct MatchCompileContext<'a, P> where P: pattern::PatternProvider + 'a {
    pattern: &'a mut P,

    cfg: cfg::PatternCfg<P>,
    leaf_bindings: HashMap<NodeIndex, HashMap<P::CfgVariable, P::PatternNodeKey>>,

    root_matrix: matrix::MatchMatrix<P>,
    fail_leaf: NodeIndex,
}
impl<'a, P> MatchCompileContext<'a, P> where P: PatternProvider {

    pub fn new(pattern: &'a mut P) -> Self {
        let root = pattern.get_root();

        let mut cfg = cfg::PatternCfg::new();
        let fail_leaf = cfg.add_fail();
        let leaves: Vec<NodeIndex> = (0..(root.clauses))
            .map(|idx| cfg.add_leaf(idx))
            .collect();

        let leaf_bindings = leaves.iter()
            .map(|leaf| {
                let bindings: HashMap<P::CfgVariable, P::PatternNodeKey> = HashMap::new();
                (*leaf, bindings)
            })
            .collect();

        let root_matrix = matrix::MatchMatrix::new(
            &root.nodes, leaves, root.variables);

        MatchCompileContext {
            pattern: pattern,

            cfg: cfg,
            leaf_bindings: leaf_bindings,

            root_matrix: root_matrix,
            fail_leaf: fail_leaf,
        }
    }

    pub fn root_matrix(&self) -> &matrix::MatchMatrix<P> {
        &self.root_matrix
    }

}

fn matrix_to_decision_tree<P>(parent: cfg::CfgNodeIndex,
                              ctx: &mut MatchCompileContext<P>,
                              spec: P::PatternNodeKind,
                              matrix: &matrix::MatchMatrix<P>,
                              introduced_vars: Vec<P::CfgVariable>)
    where P: PatternProvider
{
    let edge = cfg::CfgEdge {
        kind: spec.clone(),
        variable_binds: introduced_vars,
    };

    // Matrix is empty, no specializations can be done.
    if matrix.is_empty() {
        ctx.cfg.add_edge(parent, ctx.fail_leaf, edge);
        return;
    }

    // If the head of the matrix has only wildcards, none of the other rows
    // can happen.
    if let Some(node) = matrix.has_wildcard_head(&ctx.pattern) {
        ctx.cfg.add_edge(parent, node, edge);
        return;
    }


    // Select the variable we should specialize on.
    // This will be the column with the most consecutive non-wildcards
    // at the head.
    let specialize_variable = matrix.select_specialize_variable(&ctx.pattern);
    let specialize_variable_cfg_var = matrix.get_var(specialize_variable);

    // Add new CFG node for current
    let cfg_node = ctx.cfg.add_child(parent, edge, specialize_variable_cfg_var);

    // Find what pattern types we have as children, so that we can
    // specialize and branch to them in the CFG
    let specialization_types = matrix.collect_specialization_types(
        &ctx.pattern, specialize_variable);

    // Specialize on specific matrices
    for specialization in specialization_types.iter() {
        let (introduced, specialized) = matrix.specialize(ctx, specialize_variable,
                                                          *specialization);

        // TODO: Dedup
        // Add variable bindings to the current specializations
        for (leaf, clause) in specialized.iterate_clauses() {
            let leaf_bindings = ctx.leaf_bindings.get_mut(&leaf).unwrap();
            for (variable_num, variable_node) in clause.iter().enumerate() {
                leaf_bindings.insert(specialized.get_var(variable_num), variable_node.node);
            }
        }

        matrix_to_decision_tree(
            cfg_node, ctx, *specialization,
            &specialized, introduced);
    }

    // Specialize on default matrix
    let (introduced, default) = matrix.default(ctx, specialize_variable);

    // TODO: Dedup
    // Add variable bindings to the current specializations
    for (leaf, clause) in default.iterate_clauses() {
        let leaf_bindings = ctx.leaf_bindings.get_mut(&leaf).unwrap();
        for (variable_num, variable_node) in clause.iter().enumerate() {
            leaf_bindings.insert(default.get_var(variable_num), variable_node.node);
        }
    }

    let wildcard = ctx.pattern.get_wildcard();
    matrix_to_decision_tree(
        cfg_node, ctx,
        wildcard,
        &default, introduced);

}

pub fn to_decision_tree<P>(pattern: &mut P) -> cfg::PatternCfg<P>
    where P: PatternProvider
{
    let mut context = MatchCompileContext::new(pattern);

    let root: matrix::MatchMatrix<P> = (*context.root_matrix()).clone();

    let root_cfg = context.cfg.get_entry();
    let wildcard = context.pattern.get_wildcard();

    matrix_to_decision_tree(root_cfg, &mut context,
                            wildcard,
                            &root,
                            root.variables.clone());

    let mut cfg = context.cfg;
    cfg.leaf_bindings = context.leaf_bindings;

    assert!(!::petgraph::algo::is_cyclic_directed(&cfg.graph));
    cfg
}
