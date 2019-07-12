// Implements a variant of
// http://www.cs.tufts.edu/~nr/cs257/archive/luc-maranget/jun08.pdf

#[macro_use] extern crate derivative;

pub use petgraph::visit::EdgeRef;

mod pattern;
pub use self::pattern::{ PatternProvider, ExpandedClauseNodes };

mod cfg;
pub use self::cfg::{ PatternCfg, CfgEdge, CfgNodeKind, CfgNodeIndex };

mod matrix;

pub mod simple_pattern;

pub use ::petgraph::graph::NodeIndex;

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
struct LeafId(usize);

#[derive(Debug)]
pub struct MatchCompileContext<'a, P> where P: pattern::PatternProvider + 'a {
    pattern: &'a mut P,

    cfg: cfg::PatternCfg<P>,
    //leaf_bindings: HashMap<NodeIndex, HashMap<P::PatternNodeKey, P::CfgVariable>>,

    root_matrix: matrix::MatchMatrix<P>,
    fail_leaf: NodeIndex,
}
impl<'a, P> MatchCompileContext<'a, P> where P: PatternProvider {

    pub fn new(pattern: &'a mut P) -> Self {
        let root = pattern.get_root();

        let mut cfg = cfg::PatternCfg::new();
        let fail_leaf = cfg.add_fail();
        let leaves: Vec<LeafId> = (0..(root.clauses))
            .map(|idx| LeafId(idx))
            .collect();

        //let leaf_bindings = leaves.iter()
        //    .map(|leaf| {
        //        let bindings: HashMap<P::PatternNodeKey, P::CfgVariable> = HashMap::new();
        //        (*leaf, bindings)
        //    })
        //    .collect();

        let root_matrix = matrix::MatchMatrix::new(
            &root.nodes, leaves, root.variables);

        MatchCompileContext {
            pattern: pattern,

            cfg: cfg,
            //leaf_bindings: leaf_bindings,

            root_matrix: root_matrix,
            fail_leaf: fail_leaf,
        }
    }

    fn root_matrix(&self) -> &matrix::MatchMatrix<P> {
        &self.root_matrix
    }

}

fn matrix_to_decision_tree<P>(parent: cfg::CfgNodeIndex,
                              ctx: &mut MatchCompileContext<P>,
                              spec: Option<P::PatternNodeKind>,
                              matrix: &matrix::MatchMatrix<P>,
                              introduced_vars: Vec<P::CfgVariable>, level: usize)
    where P: PatternProvider
{


    #[cfg(feature = "debug_table_print")]
    {
        for _ in 0..level {
            print!(" ==");
        }
        println!(" MATRIX AT LEVEL {}", level);
    }

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
    if let Some(node_id) = matrix.has_wildcard_head(&ctx.pattern) {
        let binds = matrix.binds_for(node_id).unwrap();
        let node = ctx.cfg.add_leaf(parent, node_id.0, edge, binds.clone());

        let new_mat = matrix.without_head();
        matrix_to_decision_tree(node, ctx, None, &new_mat, vec![], level+1);

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

        matrix_to_decision_tree(
            cfg_node, ctx, Some(*specialization),
            &specialized, introduced, level+1);
    }

    // Specialize on default matrix
    let (introduced, default) = matrix.default(ctx, specialize_variable);

    let wildcard = ctx.pattern.get_wildcard();
    matrix_to_decision_tree(
        cfg_node, ctx,
        Some(wildcard),
        &default, introduced, level+1);

}

pub fn to_decision_tree<P>(pattern: &mut P) -> cfg::PatternCfg<P>
    where P: PatternProvider
{
    let mut context = MatchCompileContext::new(pattern);

    let root: matrix::MatchMatrix<P> = (*context.root_matrix()).clone();

    let root_cfg = context.cfg.get_entry();
    let wildcard = context.pattern.get_wildcard();

    matrix_to_decision_tree(root_cfg, &mut context,
                            Some(wildcard),
                            &root,
                            root.variables.clone(), 0);

    let cfg = context.cfg;
    assert!(!::petgraph::algo::is_cyclic_directed(&cfg.graph));
    cfg
}
