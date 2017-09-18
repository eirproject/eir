// Implements a variant of
// http://www.cs.tufts.edu/~nr/cs257/archive/luc-maranget/jun08.pdf

extern crate petgraph;
extern crate prettytable;
extern crate either;

use ::std::collections::HashMap;

mod pattern;
use self::pattern::{ PatternProvider, ExpandedClauseNodes };

pub mod cfg;
mod matrix;

mod simple_pattern;

use ::petgraph::graph::NodeIndex;

#[derive(Debug)]
pub struct MatchCompileContext<'a, P> where P: pattern::PatternProvider + 'a {
    pattern: &'a mut P,

    cfg: cfg::PatternCfg<P>,
    pattern_var_bindings: HashMap<P::PatternNodeKey, cfg::PatternCfgVariable>,

    root_matrix: matrix::MatchMatrix<P>,
    fail_leaf: NodeIndex,
}
impl<'a, P> MatchCompileContext<'a, P> where P: PatternProvider {

    pub fn new(pattern: &'a mut P) -> Self {
        let root = pattern.get_root();


        //let clauses = pattern.clauses();

        let mut cfg = cfg::PatternCfg::new();
        let fail_leaf = cfg.add_fail();
        //let leaves = clauses.iter()
        //    .enumerate()
        //    .map(|(idx, _)| cfg.add_leaf(idx))
        //    .collect();
        let leaves = (0..(root.clauses))
            .map(|idx| cfg.add_leaf(idx))
            .collect();

        let root_matrix = matrix::MatchMatrix::new(
            &root.nodes, leaves, root.variables);

        MatchCompileContext {
            pattern: pattern,

            cfg: cfg,
            pattern_var_bindings: HashMap::new(),

            root_matrix: root_matrix,
            fail_leaf: fail_leaf,
        }
    }

    pub fn add_var_binding(&mut self, pat_var: P::PatternNodeKey,
                           cfg_var: cfg::PatternCfgVariable) {
        if let Some(v) = self.pattern_var_bindings.get(&pat_var).cloned() {
            assert!(v == cfg_var);
        } else {
            self.pattern_var_bindings.insert(pat_var, cfg_var);
        }
    }

    pub fn add_matrix_bindings(&mut self, mat: &matrix::MatchMatrix<P>) {
        //for (clause_num, _clause) in mat.clause_leaves.iter().enumerate() {
        //    for (pat_num, cfg_var) in mat.variables.iter().enumerate() {
        //        let elem = &mat.data[(mat.variables.len()*clause_num) + pat_num];
        //        let pat_var = self.pattern.node(elem.node).bind;
        //        self.add_var_binding(pat_var, *cfg_var);
        //    }
        //}
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
    //println!("{} - {:?}, {:?}:", lvl, spec_t, spec);
    //matrix.to_table(ctx.pattern).printstd();

    ctx.add_matrix_bindings(matrix);

    let edge = cfg::CfgEdge {
        kind: spec.clone(),
        variable_binds: introduced_vars,
        //pattern_node: unimplemented!(),
    };

    if matrix.is_empty() {
        ctx.cfg.add_edge(parent, ctx.fail_leaf, edge);
        return;
    }

    if let Some(node) = matrix.has_wildcard_head(&ctx.pattern) {
        ctx.cfg.add_edge(parent, node, edge);
        return;
    }

    let specialize_variable = matrix.select_specialize_variable(&ctx.pattern);
    let specialize_variable_t = matrix.get_var(specialize_variable);

    let cfg_node = ctx.cfg.add_child(parent, edge, specialize_variable_t);

    let specialization_types = matrix.collect_specialization_types(
        &ctx.pattern, specialize_variable);

    for specialization in specialization_types.iter() {
        let (introduced, specialized) = matrix.specialize(ctx, specialize_variable,
                                                          *specialization);
        matrix_to_decision_tree(
            cfg_node, ctx, *specialization,
            &specialized, introduced);
    }

    let (introduced, default) = matrix.default(ctx, specialize_variable);
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
    //println!("{:#?}", context);

    let root = context.root_matrix().clone();

    let root_cfg = context.cfg.get_entry();
    let wildcard = context.pattern.get_wildcard();
    matrix_to_decision_tree(root_cfg, &mut context,
                            wildcard,
                            &root,
                            root.variables.clone());

    //println!("{:#?}", context.pattern_var_bindings);

    //use ::std::io::Write;
    //let dot = ::petgraph::dot::Dot::new(&context.cfg.graph);
    //let mut dot_file = ::std::fs::File::create("pat_cfg.dot").unwrap();
    //write!(dot_file, "{:?}", dot).unwrap();

    let cfg = context.cfg;
    assert!(!::petgraph::algo::is_cyclic_directed(&cfg.graph));
    cfg
}
