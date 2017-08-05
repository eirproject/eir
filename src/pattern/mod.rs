// Implements a variant of
// http://www.cs.tufts.edu/~nr/cs257/archive/luc-maranget/jun08.pdf

mod pattern;
mod cfg;
mod matrix;

#[cfg(test)]
mod test;

use ::std::collections::HashSet;
use ::std::slice::{ Chunks, ChunksMut };

use ::petgraph::Graph;
use ::petgraph::graph::NodeIndex;

use ::parser::AtomicLiteral;
use ::ir::hir::{ Clause, Pattern, PatternNode };

#[derive(Debug)]
pub struct MatchCompileContext<'a> {
    pattern: &'a pattern::Pattern,
    cfg: cfg::PatternCfg,

    // Outcomes
    leaves: Vec<NodeIndex>,
    fail_leaf: NodeIndex,
    input_variables: Vec<cfg::PatternCfgVariable>,
}
impl<'a> MatchCompileContext<'a> {

    pub fn new(pattern: &'a pattern::Pattern) -> Self {
        let clauses = pattern.clauses();

        let mut cfg = cfg::PatternCfg::new();
        let fail_leaf = cfg.add_fail();
        let leaves = clauses.iter()
            .map(|_| cfg.add_leaf())
            .collect();

        MatchCompileContext {
            input_variables: (0..(pattern.dimentions().0))
                .map(|_| cfg.new_variable())
                .collect(),
            fail_leaf: fail_leaf,
            leaves: leaves,
            cfg: cfg,
            pattern: pattern,
        }
    }

    pub fn root_matrix(&self) -> matrix::MatchMatrix {
        let clauses = self.pattern.clauses();
        let data = clauses.iter()
            .enumerate()
            .flat_map(|(clause_num, clause)| {
                clause.iter()
                    .enumerate()
                    .map(move |(var_num, pat)| {
                        matrix::MatchMatrixElement {
                            node: *pat,
                            variable_num: var_num,
                            clause_num: clause_num,
                        }
                    })
            })
            .collect();
        matrix::MatchMatrix {
            variables_len: self.pattern.dimentions().0,
            clauses_len: self.pattern.dimentions().1,
            data: data,
            variables: self.input_variables.clone(),
            clause_leaves: self.leaves.clone(),
        }
    }

    pub fn expand_match(&mut self,
                        on: &pattern::PatternNode) -> Vec<cfg::PatternCfgVariable> {
        (0..(on.children())).map(|_| self.cfg.new_variable()).collect()
    }

}

//fn matrix_add_child(parent: cfg::CfgNodeIndex, ctx: &mut MatchCompileContext,
//                    spec: pattern::PatternNode, spec_t: cfg::PatternCfgVariable,
//                    matrix: &matrix::MatchMatrix) {
//    if matrix.is_empty() {
//        ctx.cfg.add_edge(parent, ctx.fail_leaf, spec);
//        return;
//    }
//
//    if let Some(node) = matrix.has_wildcard_head(&ctx.pattern) {
//        ctx.cfg.add_edge(parent, node, spec);
//        return;
//    }
//
//    let cfg_node = ctx.cfg.add_child(parent, spec, spec_t);
//    matrix_to_decision_tree(cfg_node, ctx, matrix);
//}

fn matrix_to_decision_tree(parent: cfg::CfgNodeIndex, ctx: &mut MatchCompileContext,
                           spec: &pattern::PatternNode,
                           spec_t: cfg::PatternCfgVariable,
                           matrix: &matrix::MatchMatrix,
                           introduced_vars: Vec<cfg::PatternCfgVariable>, lvl: u32) {
    println!("{} - {:?}, {:?}:", lvl, spec_t, spec);
    matrix.to_table(ctx).printstd();

    let edge = cfg::CfgEdge {
        kind: spec.clone(),
        variable_binds: introduced_vars,
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
    //println!("{}", specialize_variable);
    let specialize_variable_t = matrix.get_var(specialize_variable);

    let cfg_node = ctx.cfg.add_child(parent, edge, specialize_variable_t);

    let mut specialization_types = matrix.collect_specialization_types(
        &ctx.pattern, specialize_variable);
    specialization_types.remove(&pattern::PatternNode::Wildcard);

    for specialization in specialization_types.iter() {
        let (introduced, specialized) = matrix.specialize(ctx, specialize_variable,
                                                          specialization);
        matrix_to_decision_tree(cfg_node, ctx, *specialization,
                                specialize_variable_t, &specialized,
                                introduced, lvl+1);
        //matrix_add_child(parent, ctx, *specialization, specialize_variable_t,
        //                 &specialized)
    }

    let (introduced, default) = matrix.default(ctx, specialize_variable);
    matrix_to_decision_tree(cfg_node, ctx, &pattern::PatternNode::Wildcard,
                            specialize_variable_t, &default, introduced, lvl+1);
    //matrix_add_child(parent, ctx, pattern::PatternNode::Wildcard,
    //                 specialize_variable_t, &default);

}

pub fn to_decision_tree(pattern: &pattern::Pattern) {
    let mut context = MatchCompileContext::new(pattern);
    println!("{:#?}", context);

    let root = context.root_matrix();
    //root.to_table(&context).printstd();
    //println!("{:#?}", root);

    let root_cfg = context.cfg.add_root();
    matrix_to_decision_tree(root_cfg, &mut context, &pattern::PatternNode::Wildcard,
                            cfg::PatternCfgVariable(0), &root,
                            root.variables.clone(), 0);

    use ::std::io::Write;
    let dot = ::petgraph::dot::Dot::new(&context.cfg.graph);
    let mut dot_file = ::std::fs::File::create("pat_cfg.dot").unwrap();
    write!(dot_file, "{:?}", dot).unwrap();
}
