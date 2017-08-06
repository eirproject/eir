// Implements a variant of
// http://www.cs.tufts.edu/~nr/cs257/archive/luc-maranget/jun08.pdf

use ::std::collections::HashMap;

mod pattern;
mod cfg;
mod matrix;

#[cfg(test)]
mod test;

use ::petgraph::graph::NodeIndex;

#[derive(Debug)]
pub struct MatchCompileContext<'a> {
    pattern: &'a pattern::Pattern,
    cfg: cfg::PatternCfg,
    pattern_var_bindings: HashMap<pattern::PatternVariable, cfg::PatternCfgVariable>,

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
            .enumerate()
            .map(|(idx, _)| cfg.add_leaf(idx))
            .collect();

        MatchCompileContext {
            input_variables: (0..(pattern.dimentions().0))
                .map(|_| cfg.new_variable())
                .collect(),
            fail_leaf: fail_leaf,
            leaves: leaves,
            cfg: cfg,
            pattern: pattern,
            pattern_var_bindings: HashMap::new(),
        }
    }

    pub fn add_var_binding(&mut self, pat_var: pattern::PatternVariable,
                           cfg_var: cfg::PatternCfgVariable) {
        if pat_var.0 != 0 {
            if let Some(v) = self.pattern_var_bindings.get(&pat_var).cloned() {
                assert!(v == cfg_var);
            } else {
                self.pattern_var_bindings.insert(pat_var, cfg_var);
            }
        }
    }

    pub fn add_matrix_bindings(&mut self, mat: &matrix::MatchMatrix) {
        for (clause_num, _clause) in mat.clause_leaves.iter().enumerate() {
            for (pat_num, cfg_var) in mat.variables.iter().enumerate() {
                let elem = &mat.data[(mat.variables.len()*clause_num) + pat_num];
                let pat_var = self.pattern.node(elem.node).bind;
                self.add_var_binding(pat_var, *cfg_var);
            }
        }
    }

    pub fn root_matrix(&mut self) -> matrix::MatchMatrix {
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
            data: data,
            variables: self.input_variables.clone(),
            clause_leaves: self.leaves.clone(),
        }
    }

    pub fn expand_match(&mut self,
                        on: &pattern::PatternNodeKind) -> Vec<cfg::PatternCfgVariable> {
        let res = (0..(on.children()))
            .map(|_| self.cfg.new_variable())
            .collect();
        println!("{:?}", res);
        res
    }

}

fn matrix_to_decision_tree(parent: cfg::CfgNodeIndex, ctx: &mut MatchCompileContext,
                           spec: &pattern::PatternNodeKind,
                           spec_t: cfg::PatternCfgVariable,
                           matrix: &matrix::MatchMatrix,
                           introduced_vars: Vec<cfg::PatternCfgVariable>, lvl: u32) {
    //println!("{} - {:?}, {:?}:", lvl, spec_t, spec);
    //matrix.to_table(ctx.pattern).printstd();

    ctx.add_matrix_bindings(matrix);

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
    let specialize_variable_t = matrix.get_var(specialize_variable);

    let cfg_node = ctx.cfg.add_child(parent, edge, specialize_variable_t);

    let mut specialization_types = matrix.collect_specialization_types(
        &ctx.pattern, specialize_variable);
    specialization_types.remove(&pattern::PatternNodeKind::Wildcard);

    for specialization in specialization_types.iter() {
        let (introduced, specialized) = matrix.specialize(ctx, specialize_variable,
                                                          specialization);
        matrix_to_decision_tree(cfg_node, ctx, *specialization,
                                specialize_variable_t, &specialized,
                                introduced, lvl+1);
    }

    let (introduced, default) = matrix.default(ctx, specialize_variable);
    matrix_to_decision_tree(cfg_node, ctx, &pattern::PatternNodeKind::Wildcard,
                            specialize_variable_t, &default, introduced, lvl+1);

}

pub fn to_decision_tree(pattern: &pattern::Pattern) -> cfg::PatternCfg {
    let mut context = MatchCompileContext::new(pattern);
    //println!("{:#?}", context);

    let root = context.root_matrix();

    let root_cfg = context.cfg.add_root();
    matrix_to_decision_tree(root_cfg, &mut context,
                            &pattern::PatternNodeKind::Wildcard,
                            cfg::PatternCfgVariable(0), &root,
                            root.variables.clone(), 0);

    //println!("{:#?}", context.pattern_var_bindings);

    //use ::std::io::Write;
    //let dot = ::petgraph::dot::Dot::new(&context.cfg.graph);
    //let mut dot_file = ::std::fs::File::create("pat_cfg.dot").unwrap();
    //write!(dot_file, "{:?}", dot).unwrap();

    context.cfg
}
