use ::std::collections::HashSet;

use ::prettytable::Table;

use super::pattern::{ PatternNodeIndex, Pattern, PatternNode };

#[derive(Debug)]
pub struct MatchMatrix {
    pub data: Vec<MatchMatrixElement>,
    pub variables_len: usize,
    pub clauses_len: usize,

    pub variables: Vec<super::cfg::PatternCfgVariable>,
    pub clause_leaves: Vec<super::cfg::CfgNodeIndex>,
}

#[derive(Debug)]
pub struct MatchMatrixElement {
    pub node: PatternNodeIndex,
    pub variable_num: usize,
    pub clause_num: usize,
}

impl MatchMatrix {

    pub fn new(variables: usize, clauses: usize,
               nodes: &[super::pattern::PatternNodeIndex],
               leaves: Vec<super::cfg::CfgNodeIndex>,
               vars: Vec<super::cfg::PatternCfgVariable>) -> Self {
        assert!(variables * clauses == nodes.len());

        if variables * clauses == 0 {
            MatchMatrix {
                variables_len: 0,
                clauses_len: 0,
                data: vec![],
                variables: vec![],
                clause_leaves: vec![],
            }
        } else {
            MatchMatrix {
                variables_len: variables,
                clauses_len: clauses,
                data: nodes.chunks(variables).enumerate()
                    .flat_map(|(clause_idx, clause)| {
                        clause.iter().enumerate().map(move |(variable_idx, pat)| {
                            MatchMatrixElement {
                                variable_num: variable_idx,
                                clause_num: clause_idx,
                                node: *pat,
                            }
                        })
                    }).collect(),
                variables: vars,
                clause_leaves: leaves,
            }
        }
    }

    pub fn select_specialize_variable(&self, pattern: &Pattern) -> usize {
        let mut sums = vec![(0, true); self.variables_len];

        let clauses = self.data.chunks(self.variables_len);
        for clause in clauses {
            for variable_pattern in clause.iter() {
                if pattern.node(variable_pattern.node) == &PatternNode::Wildcard {
                    sums[variable_pattern.variable_num].1 = false;
                } else {
                    if sums[variable_pattern.variable_num].1 {
                        sums[variable_pattern.variable_num].0 += 1;
                    }
                }
            }
        }

        sums.iter().enumerate()
            .max_by_key(|&(_, s)| s)
            .map(|(i, _)| i).unwrap()
    }

    pub fn get_var(&self, var: usize) -> super::cfg::PatternCfgVariable {
        self.variables[var]
    }

    pub fn collect_specialization_types<'a>(&self, pattern: &'a Pattern,
                                        variable: usize) -> HashSet<&'a PatternNode> {
        let mut types = HashSet::new();

        let clauses = self.data.chunks(self.variables_len);
        for clause in clauses {
            let variable_pattern = &clause[variable];
            types.insert(pattern.node(variable_pattern.node));
        }

        types
    }

    pub fn specialize(&self, ctx: &mut super::MatchCompileContext,
                      variable: usize,
                      on: &PatternNode) -> (Vec<super::cfg::PatternCfgVariable>,
                                           MatchMatrix) {
        let wildcard = ctx.pattern.wildcard();
        let expanded_arity = on.children();

        let mut final_variables = Vec::new();
        let new_vars = ctx.expand_match(on);
        for var in new_vars.iter() {
            final_variables.push(*var);
        }
        for var in self.variables.iter().take(variable) {
            final_variables.push(*var);
        }
        for var in self.variables.iter().skip(variable+1) {
            final_variables.push(*var);
        }

        let mut final_clause_leaves = Vec::new();
        let mut data = Vec::new();
        for (clause_num, clause) in self.data.chunks(self.variables_len).enumerate() {
            let pat = &clause[variable];
            let pat_node = ctx.pattern.node(pat.node);

            let is_wildcard = pat_node == &PatternNode::Wildcard;
            let is_match = pat_node == on;

            if is_wildcard || is_match {
                final_clause_leaves.push(self.clause_leaves[clause_num]);

                if is_match {
                    let children = ctx.pattern.node_children(pat.node);
                    assert!(children.clone().count() == expanded_arity);
                    data.extend(children);
                } else {
                    data.extend((0..expanded_arity).map(|_| wildcard));
                }

                for clause_pat in clause.iter().take(variable) {
                    data.push(clause_pat.node);
                }
                for clause_pat in clause.iter().skip(variable+1) {
                    data.push(clause_pat.node);
                }
            }
        }

        //let data: Vec<_> = self.data
        //    .chunks(self.variables_len)
        //    .flat_map(|clause| {
        //        let pat = &clause[variable];
        //        let pat_node = ctx.pattern.node(pat.node);
        //        if pat_node == on || pat_node == PatternNode::Wildcard {
        //            final_clauses += 1;
        //            let e1 = clause.iter().take(variable).map(|p| p.node);
        //            let e2 = if pat_node == on {
        //                Either::Left(ctx.pattern.node_children(pat.node))
        //            } else {
        //                let vals = vec![wildcard; expanded_arity];
        //                Either::Right(vals.into_iter())
        //            };
        //            let e3 = clause.iter().skip(variable+1).map(|p| p.node);
        //            Either::Left(e1.chain(e2).chain(e3))
        //        } else {
        //            Either::Right(vec![].into_iter())
        //        }
        //    })
        //    .collect();

        let final_variables_num = self.variables_len - 1 + expanded_arity;
        if final_variables_num * final_clause_leaves.len() == 0 {
            let new_mat = MatchMatrix::new(
                0, 0, &[], final_clause_leaves, final_variables);
            (new_vars, new_mat)
        } else {
            let new_mat = MatchMatrix::new(
                final_variables_num, final_clause_leaves.len(),
                &data, final_clause_leaves, final_variables);
            (new_vars, new_mat)
        }
    }

    pub fn default(&self, ctx: &mut super::MatchCompileContext,
                   variable: usize) -> (Vec<super::cfg::PatternCfgVariable>,
                                        MatchMatrix) {
        self.specialize(ctx, variable, &PatternNode::Wildcard)
    }

    pub fn is_empty(&self) -> bool {
        self.clauses_len == 0
    }

    pub fn has_wildcard_head(&self, pattern: &Pattern
    ) -> Option<super::cfg::CfgNodeIndex> {

        assert!(self.clauses_len > 0);
        if self.variables_len == 0 {
            Some(self.clause_leaves[0])
        } else {
            let has_wildcard_head = self.data
                .chunks(self.variables_len)
                .next().unwrap()
                .iter().all(|p| pattern.node(p.node) == &PatternNode::Wildcard);
            if has_wildcard_head {
                Some(self.clause_leaves[0])
            } else {
                None
            }
        }
    }

    pub fn to_table(&self, ctx: &super::MatchCompileContext) -> Table {
        use ::prettytable::cell::Cell;

        let mut table = Table::new();

        {
            let head_row = table.add_empty_row();
            head_row.add_cell(Cell::new(&format!("{}*{}=={}", self.variables_len,
                                                 self.clauses_len, self.data.len())));
            for variable in self.variables.iter() {
                let var_str = format!("{:?}", variable);
                head_row.add_cell(Cell::new(&var_str));
            }
        }

        if self.variables_len != 0 {
            for (row_idx, row) in self.data.chunks(self.variables_len).enumerate() {
                let t_row = table.add_empty_row();
                let leaf_id = format!("{:?}", self.clause_leaves[row_idx]);
                t_row.add_cell(Cell::new(&leaf_id));
                for col in row.iter() {
                    let node = ctx.pattern.node(col.node);
                    let cell_fmt = format!("{:?}", node);
                    let cell = Cell::new(&cell_fmt);
                    t_row.add_cell(cell);
                }
            }
        }

        table
    }

}
