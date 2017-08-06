use ::std::collections::HashSet;

use ::prettytable::Table;

use super::pattern::{ PatternNodeIndex, Pattern, PatternNodeKind };

#[derive(Debug)]
pub struct MatchMatrix {
    pub data: Vec<MatchMatrixElement>,

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

    pub fn new(nodes: &[super::pattern::PatternNodeIndex],
               leaves: Vec<super::cfg::CfgNodeIndex>,
               vars: Vec<super::cfg::PatternCfgVariable>) -> Self {
        assert!(vars.len() * leaves.len() == nodes.len());

        let data = if vars.len() == 0 {
            vec![]
        } else {
            nodes.chunks(vars.len()).enumerate()
                .flat_map(|(clause_idx, clause)| {
                    clause.iter().enumerate().map(move |(variable_idx, pat)| {
                        MatchMatrixElement {
                            variable_num: variable_idx,
                            clause_num: clause_idx,
                            node: *pat,
                        }
                    })
                }).collect()
        };

        MatchMatrix {
            data: data,
            variables: vars,
            clause_leaves: leaves,
        }
    }

    pub fn select_specialize_variable(&self, pattern: &Pattern) -> usize {
        let mut sums = vec![(0, true); self.variables.len()];

        let clauses = self.data.chunks(self.variables.len());
        for clause in clauses {
            for variable_pattern in clause.iter() {
                if pattern.node(variable_pattern.node).kind ==
                    PatternNodeKind::Wildcard {
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
                                        variable: usize) -> HashSet<&'a PatternNodeKind> {
        let mut types = HashSet::new();

        let clauses = self.data.chunks(self.variables.len());
        for clause in clauses {
            let variable_pattern = &clause[variable];
            types.insert(&pattern.node(variable_pattern.node).kind);
        }

        types
    }

    pub fn specialize(&self, ctx: &mut super::MatchCompileContext,
                      variable: usize,
                      on: &PatternNodeKind) -> (Vec<super::cfg::PatternCfgVariable>,
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
        for (clause_num, clause) in self.data.chunks(self.variables.len()).enumerate() {
            let pat = &clause[variable];
            let pat_node = ctx.pattern.node(pat.node);

            let is_wildcard = pat_node.kind == PatternNodeKind::Wildcard;
            let is_match = &pat_node.kind == on;

            if is_wildcard || is_match {
                final_clause_leaves.push(self.clause_leaves[clause_num]);

                if is_match {
                    let children: Vec<_> =
                        ctx.pattern.node_children(pat.node).collect();
                    assert!(children.len() == expanded_arity);

                    // Order of children in reverse additive order, as per docs.
                    // Need to reverse.
                    data.extend(children.iter().rev());
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

        let final_variables_num = self.variables.len() - 1 + expanded_arity;
        if final_variables_num * final_clause_leaves.len() == 0 {
            let new_mat = MatchMatrix::new(
                &[], final_clause_leaves, final_variables);
            (new_vars, new_mat)
        } else {
            let new_mat = MatchMatrix::new(
                &data, final_clause_leaves, final_variables);
            (new_vars, new_mat)
        }
    }

    pub fn default(&self, ctx: &mut super::MatchCompileContext,
                   variable: usize) -> (Vec<super::cfg::PatternCfgVariable>,
                                        MatchMatrix) {
        self.specialize(ctx, variable, &PatternNodeKind::Wildcard)
    }

    pub fn is_empty(&self) -> bool {
        self.clause_leaves.len() == 0
    }

    pub fn has_wildcard_head(&self, pattern: &Pattern
    ) -> Option<super::cfg::CfgNodeIndex> {

        assert!(self.clause_leaves.len() > 0);
        if self.variables.len() == 0 {
            Some(self.clause_leaves[0])
        } else {
            let has_wildcard_head = self.data
                .chunks(self.variables.len())
                .next().unwrap()
                .iter().all(|p| pattern.node(p.node).kind == PatternNodeKind::Wildcard);
            if has_wildcard_head {
                Some(self.clause_leaves[0])
            } else {
                None
            }
        }
    }

    pub fn to_table(&self, pat: &super::pattern::Pattern) -> Table {
        use ::prettytable::cell::Cell;

        let mut table = Table::new();

        {
            let head_row = table.add_empty_row();
            head_row.add_cell(Cell::new(&format!(
                "{}*{}=={}",
                self.variables.len(),
                self.clause_leaves.len(),
                self.data.len())));
            for variable in self.variables.iter() {
                let var_str = format!("{:?}", variable);
                head_row.add_cell(Cell::new(&var_str));
            }
        }

        for row_idx in 0..self.clause_leaves.len() {
            let t_row = table.add_empty_row();
            let leaf_id = format!("{:?}", self.clause_leaves[row_idx]);
            t_row.add_cell(Cell::new(&leaf_id));

            let row_start = row_idx * self.variables.len();
            for col in &self.data[row_start..(row_start+self.variables.len())] {
                let node = pat.node(col.node);
                let cell_fmt = format!("{:?}", node);
                let cell = Cell::new(&cell_fmt);
                t_row.add_cell(cell);
            }
        }

        table
    }

}
