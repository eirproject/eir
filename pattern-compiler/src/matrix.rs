use ::std::collections::HashSet;

use ::prettytable::Table;

use ::petgraph::graph::NodeIndex;

use ::either::Either;

use super::pattern::PatternProvider;

#[derive(Debug, Derivative)]
#[derivative(Clone(bound=""))]
pub struct MatchMatrix<P> where P: PatternProvider {
    pub data: Vec<MatchMatrixElement<P>>,

    pub variables: Vec<P::CfgVariable>,
    pub clause_leaves: Vec<super::cfg::CfgNodeIndex>,
}

#[derive(Debug, Derivative)]
#[derivative(Clone(bound=""))]
pub struct MatchMatrixElement<P> where P: PatternProvider {
    pub node: P::PatternNodeKey,
    pub variable_num: usize,
    pub clause_num: usize,
}

fn chunks_len<'a, T>(entities: &'a [T], chunk_len: usize, num_chunks: usize)
                     -> Box<Iterator<Item = &'a [T]> + 'a> {

    assert!(entities.len() == (chunk_len * num_chunks));
    let ret = if chunk_len == 0 {
        Either::Left((0..num_chunks).map(|_| [].as_ref()))
    } else {
        Either::Right(entities.chunks(chunk_len))
    };
    Box::new(ret)
}

impl<P> MatchMatrix<P> where P: PatternProvider {

    pub fn new(nodes: &[P::PatternNodeKey],
               leaves: Vec<super::cfg::CfgNodeIndex>,
               vars: Vec<P::CfgVariable>) -> Self {
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

    /// Selects which variable should be specialized on in this matrix.
    /// This will always select the variable which has the most consecutive
    /// wildcards from the top, as this will minimize the amount of
    /// comparisons we will have to perform.
    pub fn select_specialize_variable(&self, pattern: &P) -> usize
    {
        let mut sums = vec![(0, true); self.variables.len()];

        let clauses = self.data.chunks(self.variables.len());
        for clause in clauses {
            for variable_pattern in clause.iter() {
                if pattern.is_wildcard(pattern.get_kind(variable_pattern.node)) {
                    sums[variable_pattern.variable_num].1 = false;
                } else {
                    if sums[variable_pattern.variable_num].1 {
                        sums[variable_pattern.variable_num].0 += 1;
                    }
                }
            }
        }

        sums.iter().enumerate()
            .max_by_key(|&(_, s)| s.0)
            .map(|(i, _)| i).unwrap()
    }

    pub fn get_var(&self, var: usize) -> P::CfgVariable {
        self.variables[var]
    }

    /// Constructs a set of all node kinds in the given variable in the given pattern
    pub fn collect_specialization_types<'a>(&self, pattern: &'a P,
                                            variable: usize)
                                            -> HashSet<P::PatternNodeKind>
    {
        let mut types = HashSet::new();

        let clauses = self.data.chunks(self.variables.len());
        for clause in clauses {
            let variable_pattern = &clause[variable];
            types.insert(pattern.get_kind(variable_pattern.node));
        }

        types.remove(&pattern.get_wildcard());

        types
    }

    pub fn specialize(&self, ctx: &mut super::MatchCompileContext<P>,
                         variable: usize,
                         on: P::PatternNodeKind)
                         -> (Vec<P::CfgVariable>, MatchMatrix<P>)
    {
        //println!("Specialize variable #{} on {:?}", variable, on);
        //println!("{}", self.to_table(&ctx.pattern));

        // 1: Collect rows that should be included in the specialization
        let to_specialize_rows: Vec<(usize, &[MatchMatrixElement<_>])> = self.data
            .chunks(self.variables.len())
            .enumerate()
            .filter(|&(_, node)| ctx.pattern.kind_includes(on, node[variable].node))
            .collect();

        // 2: Split rows into list of specialization nodes, and the rest
        let specialize_nodes: Vec<&MatchMatrixElement<_>> = to_specialize_rows.iter()
            .map(|&(_, nodes)| &nodes[variable])
            .collect();
        let rest_nodes: Vec<Vec<&MatchMatrixElement<_>>> = to_specialize_rows.iter()
            .map(|&(_, nodes)| {
                nodes.iter()
                    .filter(|node| node.variable_num != variable)
                    .collect::<Vec<_>>()
            })
            .collect();

        // 3: Generate specialized by PatternProvider::expand_clause_nodes
        let specialized = {
            let nodes: Vec<_> = specialize_nodes.iter()
                .map(|node| node.node)
                .collect();
            ctx.pattern.expand_clause_nodes(nodes)
        };

        // 4: Merge specialized with rest from step 2 and return new matrix
        //let specialized_chunked = specialized.nodes
        //    .chunks(specialized.variables.len());
        let specialized_chunked = chunks_len(&specialized.nodes, specialized.variables.len(),
                                             specialized.clauses);

        let new_nodes: Vec<_> = specialized_chunked
            .zip(rest_nodes.iter())
            .flat_map(|(specialized, rest)| {
                let specialized_m = specialized.iter().map(|node| *node);
                let rest_m = rest.iter().map(|node| node.node);

                specialized_m.chain(rest_m)
            })
            .collect();

        let new_clause_leaves: Vec<_> = to_specialize_rows.iter()
            .map(|&(clause_num, _)| self.clause_leaves[clause_num])
            .collect();

        let new_variables: Vec<_> = {
            let rest_variables = self.variables.iter()
                .enumerate()
                .filter(|&(var_num, _)| var_num != variable)
                .map(|(_, var)| *var);

            specialized.variables.iter()
                .map(|var| *var)
                .chain(rest_variables)
                .collect()
        };

        let new_mat = Self::new(new_nodes.as_slice(),
                                new_clause_leaves, new_variables);
        (specialized.variables, new_mat)
    }

    pub fn without_head<'a>(&'a self) -> MatchMatrix<P> {
        // TODO move to actual new instead of this
        // This just removes the top row of the table.
        // Since data is unrolled, remove the n first entries
        // where n is the number of variables
        // Then remove first clause leaf
        let mut new = self.clone();
        for _ in 0..(new.variables.len()) {
            new.data.remove(0);
        }
        new.clause_leaves.remove(0);
        for entry in new.data.iter_mut() {
            entry.clause_num -= 1;
        }
        new
    }

    pub fn iterate_clauses<'a>(&'a self) -> Box<Iterator<Item = (NodeIndex, &'a [MatchMatrixElement<P>])> + 'a> {
        let iter = self.clause_leaves.iter().map(|l| *l)
            .zip(chunks_len(&self.data, self.variables.len(), self.clause_leaves.len()));

        Box::new(iter)
    }

    pub fn default(&self, ctx: &mut super::MatchCompileContext<P>,
                   variable: usize)
                   -> (Vec<P::CfgVariable>, MatchMatrix<P>)
    {
        let wildcard = ctx.pattern.get_wildcard();
        self.specialize(ctx, variable, wildcard)
    }

    pub fn is_empty(&self) -> bool {
        self.clause_leaves.len() == 0
    }

    pub fn has_wildcard_head(&self, pattern: &P)
                             -> Option<super::cfg::CfgNodeIndex>
    {

        assert!(self.clause_leaves.len() > 0);
        if self.variables.len() == 0 {
            Some(self.clause_leaves[0])
        } else {
            let has_wildcard_head = self.data
                .chunks(self.variables.len())
                .next().unwrap()
                .iter().all(|p| {
                    pattern.is_wildcard(pattern.get_kind(p.node))
                });
            if has_wildcard_head {
                Some(self.clause_leaves[0])
            } else {
                None
            }
        }
    }

    pub fn to_table(&self, pat: &P) -> Table {
        use ::prettytable::Cell;

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
                let node = pat.get_kind(col.node);
                let cell_fmt = format!("{:?}", node);
                let cell = Cell::new(&cell_fmt);
                t_row.add_cell(cell);
            }
        }

        table
    }

}
