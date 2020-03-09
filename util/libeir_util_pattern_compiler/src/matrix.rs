use std::collections::{ HashMap, HashSet };

#[cfg(feature = "debug_table_print")]
use prettytable::Table;


#[cfg(feature = "debug_table_print")]
use log::trace;
#[cfg(feature = "debug_table_print")]
use super::TARGET;

use super::LeafId;
use super::pattern::PatternProvider;

#[derive(Debug, Derivative)]
#[derivative(Clone(bound=""))]
pub(crate) struct MatchMatrix<P> where P: PatternProvider {
    pub data: Vec<MatchMatrixElement<P>>,

    pub variables: Vec<P::CfgVariable>,
    pub clause_leaves: Vec<LeafId>,

    pub leaf_bindings: Vec<HashMap<P::PatternNodeKey, P::CfgVariable>>,
}

#[derive(Debug, Derivative)]
#[derivative(Clone(bound=""))]
pub struct MatchMatrixElement<P> where P: PatternProvider {
    pub node: P::PatternNodeKey,
    pub variable_num: usize,
    pub clause_num: usize,
}

//fn chunks_len<'a, T>(entities: &'a [T], chunk_len: usize, num_chunks: usize)
//                     -> impl Iterator<Item = &'a [T]> + 'a {
//
//    assert!(entities.len() == (chunk_len * num_chunks));
//    let ret = if chunk_len == 0 {
//        Either::Left((0..num_chunks).map(|_| [].as_ref()))
//    } else {
//        Either::Right(entities.chunks(chunk_len))
//    };
//    ret
//}

impl<P> MatchMatrix<P> where P: PatternProvider {

    pub fn new(nodes: &[P::PatternNodeKey],
               leaves: Vec<LeafId>,
               vars: Vec<P::CfgVariable>,
    ) -> Self
    {
        let binds = (0..leaves.len())
            .map(|_| HashMap::new())
            .collect();
        MatchMatrix::with_bindings(
            nodes, leaves, vars, binds,
        )
    }

    fn with_bindings(nodes: &[P::PatternNodeKey],
                     leaves: Vec<LeafId>,
                     vars: Vec<P::CfgVariable>,
                     mut leaf_bindings: Vec<HashMap<P::PatternNodeKey, P::CfgVariable>>,
    ) -> Self
    {
        assert!(vars.len() * leaves.len() == nodes.len());
        assert!(leaf_bindings.len() == leaves.len());

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

        // Insert all bindings
        if nodes.len() > 0 {
            for (idx, chunk) in data.chunks(vars.len()).enumerate() {
                for (elem, var) in chunk.iter().zip(vars.iter()) {
                    leaf_bindings[idx].insert(elem.node, *var);
                }
            }
        }

        MatchMatrix {
            data: data,
            variables: vars,
            clause_leaves: leaves,
            leaf_bindings: leaf_bindings,
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
        #[cfg(feature = "debug_table_print")]
        {
            trace!(target: TARGET, "Specialize variable #{} on {:?}", variable, on);
            trace!(target: TARGET, "{}", self.to_table(&ctx.pattern));
            trace!(target: TARGET, "binds: {:#?}", self.leaf_bindings);
        }

        // 1. Determine how we want to handle the different clauses
        #[derive(Debug, Copy, Clone, PartialEq, Eq)]
        enum ClauseMode {
            /// The clause is included in the kind we specialize on
            Expand,
            /// The clause is a wildcard, expand into wildcards
            Wildcard,
            /// Clause is not included
            Skip,
        }
        let clause_modes: Vec<ClauseMode> = self.data
            .chunks(self.variables.len())
            .map(|nodes| {
                let n = nodes[variable].node;
                if ctx.pattern.kind_includes(on, n) {
                    ClauseMode::Expand
                } else if ctx.pattern.is_wildcard(ctx.pattern.get_kind(n)) {
                    ClauseMode::Wildcard
                } else {
                    ClauseMode::Skip
                }
            })
            .collect();

        // 2. Expand nodes
        let expanded = {
            let nodes: Vec<_> = clause_modes.iter().cloned()
                .zip(self.data.chunks(self.variables.len()))
                .filter(|(mode, _)| *mode == ClauseMode::Expand)
                .map(|(_, nodes)| nodes[variable].node)
                .collect();
            let nodes_len = nodes.len();

            let res = ctx.pattern.expand_clause_nodes(nodes, on);

            assert!(res.clauses == nodes_len);
            assert!(res.nodes.len() == res.clauses * res.variables.len());

            res
        };

        let expanded_var_num = expanded.variables.len();

        // 3. Merge expanded with wildcard expansions and residuals
        let merged = {
            let mut out = Vec::new();

            // Because .chunks does not accept 0 as a chunk length,
            // we do this workaround
            let mut expanded_iter = if expanded.nodes.len() > 0 {
                Some(expanded.nodes
                     .chunks(expanded_var_num))
            } else {
                None
            };

            let mut residual_iter = clause_modes.iter().cloned()
                .zip(self.data.chunks(self.variables.len()))
                .filter(|(mode, _)| *mode != ClauseMode::Skip)
                .map(|(_, elems)| {
                    elems.iter()
                        .filter(|elem| elem.variable_num != variable)
                        .map(|elem| elem.node)
                });

            for mode in clause_modes.iter() {
                // Append expanded
                match mode {
                    ClauseMode::Expand => {
                        if let Some(ref mut inner) = expanded_iter {
                            let expanded_nodes = inner.next().unwrap();
                            out.extend(expanded_nodes.iter().cloned());
                        } else {
                            assert!(expanded.nodes.len() == 0);
                        }
                    }
                    ClauseMode::Wildcard => {
                        for _ in 0..expanded_var_num {
                            out.push(ctx.pattern.get_wildcard_node());
                        }
                    }
                    ClauseMode::Skip => continue,
                }

                // Append residual
                let residuals = residual_iter.next().unwrap();
                out.extend(residuals);

            }

            if let Some(ref mut inner) = expanded_iter {
                assert!(inner.next().is_none());
            }
            assert!(residual_iter.next().is_none());

            out
        };

        let new_variables: Vec<_> = {
            let rest_variables = self.variables.iter()
                .enumerate()
                .filter(|&(var_num, _)| var_num != variable)
                .map(|(_, var)| *var);

            expanded.variables.iter()
                .map(|var| *var)
                .chain(rest_variables)
                .collect()
        };

        let new_clause_leaves: Vec<_> = clause_modes.iter().cloned()
            .zip(self.clause_leaves.iter())
            .filter(|(mode, _)| *mode != ClauseMode::Skip)
            .map(|(_, clause)| *clause)
            .collect();

        let new_leaf_bindings: Vec<_> = clause_modes.iter()
            .zip(self.leaf_bindings.iter())
            .filter(|(mode, _)| **mode != ClauseMode::Skip)
            .map(|(_, binds)| binds.clone())
            .collect();


        let matrix = Self::with_bindings(&merged, new_clause_leaves, new_variables, new_leaf_bindings);

        (expanded.variables, matrix)
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
        new.leaf_bindings.remove(0);
        new
    }

    pub(crate) fn binds_for<'a>(&'a self, leaf: LeafId) -> Option<&'a HashMap<P::PatternNodeKey, P::CfgVariable>> {
        self.clause_leaves
            .iter().enumerate()
            .find(|(_, l)| **l == leaf)
            .map(|(idx, _)| &self.leaf_bindings[idx])
    }

    pub(crate) fn binds_for_head<'a>(&'a self) -> &'a HashMap<P::PatternNodeKey, P::CfgVariable> {
        &self.leaf_bindings[0]
    }

    //pub(crate) fn iterate_clauses<'a>(&'a self) -> impl Iterator<Item = (LeafId, &'a [MatchMatrixElement<P>])> + 'a {
    //    let iter = self.clause_leaves.iter().map(|l| *l)
    //        .zip(chunks_len(&self.data, self.variables.len(), self.clause_leaves.len()));

    //    Box::new(iter)
    //}

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

    pub(crate) fn has_wildcard_head(&self, pattern: &P)
                             -> Option<LeafId>
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

    #[cfg(feature = "debug_table_print")]
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
