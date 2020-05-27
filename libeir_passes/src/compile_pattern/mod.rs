use matches::matches;

use bumpalo::{collections::Vec as BVec, Bump};
use hashbrown::HashMap;

use fnv::FnvBuildHasher;
type BFnvHashMap<'bump, K, V> = HashMap<K, V, FnvBuildHasher, &'bump Bump>;

use libeir_ir::FunctionBuilder;
use libeir_ir::OpKind;
use libeir_ir::PatternNode;
use libeir_ir::Value;

use libeir_util_pattern_compiler::to_decision_tree;

mod erlang_pattern_provider;
use self::erlang_pattern_provider::pattern_to_provider;

mod lower_cfg;
use self::lower_cfg::lower_cfg;
use self::lower_cfg::DecisionTreeDestinations;

use super::FunctionPass;

#[cfg(test)]
mod tests;

pub struct CompilePatternPass {
    bump: Option<Bump>,
}

impl CompilePatternPass {
    pub fn new() -> Self {
        CompilePatternPass {
            bump: Some(Bump::new()),
        }
    }
}

impl FunctionPass for CompilePatternPass {
    fn name(&self) -> &str {
        "compile_pattern"
    }
    fn run_function_pass(&mut self, b: &mut FunctionBuilder) {
        self.compile_pattern(b);
    }
}

impl CompilePatternPass {
    pub fn compile_pattern(&mut self, b: &mut FunctionBuilder) {
        let mut bump = self.bump.take().unwrap();

        {
            // Find all pattern matching constructs
            let case_blocks = {
                let fun = b.fun();

                let mut case_blocks = BVec::new_in(&bump);

                let graph = fun.block_graph();
                for block in graph.dfs_iter() {
                    if matches!(fun.block_kind(block), Some(OpKind::Case { .. })) {
                        case_blocks.push(block);
                    }
                }

                case_blocks
            };

            for block in case_blocks.iter().cloned() {
                let no_match;
                let mut guards = BVec::new_in(&bump);
                let mut bodies = BVec::new_in(&bump);
                let match_val;
                let mut values = BVec::new_in(&bump);
                let mut clauses = BVec::new_in(&bump);

                if let Some(OpKind::Case { clauses: c }) = b.fun().block_kind(block) {
                    clauses.extend(c.as_slice(&b.fun().pool.clause).iter().cloned());
                } else {
                    unreachable!()
                };
                let num_clauses = clauses.len();

                // Extract arguments from block reads
                {
                    let reads = b.fun().block_reads(block);
                    let mut r_iter = reads.iter();

                    // First entry is always no_match
                    no_match = *r_iter.next().unwrap();

                    // Guards and bodies
                    for _ in 0..num_clauses {
                        guards.push(*r_iter.next().unwrap());
                        bodies.push(*r_iter.next().unwrap());
                    }

                    // Match value
                    match_val = *r_iter.next().unwrap();

                    // Values
                    while let Some(val) = r_iter.next() {
                        values.push(*val);
                    }
                }

                let destinations = DecisionTreeDestinations {
                    fail: no_match,
                    guards,
                    bodies,
                };

                // Get/validate number of roots
                let mut roots_num = None;
                for clause in clauses.iter() {
                    let num = b.pat().clause_root_nodes(*clause).len();
                    if let Some(num_old) = roots_num {
                        assert!(num == num_old);
                    } else {
                        roots_num = Some(num);
                    }
                }

                let mut value_map = HashMap::new();
                {
                    // Create map of PatternValue => Value
                    let mut value_idx = 0;
                    for clause in clauses.iter() {
                        for value in b.pat().clause_values(*clause) {
                            value_map.insert(*value, ValueBind::Value(values[value_idx]));
                            value_idx += 1;
                        }
                    }
                    assert!(values.len() == value_idx);

                    // Create map of PatternValue => PatternNode
                    value_map.extend(
                        clauses
                            .iter()
                            .flat_map(|clause| b.pat().clause_node_binds_iter(*clause))
                            .map(|(k, v)| (k, ValueBind::Node(v))),
                    );
                }

                let mut provider = pattern_to_provider(b, &clauses, &value_map);
                let decision_tree = to_decision_tree(&mut provider);

                let mut out = Vec::new();
                decision_tree.to_dot(&mut out).unwrap();

                let cfg_entry =
                    lower_cfg(&bump, b, &provider, &decision_tree, &clauses, &destinations);

                b.block_clear(block);
                b.op_call_flow(block, cfg_entry, &[match_val]);
            }
        }

        bump.reset();
        self.bump = Some(bump);
    }
}

#[derive(Debug, Copy, Clone)]
pub(super) enum ValueBind {
    Value(Value),
    Node(PatternNode),
}
