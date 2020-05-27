use std::collections::HashSet;

use fnv::FnvBuildHasher;
use hashbrown::HashMap;
type FnvHashMap<K, V> = HashMap<K, V, FnvBuildHasher>;

use petgraph::algo::dominators::Dominators;

use cranelift_bforest::{Set, SetForest};

use crate::{Block, Value};
use crate::{CallKind, Function, MatchKind, OpKind};

#[derive(Debug)]
pub enum ValidationError {
    /// There was an empty block in the function, this is illegal
    EmptyBlock {
        block: Block,
    },

    /// Tried to call a block with wrong arity
    BlockCallArity {
        caller: Block,
        callee: Block,
        attempted: usize,
        actual: usize,
    },

    ValueCallArity {
        caller: Block,
        callee: Value,
        attempted: usize,
        actual: usize,
    },

    /// The arity of the block marked as entry did not match with the identifier
    EntryArityMismatch,

    /// Tried to read a SSA variable that was not visible.
    InvalidRead {
        value: Value,
        block: Block,
    },

    UnfinishedBlock {
        block: Block,
    },
}

fn get_value_list<'a>(fun: &'a Function, value: Value) -> Option<&'a [Value]> {
    if let Some(prim) = fun.value_primop(value) {
        match fun.primop_kind(prim) {
            crate::PrimOpKind::ValueList => return Some(fun.primop_reads(prim)),
            _ => (),
        }
    }
    None
}

impl Function {
    pub fn validate(&self, errors: &mut Vec<ValidationError>) {
        let block_graph = self.block_graph();
        let doms = petgraph::algo::dominators::simple_fast(&block_graph, self.block_entry());

        // Validate internal graph invariants
        self.graph_validate_global();

        self.validate_entry_invariants(errors);
        self.validate_blocks(errors);
        self.validate_ssa_visibility(&doms, errors);
    }

    fn validate_call_to(
        &self,
        errors: &mut Vec<ValidationError>,
        caller: Block,
        val: Value,
        arity: usize,
    ) {
        if let Some(block) = self.value_block(val) {
            let actual = self.block_args(block).len();
            if actual != arity {
                errors.push(ValidationError::BlockCallArity {
                    caller,
                    callee: block,
                    attempted: arity,
                    actual,
                });
            }
        }
    }

    fn validate_blocks(&self, errors: &mut Vec<ValidationError>) {
        let block_graph = self.block_graph();

        let entry = self.block_entry();
        let ret_val = self.block_args(entry)[0];
        let thr_val = self.block_args(entry)[1];

        let mut dfs = block_graph.dfs();
        while let Some(block) = dfs.next(&block_graph) {
            if let Some(kind) = self.block_kind(block) {
                let reads = self.block_reads(block);

                match kind {
                    OpKind::Call(CallKind::ControlFlow) => {
                        self.validate_call_to(errors, block, reads[0], reads.len() - 1);
                    }
                    OpKind::Call(CallKind::Function) => {
                        self.validate_call_to(errors, block, reads[0], reads.len() - 1);
                        self.validate_call_to(errors, block, reads[1], 1);
                        self.validate_call_to(errors, block, reads[2], 3);
                        if reads[0] == ret_val {
                            if reads.len() != 2 {
                                errors.push(ValidationError::ValueCallArity {
                                    caller: block,
                                    callee: ret_val,
                                    attempted: reads.len() - 1,
                                    actual: 1,
                                });
                            }
                        }
                        if reads[0] == thr_val {
                            if reads.len() != 4 {
                                errors.push(ValidationError::ValueCallArity {
                                    caller: block,
                                    callee: thr_val,
                                    attempted: reads.len() - 1,
                                    actual: 3,
                                });
                            }
                        }
                    }
                    OpKind::IfBool => {
                        self.validate_call_to(errors, block, reads[0], 0);
                        self.validate_call_to(errors, block, reads[1], 0);
                        if reads.len() == 4 {
                            self.validate_call_to(errors, block, reads[2], 0);
                        } else {
                            assert!(reads.len() == 3);
                        }
                    }
                    OpKind::UnpackValueList(n) => {
                        self.validate_call_to(errors, block, reads[0], *n);
                    }
                    OpKind::Match { branches } => {
                        let targets_opt = get_value_list(self, reads[0]);
                        let other_targets = &[reads[0]];
                        let targets = targets_opt.unwrap_or(other_targets);

                        assert!(targets.len() == branches.len());
                        for (branch, target) in branches.iter().zip(targets.iter()) {
                            match branch {
                                MatchKind::ListCell => {
                                    self.validate_call_to(errors, block, *target, 2);
                                }
                                MatchKind::MapItem => {
                                    self.validate_call_to(errors, block, *target, 1);
                                }
                                MatchKind::Tuple(n) => {
                                    self.validate_call_to(errors, block, *target, *n);
                                }
                                MatchKind::Type(_) => {
                                    self.validate_call_to(errors, block, *target, 0);
                                }
                                MatchKind::Value => {
                                    self.validate_call_to(errors, block, *target, 0);
                                }
                                MatchKind::Wildcard => {
                                    self.validate_call_to(errors, block, *target, 0);
                                }
                                _ => (),
                            }
                        }
                    }
                    _ => (), // TODO validate more types
                }
            } else {
                errors.push(ValidationError::EmptyBlock { block });
            }
        }
    }

    fn validate_entry_invariants(&self, errors: &mut Vec<ValidationError>) {
        let entry = self.block_entry();
        let arity = self.block_args(entry).len();

        // In the calling convention, the first two arguments are (ok_cont, err_cont)
        if arity != self.ident().arity + 2 {
            errors.push(ValidationError::EntryArityMismatch);
        }
    }
}

impl Function {
    /// Strictly validate SSA visibility
    ///
    /// It operates on the following principle:
    /// * The set of SSA variables visible at the beginning of a block is
    ///   (the SSA variables visible at its immediate dominator)
    ///   + (the set of SSA variables declared in the arguments).
    ///
    /// More specifically:
    /// * Seed the live variable map with the entry node.
    /// * While there are basic blocks missing from our live variable map:
    ///   * Loop through the set of missing blocks:
    ///      * Calculate the live variables in the block if its
    ///        immediate dominator has already been calculated, otherwise
    ///        skip.
    /// * Go through each basic block in the cfg and validate that it only
    ///   references live variables.
    fn validate_ssa_visibility(&self, doms: &Dominators<Block>, errors: &mut Vec<ValidationError>) {
        let entry_block = self.block_entry();

        let mut pool = SetForest::new();

        // Live variables on block entry and exit
        let mut live_variables: FnvHashMap<Block, Set<Value>> =
            FnvHashMap::with_hasher(Default::default());

        // Seed entry node
        let entry_vals = Set::new();
        self.insert_live_for_node(entry_block, entry_vals, &mut pool, &mut live_variables);

        // Iterate until all calculated
        let mut missing_nodes: HashSet<_> = self.block_graph().dfs_iter().collect();
        missing_nodes.remove(&entry_block);
        let mut processed = Vec::new();

        while !missing_nodes.is_empty() {
            for node in missing_nodes.iter() {
                if let Some(idom) = doms.immediate_dominator(*node) {
                    // If the immediate dominator is already processed, process this block.
                    if let Some(idom_live) = live_variables.get(&idom) {
                        let live = idom_live.make_copy(&mut pool);
                        self.insert_live_for_node(*node, live, &mut pool, &mut live_variables);
                        processed.push(*node);
                    }
                } else {
                    // Since we only go through nodes that are alive, that is, reachable
                    // from the entry point, all nodes (except the entry itself), should
                    // have an idom
                    unreachable!()
                }
            }

            // Remove processed
            if !missing_nodes.is_empty() {
                assert!(!processed.is_empty());
            }
            for proc in processed.iter() {
                missing_nodes.remove(proc);
            }
            processed.clear();
        }

        // Go through all blocks and validate visibility
        for block in self.block_graph().dfs_iter() {
            let visible = &live_variables[&block];
            for read in self.block_reads(block) {
                self.value_walk_nested_values::<_, ()>(*read, &mut |val| {
                    if self.value_argument(val).is_some() && !visible.contains(val, &pool, &()) {
                        errors.push(ValidationError::InvalidRead { value: val, block });
                    }
                    Ok(())
                })
                .unwrap();
            }
        }
    }

    fn insert_live_for_node(
        &self,
        block: Block,
        mut base_set: Set<Value>,
        pool: &mut SetForest<Value>,
        live: &mut FnvHashMap<Block, Set<Value>>,
    ) {
        for arg in self.block_args(block) {
            base_set.insert(*arg, pool, &());
        }
        live.insert(block, base_set);
    }
}
