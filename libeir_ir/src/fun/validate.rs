use crate::op::OpKind;
use super::Function;
use super::{ Block, Value };

use libeir_util::pooled_entity_set::{ EntitySetPool, PooledEntitySet };
use cranelift_entity::ListPool;

use std::collections::{ HashMap, HashSet };
use petgraph::{ Direction };
use petgraph::graph::{ Graph, NodeIndex };
use petgraph::algo::dominators::Dominators;

use matches::{ matches, assert_matches };

//pub enum ValidationEntry {
//    /// The entry EBB has a different arity from the function signature.
//    FunctionEntryArityMismatch {
//        ident_arity: usize,
//        entry_arity: usize,
//    },
//    /// A lambda function must unpack its env as the first OP in
//    /// its entry basic block.
//    LambdaNoUnpackEnv,
//    /// Basic block is empty. Not allowed.
//    EbbEnpty {
//        ebb: Ebb,
//    },
//    CantBeTerminator,
//    MustBeTerminator,
//    InvalidEbbCalls,
//    OrphanNodeWarning,
//    /// Tried to read a SSA variable that was not visible.
//    InvalidRead,
//}

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

    /// The arity of the block marked as entry did not match with the identifier
    EntryArityMismatch,

    /// Tried to read a SSA variable that was not visible.
    InvalidRead {
        value: Value,
        block: Block,
    },

}

impl Function {

    pub fn validate(&self) {

        let mut errors = Vec::new();

        let block_graph = self.block_graph();
        let doms = petgraph::algo::dominators::simple_fast(&block_graph, self.block_entry());

        //validate_entry_invariants(self);
        //validate_ssa_visibility(self, &doms);
        self.validate_entry_invariants(&mut errors);
        self.validate_blocks(&mut errors);
        self.validate_ssa_visibility(&doms, &mut errors);

        println!("{:#?}", errors);
    }

    fn validate_call_to(&self, errors: &mut Vec<ValidationError>, caller: Block, val: Value, arity: usize) {
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

        let mut dfs = block_graph.dfs();
        while let Some(block) = dfs.next(&block_graph) {
            if let Some(kind) = self.block_kind(block) {
                match kind {
                    OpKind::Call => {
                        let reads = self.block_reads(block);
                        self.validate_call_to(errors, block, reads[0], reads.len() - 1);
                    }
                    _ => (), // TODO validate more types
                }
            } else {
                errors.push(ValidationError::EmptyBlock { block: block });
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

        let mut pool: EntitySetPool = ListPool::new();

        // Live variables on block entry and exit
        let mut live_variables: HashMap<Block, PooledEntitySet<Value>>
            = HashMap::new();

        // Seed entry node
        let mut entry_vals = PooledEntitySet::new();
        self.insert_live_for_node(entry_block, entry_vals,
                                  &mut pool,
                                  &mut live_variables);

        // Iterate until all calculated
        let mut missing_nodes: HashSet<_> = self.block_graph().dfs_iter().collect();
        missing_nodes.remove(&entry_block);
        let mut processed = Vec::new();

        while missing_nodes.len() > 0 {

            for node in missing_nodes.iter() {

                if let Some(idom) = doms.immediate_dominator(*node){
                    // If the immediate dominator is already processed, process this block.
                    if let Some(idom_live) = live_variables.get(&idom) {
                        let live = idom_live.make_copy(&mut pool);
                        self.insert_live_for_node(*node, live, &mut pool,
                                                  &mut live_variables);
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
            if missing_nodes.len() > 0 {
                assert!(processed.len() > 0);
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
                if let Some(_) = self.value_arg_definition(*read) {
                    if !visible.contains(*read, &pool) {
                        errors.push(ValidationError::InvalidRead {
                            value: *read,
                            block: block,
                        });
                    }
                }
            }
        }

    }

    fn insert_live_for_node(&self, block: Block, base_set: PooledEntitySet<Value>,
                            pool: &mut EntitySetPool,
                            live: &mut HashMap<Block, PooledEntitySet<Value>>) {
        let mut set = base_set.make_copy(pool);
        for arg in self.block_args(block) {
            set.insert(*arg, pool);
        }
        live.insert(block, set);
    }

}
