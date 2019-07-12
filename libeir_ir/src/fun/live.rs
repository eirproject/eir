use std::collections::HashMap;

use crate::{ Function };
use crate::{ Block, Value };

use libeir_util::pooled_entity_set::{ EntitySetPool, PooledEntitySet, PooledSetValue };

use petgraph::visit::DfsPostOrder;

use matches::assert_matches;

/// # Value liveness calculation
/// Utility for calculating live values at every point in a functions
/// CFG.
///
/// This requires that orphan nodes have been removed before it is run.
///
/// TODO
/// If orphan nodes still exist when this is run, this will loop infinitely.
/// Add a missing counter to detect when we have reached a equilibrium that
/// is not final.
///
/// TODO
/// Right now this uses a (probably) bad algorithm I made up on the spot.
/// I am sure there are better and more efficient ways of doing this,
/// but this should work for now.
/// For CFGs that are acyclic, this algorithm will complete in a single
/// iteration. For cyclic CFGs, this will take more than one iteration,
/// but with the sort of control flow the compiler produces, it shouldn't
/// take more than two iterations most of the time.
///
/// TODO
/// Right now a new set is allocated for every node at every pass.
/// Since we are using arena based allocation, this should not be too bad,
/// but we probably want to improve this in the future. This will probably
/// work fine up to reasonably large functions.

#[derive(Debug)]
pub struct LiveValues {
    /// Values that need to exist at every block
    pub live: HashMap<Block, PooledEntitySet<Value>>,
    /// The pool where `ebb_live` and `flow_live` is allocated.
    pub pool: EntitySetPool,
}

fn dataflow_pass(
    fun: &Function,
    pool: &mut EntitySetPool,
    live: &mut HashMap<Block, PooledEntitySet<Value>>,
) -> bool {

    let graph = fun.block_graph();
    let mut visitor = graph.dfs_post_order();

    let mut stable = true;

    // For each Op node in the cfg
    'outer: while let Some(block) = visitor.next(&graph) {

        let mut set: PooledEntitySet<Value> = PooledEntitySet::new();

        // For each of the outgoing branches, add its live values to the current set
        for branch in graph.outgoing(block) {
            if let Some(vals) = live.get(&branch) {
                set.union(vals, pool);
            }
        }

        // Add the reads for the block OP to the current set
        for read in fun.block_reads(block) {
            // Only insert if it actually is a variable, not a block or constant
            if fun.value_arg_definition(*read).is_some() {
                set.insert(*read, pool);
            }
        }

        // Remove the block arguments from the current set
        for arg in fun.block_args(block) {
            set.remove(*arg, pool);
        }

        // If we don't have a previous live calculation for this block, or if
        // the live set has changed, we are not yet stable.
        if let Some(mut old_set) = live.remove(&block) {
            if !old_set.eq(&set, pool) {
                stable = false;
            }
            old_set.clear(pool);
        } else {
            stable = false;
        }

        // Insert the new calculated live variables
        live.insert(block, set);

    }

    stable
}

pub fn calculate_live_values(fun: &Function) -> LiveValues {
    let mut pool: EntitySetPool = EntitySetPool::new();

    let mut live: HashMap<Block, PooledEntitySet<Value>> = HashMap::new();

    // Iterate dataflow until all dependencies have been resolved
    loop {
        let res = dataflow_pass(
            fun,
            &mut pool,
            &mut live,
        );
        if res { break; }
    }

    // Validate that the live set at entry is empty
    {
        let entry = fun.block_entry();
        assert!(live[&entry].size(&pool) == 0);
    }

    LiveValues {
        pool: pool,
        live: live,
    }
}
