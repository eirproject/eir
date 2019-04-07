use std::collections::HashMap;

use crate::{ Function, FunctionCfg };
use crate::{ Op, Ebb, Value, EbbCall };
use crate::{ CfgNode, CfgEdge, Direction };

use util::pooled_entity_set::{ EntitySetPool, PooledEntitySet, PooledSetValue };

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
    /// Values that need to exist at the beginning of an Ebb.
    pub ebb_live: HashMap<Ebb, PooledEntitySet<Value>>,
    /// Values that need to exist at every control flow point in an Ebb.
    pub flow_live: HashMap<Op, PooledEntitySet<Value>>,
    /// The pool where `ebb_live` and `flow_live` is allocated.
    pub pool: EntitySetPool,
}

fn dataflow_pass(
    fun: &Function,
    cfg: &FunctionCfg,
    pool: &mut EntitySetPool,
    flow_live: &mut HashMap<Op, PooledEntitySet<Value>>,
    ebb_live: &mut HashMap<Ebb, PooledEntitySet<Value>>,
) -> bool {

    let entry = fun.ebb_entry();

    let mut visitor = DfsPostOrder::new(&cfg.graph, cfg.ebbs[&entry]);

    let mut missing = false;

    // For each Op node in the cfg
    loop {
        let next_opt = visitor.next(&cfg.graph);
        if next_opt.is_none() { break; }
        let curr_idx = next_opt.unwrap();

        if let CfgNode::Op(start_op) = cfg.graph[curr_idx] {

            // Track values in the current segment
            let mut map: PooledEntitySet<Value> = PooledEntitySet::new();
            let mut first = true;
            let mut reached_start = true;

            // Go backward until we hit the start of another segment
            for op in fun.iter_op_rev_from(start_op) {
                let kind = fun.op_kind(op);
                //println!("{:?}", kind);
                let branches = fun.op_branches(op);
                let reads = fun.op_reads(op);
                let writes = fun.op_writes(op);

                let is_segment_fin = cfg.ops.contains_key(&op) && !first;

                // If the current op branches/terminates,
                // store live variables
                if is_segment_fin {
                    reached_start = false;
                    flow_live.insert(op, map.clone());
                    break;
                }

                // If the block is not a terminator, add the values that
                // come from Ebb flow.
                if !kind.is_block_terminator() && first {
                    let vals = &flow_live[&op];
                    map.union(vals, pool);
                }

                // Add the values that come from outgoing branches
                for branch in branches {
                    let call_in_args = fun.ebb_call_args(*branch);
                    let target_ebb = fun.ebb_call_target(*branch);
                    let call_out_args = fun.ebb_args(target_ebb);

                    assert!(call_out_args.len() == call_out_args.len());

                    // If the branch is not finished, data has not yet propagated.
                    // Mark as unfinished.
                    if let Some(vals) = ebb_live.get(&target_ebb) {
                        map.union(vals, pool);

                        // Rename
                        for (in_arg, out_arg) in call_in_args.iter()
                            .zip(call_out_args.iter())
                        {
                            // Only actually perform rename if the value is needed
                            if map.remove(*out_arg, pool) {
                                if !fun.value_is_constant(*in_arg) {
                                    map.insert(*in_arg, pool);
                                }
                            }
                        }
                    } else {
                        missing = true;
                    }

                }

                // Values that are written are not live before they are assigned
                for write in writes {
                    map.remove(*write, pool);
                }

                // Reads are required to be alive
                for read in reads {
                    if !fun.value_is_constant(*read) {
                        map.insert(*read, pool);
                    }
                }

                first = false;
            }

            // If we reached the start of the ebb, store live for ebb
            if reached_start {

                // Find Ebb node
                let in_idx = cfg.graph.first_edge(
                    curr_idx, Direction::Incoming).unwrap();
                assert!(cfg.graph[in_idx] == CfgEdge::Flow);
                let (ebb_idx, _) = cfg.graph.edge_endpoints(in_idx).unwrap();

                let ebb = if let CfgNode::Ebb(e) = cfg.graph[ebb_idx] { e }
                else { panic!() };

                ebb_live.insert(ebb, map);

            }

        }
    }

    missing
}

pub fn calculate_live_values(fun: &Function) -> LiveValues {
    let mut pool: EntitySetPool = EntitySetPool::new();

    let cfg = fun.gen_cfg();

    let mut ebb_live: HashMap<Ebb, PooledEntitySet<Value>> = HashMap::new();
    let mut flow_live: HashMap<Op, PooledEntitySet<Value>> = HashMap::new();

    // Iterate dataflow until all dependencies have been resolved
    loop {
        let res = dataflow_pass(
            fun,
            &cfg,
            &mut pool,
            &mut flow_live,
            &mut ebb_live,
        );
        if !res { break; }
    }

    LiveValues {
        pool: pool,
        ebb_live: ebb_live,
        flow_live: flow_live,
    }
}
