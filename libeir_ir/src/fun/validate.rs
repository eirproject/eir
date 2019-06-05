use crate::op::OpKind;
use super::Function;
use super::{ Ebb, EbbCall, Op, Value };
use super::graph::{ FunctionCfg, CfgNode, CfgEdge };

use util::pooled_entity_set::{ PooledSetValue, PooledEntitySet };
use cranelift_entity::ListPool;

use std::collections::{ HashMap, HashSet };
use petgraph::{ Direction };
use petgraph::graph::{ Graph, NodeIndex };
use petgraph::algo::dominators::Dominators;

use matches::{ matches, assert_matches };

pub enum ValidationEntry {
    /// The entry EBB has a different arity from the function signature.
    FunctionEntryArityMismatch {
        ident_arity: usize,
        entry_arity: usize,
    },
    /// A lambda function must unpack its env as the first OP in
    /// its entry basic block.
    LambdaNoUnpackEnv,
    /// Basic block is empty. Not allowed.
    EbbEnpty {
        ebb: Ebb,
    },
    CantBeTerminator,
    MustBeTerminator,
    InvalidEbbCalls,
    OrphanNodeWarning,
    /// Tried to read a SSA variable that was not visible.
    InvalidRead,
}

impl Function {
    pub fn validate(&self) {
        let cfg = self.gen_cfg();
        let doms = petgraph::algo::dominators::simple_fast(
            &cfg.graph, cfg.ebbs[&self.ebb_entry()]);

        validate_entry_invariants(self, &cfg);
        validate_ssa_visibility(self, &cfg, &doms);
        validate_ebb_calls(self, &cfg);
    }
}

fn validate_ebb_calls(fun: &Function, cfg: &FunctionCfg) {
    for ebb in fun.iter_ebb() {
        for op in fun.iter_op(ebb) {
            let kind = fun.op_kind(op);
            if !kind.allowed_in_dialect(fun.dialect) {
                println!("ERROR: Operation {:?} not allowed in {:?} dialect",
                         kind, fun.dialect);
            }

            for branch in fun.op_branches(op) {
                let target = fun.ebb_call_target(*branch);
                if fun.ebb_call_args(*branch).len() != fun.ebb_args(target).len() {
                    println!("ERROR: EbbCall arity does not match arity of Ebb {} {}", branch, target);
                }
            }
        }
    }
}

fn validate_entry_invariants(fun: &Function, cfg: &FunctionCfg) {
    let entry_ebb = fun.ebb_entry();

    let arity = fun.ident.arity;
    let needed_entry_arity = if fun.ident.lambda.is_some() { arity + 1 } else { arity };

    //if fun.ebb_args(entry_ebb).len() != needed_entry_arity {
    //    println!("ERROR: Entry Ebb and identifier must be of same arity");
    //}

    if fun.ident.lambda.is_some() {
        let entry_first_op = fun.iter_op(entry_ebb).next().unwrap();
        if !matches!(fun.op_kind(entry_first_op), OpKind::UnpackEnv) {
            println!("ERROR: Lambda must have UnpackEnv as its first argument");
        }
    }

}

/// Strictly validate SSA visibility
///
/// It operates on the following principle:
/// * The set of SSA variables visible at the beginning of a block is
///   (the SSA variables visible at the end of its immediate dominator)
///   + (the set of SSA variables declared in PHI nodes of the block).
///
/// More specifically:
/// * Seed the live variable map with the entry node.
/// * While there are basic blocks missing from our live variable map:
///   * Loop through the set of missing basic blocks:
///      * Calculate the live variables in the basic block if its
///        immediate dominator has already been calculated, otherwise
///        skip.
/// * Go through each basic block in the cfg and validate that it only
///   references live variables.
fn validate_ssa_visibility(fun: &Function, cfg: &FunctionCfg,
                           doms: &Dominators<NodeIndex>) {
    let entry_ebb = fun.ebb_entry();

    let mut pool: ListPool<PooledSetValue> = ListPool::new();

    // Live variables on block entry and exit
    let mut live_variables_ops: HashMap<Op, PooledEntitySet<Value>>
        = HashMap::new();
    let mut live_variables_ebbs: HashMap<Ebb, PooledEntitySet<Value>>
        = HashMap::new();

    // Seed entry node
    let mut entry_vals = PooledEntitySet::new();
    for const_val in fun.iter_constants() {
        entry_vals.insert(*const_val, &mut pool);
    }

    insert_live_for_node(fun, entry_ebb, entry_vals,
                         &mut pool,
                         &mut live_variables_ops,
                         &mut live_variables_ebbs);

    // Iterate until all calculated
    let mut missing_nodes: HashSet<_> =
        cfg.ebbs.keys().cloned().collect();
    missing_nodes.remove(&entry_ebb);
    let mut processed = Vec::new();

    while missing_nodes.len() > 0 {

        for node in missing_nodes.iter() {

            // If there are no incoming edges, this Ebb is an orphan.
            // Remove from set.
            let idom = doms.immediate_dominator(cfg.ebbs[node]);
            if idom.is_none() {
                processed.push(*node);
                continue;
            }

            // The immediate dominator of a Ebb will always be an Op.
            // Get the identifier of the immediate dominator.
            let node_idom = idom.unwrap();
            let idom_op = if let CfgNode::Op(op) = cfg.graph[node_idom] {
                op
            } else {
                panic!()
            };

            // If the immediate dominator is already processed, process this Ebb.
            if live_variables_ops.contains_key(&idom_op) {
                let live = live_variables_ops[&idom_op].make_copy(&mut pool);
                insert_live_for_node(fun, *node, live, &mut pool,
                                     &mut live_variables_ops,
                                     &mut live_variables_ebbs);
                processed.push(*node);
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

    let mut aux_values = HashSet::new();

    // Go through all blocks and validate visibility
    for ebb in fun.iter_ebb() {
        if doms.immediate_dominator(cfg.ebbs[&ebb]).is_none() && ebb != entry_ebb {
            println!("WARNING: Orphan {}", ebb);
            continue;
        }

        let mut last_valset = &live_variables_ebbs[&ebb];

        for op in fun.iter_op(ebb) {
            if live_variables_ops.contains_key(&op) {
                aux_values.clear();
                last_valset = &live_variables_ops[&op];
            }
            for read in fun.op_reads(op) {
                if !last_valset.contains(*read, &mut pool)
                    && !aux_values.contains(read) {
                    println!("ERROR: Value not visible at location {}", read);
                }
            }
            for write in fun.op_writes(op) {
                aux_values.insert(*write);
            }
            for branch in fun.op_branches(op) {
                for arg in fun.ebb_call_args(*branch) {
                    if !last_valset.contains(*arg, &mut pool)
                        && !aux_values.contains(arg) {
                        println!("ERROR: Value not visible at location {}", arg);
                    }
                }
            }
        }

    }

}

fn insert_live_for_node(fun: &Function, ebb: Ebb, base: PooledEntitySet<Value>,
                        pool: &mut ListPool<PooledSetValue>,
                        live_ops: &mut HashMap<Op, PooledEntitySet<Value>>,
                        live_ebbs: &mut HashMap<Ebb, PooledEntitySet<Value>>) {
    let mut base = base;

    for arg in fun.ebb_args(ebb) {
        base.insert(*arg, pool);
    }

    live_ebbs.insert(ebb, base.make_copy(pool));

    for op in fun.iter_op(ebb) {
        for write in fun.op_writes(op) {
            base.insert(*write, pool);
        }
        if fun.op_branches(op).len() > 0 {
            live_ops.insert(op, base.make_copy(pool));
        }
    }
}
