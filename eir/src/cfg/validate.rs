use ::std::collections::{ HashSet, HashMap };
use crate::Source;
use crate::op::OpKind;
use crate::cfg::{ FunctionCfg, LabelN, SSAVariable };
use crate::FunctionIdent;

pub enum ValidationEntry {
    /// The entry basic block has phi nodes. This is not allowed.
    PhiInEntry,
    /// Arity mismatch between ident and arguments OP.
    ArgumentsArityMismatch {
        ident_arity: usize,
        arguments_reads: usize,
    },
    /// A lambda function must unpack its env as the second OP in
    /// its entry basic block.
    LambdaNoUnpackEnv,
    /// Function must always have a arguments OP as its first entry
    /// in the entry basic block.
    FunctionNoArguments,
    /// SSA variable is assigned twice
    SSADoubleAssign {
        ssa: SSAVariable,
    },
    /// Basic block is empty. Not allowed.
    BasicBlockEmpty {
        label: LabelN,
    },
    CantBeTerminator,
    MustBeTerminator,
    InvalidNumJumps,
    OrphanNodeWarning,
    /// Tried to read a SSA variable that was not visible.
    InvalidRead,
}

impl FunctionCfg {
    pub fn validate(ident: &FunctionIdent, cfg: &FunctionCfg) {
        validate_proper_ssa(ident, cfg);
    }
}

fn validate_proper_ssa(ident: &FunctionIdent, cfg: &FunctionCfg) {

    // Sanity check for our graph implementation.
    // This has shown to be slightly more effective than placebo
    // on spooky errors.
    {
        let mut a = HashSet::new();
        let mut b = HashSet::new();
        let mut c = HashSet::new();
        let mut d = HashSet::new();
        for node_c in cfg.graph.nodes() {
            let node = node_c.inner.borrow();
            a.insert(node_c.label);
            for incoming in node_c.incoming.iter() {
                b.insert(incoming.1);
            }
            for outgoing in node_c.outgoing.iter() {
                c.insert(outgoing.1);
            }
            for phi in node.phi_nodes.iter() {
                for entry in phi.entries.iter() {
                    d.insert(entry.0);
                }
            }
        }
        assert!(b.difference(&a).count() == 0);
        assert!(c.difference(&a).count() == 0);
        assert!(d.difference(&a).count() == 0);
    }

    validate_entry_invariants(ident, cfg);
    validate_double_assign(cfg);
    validate_block_terminators(cfg);
    validate_ssa_visibility(cfg);

}

fn validate_entry_invariants(ident: &FunctionIdent, cfg: &FunctionCfg) {
    let entry_container = cfg.graph.node(cfg.entry());
    let entry = entry_container.inner.borrow();

    // Check entry invariants
    if entry.phi_nodes.len() != 0 {
        println!("Entry block can't have PHI nodes");
    }

    if let OpKind::Arguments = entry.ops[0].kind {
        if entry.ops[0].writes.len() != ident.arity as usize {
            println!("Arity of ident and OpKind::Arguments does not match");
        }
        if ident.lambda.is_some() {
            if let OpKind::UnpackEnv = entry.ops[1].kind {
            } else {
                println!("Entry block of lambda function must have OpKind::UnpackEnv as its second argument");;
            }
        }

    } else {
        println!("Entry block must start with OpKind::Arguments");
    }
}

fn validate_double_assign(cfg: &FunctionCfg) {

    // Collect all ssa variables and check for double assigns
    let mut assigns = HashSet::new();

    for block_container in cfg.graph.nodes() {
        let block = block_container.inner.borrow();
        for phi in block.phi_nodes.iter() {
            if assigns.contains(&phi.ssa) {
                println!("Double assign of {:?}", phi.ssa);
            }
            assigns.insert(phi.ssa);
        }

        for op in block.ops.iter() {
            for write in op.writes.iter() {
                if assigns.contains(write) {
                    println!("Double assign of {:?}", write);
                }
                assigns.insert(*write);
            }
        }
    }

    // This catches a subset of the errors validate_ssa_visibility
    // does. Skip.
    //for block_container in cfg.graph.nodes() {
    //    let block = block_container.inner.borrow_mut();
    //    for phi in block.phi_nodes.iter() {
    //        for &(_label, ref src) in phi.entries.iter() {
    //            if let Source::Variable(ref ssa) = src {
    //                if !assigns.contains(&ssa) {
    //                    println!("Use of unassigned {:?}", ssa);
    //                }
    //            }
    //        }
    //    }

    //    for op in block.ops.iter() {
    //        for read in op.reads.iter() {
    //            if let Source::Variable(ref ssa) = *read {
    //                if !assigns.contains(&ssa) {
    //                    println!("Use of unassigned {:?}", ssa);
    //                }
    //            }
    //        }
    //    }
    //}

}

/// Check that all blocks strictly end in terminators
fn validate_block_terminators(cfg: &FunctionCfg) {
    for block_container in cfg.graph.nodes() {
        let block = block_container.inner.borrow_mut();
        if block.ops.len() == 0 {
            println!("{}: Empty block", block_container.label);
        } else {
            for op in block.ops[0..(block.ops.len() - 1)].iter() {
                if let Some(_) = op.kind.num_jumps() {
                    println!("{}: OP in block body can't be terminator",
                             block_container.label);
                    println!("    {:?}", op.kind);
                }
            }
            let last = block.ops.last().unwrap();
            let last_nj = last.kind.num_jumps();
            if let Some(jumps_num) =  last_nj {
                let actual_jumps_num = block_container.outgoing.len();
                if jumps_num != actual_jumps_num {
                    println!("{}: OP must have {} jumps, has {}",
                             block_container.label, jumps_num, actual_jumps_num);
                    println!("    {:?}", last.kind);
                }
            } else {
                println!("{}: OP at end of block must be terminator",
                         block_container.label);
                println!("    {:?}", last.kind);
            }
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
fn validate_ssa_visibility(cfg: &FunctionCfg) {

    let entry_label = cfg.entry();
    let idom_map = cfg.graph.dominators(entry_label);

    // Live variables on block entry and exit
    let mut live_variables_entry: HashMap<LabelN, HashSet<SSAVariable>>
        = HashMap::new();
    let mut live_variables_exit: HashMap<LabelN, HashSet<SSAVariable>>
        = HashMap::new();

    // Seed entry node
    live_variables_entry.insert(entry_label, HashSet::new());
    let mut entry_live_exit = HashSet::new();
    insert_live_for_node(cfg, entry_label, &mut entry_live_exit);
    live_variables_exit.insert(entry_label, entry_live_exit);

    // Iterate until all calculated
    let mut missing_nodes: HashSet<_>
        = idom_map.keys().cloned().collect();
    missing_nodes.remove(&entry_label);
    let mut processed = Vec::new();

    while missing_nodes.len() > 0 {

        for node in missing_nodes.iter() {
            let node_dom = idom_map[&node];
            if live_variables_exit.contains_key(&node_dom) {
                let mut live = live_variables_exit[&node_dom].clone();
                live_variables_entry.insert(*node, live.clone());
                insert_live_for_node(cfg, *node, &mut live);
                live_variables_exit.insert(*node, live);
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

    // Go through all blocks and validate visibility
    for node_container in cfg.graph.nodes() {
        let node = node_container.inner.borrow();
        if !idom_map.contains_key(&node_container.label) {
            println!("WARNING: Orphan node {}", node_container.label);
            continue;
        }

        let mut visible = live_variables_entry[&node_container.label].clone();

        for phi in node.phi_nodes.iter() {
            for (label, source) in phi.entries.iter() {
                if let Source::Variable(ssa) = source {
                    // Ignore orphaned parts of the CFG.
                    if let Some(prev_visible) = live_variables_exit.get(label) {
                        if !prev_visible.contains(ssa) {
                            println!("{}: {:?} is referenced in PHI node, but is not visible in {}", node_container.label, ssa, label);
                        }
                    }
                }
            }
            visible.insert(phi.ssa);
        }

        for op in node.ops.iter() {
            for source in op.reads.iter() {
                if let Source::Variable(ssa) = source {
                    if !visible.contains(ssa) {
                        println!("{}: {:?} is referenced in read, but is not visible", node_container.label, ssa);
                    }
                }
            }
            for write in op.writes.iter() {
                visible.insert(*write);
            }
        }

    }

}

fn insert_live_for_node(cfg: &FunctionCfg, node: LabelN,
                        live: &mut HashSet<SSAVariable>) {
    let node_container = &cfg.graph[node];
    let node = node_container.inner.borrow();
    for phi in node.phi_nodes.iter() {
        live.insert(phi.ssa);
    }
    for op in node.ops.iter() {
        for write in op.writes.iter() {
            live.insert(*write);
        }
    }
}






