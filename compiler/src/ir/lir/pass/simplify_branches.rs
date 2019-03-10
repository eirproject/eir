//use ::parser::AtomicLiteral;
//use ::ir::SSAVariable;
//use ::ir::lir::{ BasicBlock, OpKind, Source, FunctionCfg };
use ::ir::lir::FunctionCfg;
//use ::std::collections::HashMap;
use ::eir::{ Source, SSAVariable };
use ::eir::{ AtomicTerm, ConstantTerm };
use ::eir::Atom;
use ::eir::op::OpKind;
use ::eir::intern::{ FALSE, NIL };
use ::eir::cfg::{ LabelN, EdgeN };

use ::std::collections::{ HashMap, HashSet };

pub fn simplify_branches(cfg: &mut FunctionCfg) {
    remove_constant_branches(cfg);
    merge_single_jumps(cfg);
}

pub fn remove_constant_branches(cfg: &mut FunctionCfg) {
    let mut static_branches = Vec::new();
    for node in cfg.graph.nodes_mut() {
        let mut inner = node.inner.borrow_mut();
        let last_op = inner.ops.last_mut().unwrap();
        if let OpKind::IfTruthy = last_op.kind {
            assert!(last_op.reads.len() == 1);
            if let Source::Constant(cons) = &last_op.reads[0] {
                let is_false = cons == &ConstantTerm::Atomic(
                    AtomicTerm::Atom(FALSE.clone()));
                let is_nil = cons == &ConstantTerm::Atomic(
                    AtomicTerm::Atom(NIL.clone()));
                if is_false || is_nil {
                    static_branches.push((node.label, 1));
                } else {
                    static_branches.push((node.label, 0));
                }
            }
        }
    }

    for (node_label, taken_branch) in static_branches.iter() {
        let mut to_remove_edges = Vec::new();
        {
            let node = cfg.graph.node_mut(*node_label);
            for (edge, _node) in node.outgoing.iter() {
                to_remove_edges.push(*edge);
            }
            let mut node_inner = node.inner.borrow_mut();
            let mut last_op = node_inner.ops.last_mut().unwrap();
            last_op.kind = OpKind::Jump;
            last_op.reads = vec![];
            last_op.writes = vec![];
        }
        to_remove_edges.remove(*taken_branch);

        for to_remove_edge in to_remove_edges.iter() {
            cfg.remove_edge(*to_remove_edge);
        }
    }

}

struct NodeUpdates {
    // The node the updates should apply to.
    node: LabelN,
    // The input edge we followed for getting to
    // these updates. Used for PHI node updates
    old_in_edge: EdgeN,
    // The new edges that should be rechilded to target
    // this node.
    new_in_edges: Vec<(EdgeN, LabelN)>,
}

fn merge_single_jumps(cfg: &mut FunctionCfg) {

    // Find candidate jumps
    // Candidate jumps:
    // * Contain no PHI nodes
    // * Contain only one OP, a Jump
    let mut changes = HashMap::new();
    let mut jump_nodes = HashSet::new();
    {
        for jump_node in cfg.graph.nodes() {
            let jump_node_inner = jump_node.inner.borrow();
            if jump_node_inner.ops.len() == 1 && jump_node_inner.phi_nodes.len() == 0 {
                if let OpKind::Jump = jump_node_inner.ops[0].kind {
                    assert!(jump_node.outgoing.len() == 1);
                    let target = jump_node.outgoing[0];
                    let mut rechild_edges = Vec::new();
                    for n in jump_node.incoming.iter() {
                        rechild_edges.push(*n);
                    }
                    let update = NodeUpdates {
                        node: target.1,
                        old_in_edge: target.0,
                        new_in_edges: rechild_edges,
                    };
                    changes.insert(update.old_in_edge, update);
                    jump_nodes.insert(jump_node.label);
                }
            }
        }
    }

    // Propagate jumps
    // In the cases where we have a chain of jumps, we propagate
    // the updates down the chain iteratively until we see no more
    // changes.
    let to_process: HashSet<EdgeN> = changes.keys().cloned().collect();
    let mut changed = true;
    while changed {
        changed = false;
        for process_edge in to_process.iter() {
            let new = {
                let update = &changes[process_edge];
                let mut new = Vec::new();
                for (in_edge, in_node) in update.new_in_edges.iter() {
                    if !jump_nodes.contains(in_node) || !changes.contains_key(in_edge) {
                        new.push((*in_edge, *in_node));
                    } else {
                        changed = true;
                        for (new_edge, new_node) in changes[in_edge].new_in_edges.iter() {
                            new.push((*new_edge, *new_node));
                        }
                    }
                }
                new
            };
            changes.get_mut(process_edge).unwrap().new_in_edges = new;
        }
    }

    // Update all node edges
    // We don't update jumps which are part of jump chains,
    // these will be purged by the `remove_orphan_blocks` pass.
    for update in changes.values() {
        if jump_nodes.contains(&update.node) {
            continue;
        }

        {
            let target_node = cfg.graph.node(update.node);
            let mut target_inner = target_node.inner.borrow_mut();

            // Retarget all phi nodes
            for phi in target_inner.phi_nodes.iter_mut() {
                let source = {
                    let (_, source) = phi.entries.iter()
                        .find(|e| e.0 == update.old_in_edge).unwrap();
                    source.clone()
                };
                //let (_, source) = phi.entries.remove(old_pos);
                for (new_edge, _new_node) in update.new_in_edges.iter() {
                    phi.entries.push((*new_edge, source.clone()));
                }
            }
        }

        // Retarget new edges
        for (new_edge, _new_node) in update.new_in_edges.iter() {
            cfg.graph.rechild_edge(*new_edge, update.node);
        }

    }

}


