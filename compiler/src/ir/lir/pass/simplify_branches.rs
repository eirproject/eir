//use ::parser::AtomicLiteral;
//use ::ir::SSAVariable;
//use ::ir::lir::{ BasicBlock, OpKind, Source, FunctionCfg };
//use ::std::collections::HashMap;
use ::eir::{ AtomicTerm, ConstantTerm };
use ::eir::Atom;
use ::eir::op::OpKind;
use ::eir::intern::{ FALSE, NIL };
use ::eir::FunctionBuilder;
use ::eir::{ CfgNode, CfgEdge, Direction };
use ::eir::{ ValueType };

use ::matches::matches;

use ::std::collections::{ HashMap, HashSet };

pub fn simplify_branches(b: &mut FunctionBuilder) {
    remove_constant_branches(b);
    merge_jump_chains(b);
}

pub fn remove_constant_branches(b: &mut FunctionBuilder) {
    // Remove if_truthy ops which always go one way

    let nil = Atom::from_str("nil");
    let fals = Atom::from_str("false");

    let mut to_remove = Vec::new();
    for ebb in b.function().iter_ebb() {
        for op in b.function().iter_op(ebb) {
            if let OpKind::IfTruthy = b.function().op_kind(op) {
                let read = b.function().op_reads(op)[0];
                let val = b.function().value(read);
                if let ValueType::Constant(constant) = val {
                    let eq_nil = matches!(constant, ConstantTerm::Atomic(
                        AtomicTerm::Atom(a)) if *a == nil);
                    let eq_fals = matches!(constant, ConstantTerm::Atomic(
                        AtomicTerm::Atom(a)) if *a == fals);

                    if eq_nil || eq_fals {
                        to_remove.push((op, false));
                    } else {
                        to_remove.push((op, true));
                    }
                }
            }
        }

    }

    for (op, res) in to_remove {
        b.position_after(op);
        let cont_ebb = b.ebb_split();
        let false_branch = b.function().op_branches(op)[0];
        b.remove_op(op);
        if res {
            let call = b.create_ebb_call(cont_ebb, &[]);
            b.op_jump(call);
        } else {
            b.op_jump(false_branch);
        }
    }

}

//pub fn remove_constant_branches(cfg: &mut FunctionCfg) {
//    let mut static_branches = Vec::new();
//    for node in cfg.graph.nodes_mut() {
//        let mut inner = node.inner.borrow_mut();
//        let last_op = inner.ops.last_mut().unwrap();
//        if let OpKind::IfTruthy = last_op.kind {
//            assert!(last_op.reads.len() == 1);
//            if let Source::Constant(cons) = &last_op.reads[0] {
//                let is_false = cons == &ConstantTerm::Atomic(
//                    AtomicTerm::Atom(FALSE.clone()));
//                let is_nil = cons == &ConstantTerm::Atomic(
//                    AtomicTerm::Atom(NIL.clone()));
//                if is_false || is_nil {
//                    static_branches.push((node.label, 1));
//                } else {
//                    static_branches.push((node.label, 0));
//                }
//            }
//        }
//    }
//
//    for (node_label, taken_branch) in static_branches.iter() {
//        let mut to_remove_edges = Vec::new();
//        {
//            let node = cfg.graph.node_mut(*node_label);
//            for (edge, _node) in node.outgoing.iter() {
//                to_remove_edges.push(*edge);
//            }
//            let mut node_inner = node.inner.borrow_mut();
//            let mut last_op = node_inner.ops.last_mut().unwrap();
//            last_op.kind = OpKind::Jump;
//            last_op.reads = vec![];
//            last_op.writes = vec![];
//        }
//        to_remove_edges.remove(*taken_branch);
//
//        for to_remove_edge in to_remove_edges.iter() {
//            cfg.remove_edge(*to_remove_edge);
//        }
//    }
//
//}

//struct NodeUpdates {
//    // The node the updates should apply to.
//    node: LabelN,
//    // The input edge we followed for getting to
//    // these updates. Used for PHI node updates
//    old_in_edge: EdgeN,
//    // The new edges that should be rechilded to target
//    // this node.
//    new_in_edges: Vec<(EdgeN, LabelN)>,
//}

fn merge_jump_chains(b: &mut FunctionBuilder) {
    let cfg = b.function().gen_cfg();

    // Find candidate jumps
    // Candidate jumps:
    // - The first ebb ends with a Jump op
    // - The second ebb only has a single incoming edge
    let mut args_buf = Vec::new();
    for node_idx in cfg.graph.node_indices() {
        // If the node is an operation and the operation is a Jump
        if let CfgNode::Op(op) = &cfg.graph[node_idx] {
            if let OpKind::Jump = b.function().op_kind(*op) {
                let origin_ebb = b.function().op_ebb(*op);

                // Find jump target
                let jump_call = b.function().op_branches(*op)[0];
                let target_ebb = b.function().ebb_call_target(jump_call);
                let target_ebb_idx = cfg.ebbs[&target_ebb];

                // Find incoming edges to target, only go ahead if
                // there is only a single edge.
                let mut edges_iter = cfg.graph.edges_directed(
                    target_ebb_idx, Direction::Incoming);
                let first_edge = edges_iter.next();
                let second_edge = edges_iter.next();
                if first_edge.is_some() && second_edge.is_none() {
                    // This jump is qualified for a merge.
                    args_buf.clear();
                    args_buf.extend(b.function().ebb_call_args(jump_call)
                                    .iter().cloned());

                    b.function_mut().op_remove(*op);
                    b.position_at_end(origin_ebb);
                    b.ebb_concat(target_ebb, &args_buf);
                }
            }
        }
    }

}


