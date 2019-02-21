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

pub fn simplify_branches(cfg: &mut FunctionCfg) {
    //remove_constant_branches(cfg);
    //merge_single_jumps(cfg);
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
            cfg.graph.remove_edge(*to_remove_edge);
        }
    }

}

//pub fn merge_single_jumps(cfg: &mut FunctionCfg) {
//    let mut targets = Vec::new();
//    {
//        for start_node in cfg.graph.nodes() {
//            let start_node_inner = start_node.inner.borrow();
//            let last_op = start_node_inner.ops.last().unwrap();
//            if let OpKind::Jump = last_op.kind {
//                assert!(start_node.outgoing.len() == 1);
//                let (end_node_edge, end_node_label) = start_node.outgoing[0];
//                let end_node = cfg.graph.node(end_node_label);
//                if end_node.incoming.len() == 1 {
//                    targets.push((start_node.label, end_node_edge, end_node_label));
//                }
//            }
//        }
//    }
//
//    for (s_label, edge_label, e_label) in targets.iter() {
//        // Find moves we should insert due to removal of
//        // potential phi nodes 
//        let phi_moves: Vec<(SSAVariable, Source)> = {
//            let target = cfg.graph.node(*e_label);
//            let target_inner = target.inner.borrow();
//            target_inner.phi_nodes.iter()
//                .map(|phi| {
//                    assert!(phi.entries.len() == 1);
//                    (phi.ssa, phi.entries[0].1.clone())
//                }).collect()
//        };
//        cfg.remove_edge(*edge_label);
//        {
//            let target = cfg.graph.node(*e_label);
//            let mut target_inner = 
//        }
//    }
//}


