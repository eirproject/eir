use ::eir::{ SSAVariable, Source };
use ::eir::cfg::{ FunctionCfg, LabelN };
use ::eir::op::{ OpKind };
use std::collections::HashSet;

fn does_return_val(cfg: &FunctionCfg, last_label: LabelN, label: LabelN, val: SSAVariable) -> bool {
    let block_container = &cfg.graph[label];
    let block = block_container.inner.borrow();
    assert!(block.ops.len() > 0);
    let last = block.ops.last().unwrap();

    let mut val = val;

    // Follow potential phi node
    if block.phi_nodes.len() > 0 {
        if block.phi_nodes.len() != 1 {
            return false;
        }
        let phi = &block.phi_nodes[0];
        let entry = phi.entries.iter()
            .find(|(l, _s)| *l == last_label).unwrap();
        if let Source::Variable(issa) = entry.1 {
            if issa == val {
                val = phi.ssa;
            } else {
                return false;
            }
        } else {
            return false;
        }
    }

    // Validate ops are not logic
    for op in block.ops.iter() {
        if op.kind.is_logic() {
            return false;
        }
    }

    match last.kind {
        OpKind::Jump => does_return_val(cfg, label,
                                        block_container.outgoing[0].1, val),
        OpKind::ReturnOk => last.reads[0] == Source::Variable(val),
        OpKind::ReturnThrow => last.reads[0] == Source::Variable(val),
        _ => false,
    }
}

/// Promotes function calls at the end of control flow to tail calls.
///
/// Requires `propagate_atomics` to be run before.
pub fn promote_tail_calls(cfg: &mut FunctionCfg) {
    let mut calls = HashSet::new();

    // 1. Identify all basic blocks which end in function calls
    for block_container in cfg.graph.nodes() {
        let block = block_container.inner.borrow();
        let last_op = block.ops.last().unwrap();
        match last_op.kind {
            OpKind::Apply { .. } => {
                calls.insert(block_container.label);
            }
            OpKind::Call { .. } => {
                calls.insert(block_container.label);
            }
            _ => (),
        }
    }

    let mut tail_calls = HashSet::new();

    // 2. Identify tail calls by following function jumps until we
    // either find control flow logic or hit ReturnOk and ReturnThrow.
    for call_label in calls.iter() {
        let call_block_container = &cfg.graph[*call_label];
        let call_block = call_block_container.inner.borrow();
        let last_op = call_block.ops.last().unwrap();

        let ok_jump_label = call_block_container.outgoing[0].1;
        let returns_ok = does_return_val(
            cfg, *call_label, ok_jump_label, last_op.writes[0]);

        let error_jump_label = call_block_container.outgoing[1].1;
        let returns_error = does_return_val(
            cfg, *call_label, error_jump_label, last_op.writes[1]);
        //println!("{} {}", returns_ok, returns_error);

        if returns_ok && returns_error {
            tail_calls.insert(*call_label);
        }
    }

    // 3. Rewrite CFG so that the tail calls are actual terminators
    for call_label in tail_calls.iter() {
        let (ok_edge, err_edge) = {
            let call_block_container = &cfg.graph[*call_label];
            let mut call_block = call_block_container.inner.borrow_mut();
            let last_op = call_block.ops.last_mut().unwrap();
            last_op.writes = vec![];
            match last_op.kind {
                OpKind::Call { ref mut tail_call } => *tail_call = true,
                OpKind::Apply { ref mut tail_call } => *tail_call = true,
                _ => unreachable!(),
            }
            (call_block_container.outgoing[0].0, call_block_container.outgoing[1].0)
        };
        cfg.remove_edge(ok_edge);
        cfg.remove_edge(err_edge);
    }

    //println!("Identified tail calls: {:?}", tail_calls);

}
