//use ::eir::{ SSAVariable, Source };
//use ::eir::cfg::{ FunctionCfg, EdgeN, LabelN };
//use ::eir::op::{ OpKind };
use std::collections::{ HashMap, HashSet };

use ::eir::{ Function, FunctionBuilder };
use ::eir::{ Op, Value, EbbCall };
use ::eir::op::OpKind;

#[derive(PartialEq, Eq)]
enum OutBranch {
    Ok,
    Throw,
}

pub fn promote_tail_calls(b: &mut FunctionBuilder) {

    let mut call_ops = HashSet::new();

    {
        // Find all calls
        let fun = b.function();
        for ebb in fun.iter_ebb() {
            for op in fun.iter_op(ebb) {
                match fun.op_kind(op) {
                    OpKind::Call { .. } => {
                        call_ops.insert(op);
                    },
                    OpKind::Apply { .. } => {
                        call_ops.insert(op);
                    },
                    _ => (),
                }
            }
        }
    }

    // Change to tail calls if both ok and throw branches end
    // up returning the value unconditionally and without
    // side effects.
    for call in call_ops.iter() {
        let does_return = {
            let fun = b.function();
            does_return_val(fun, *call, OutBranch::Ok)
                && does_return_val(fun, *call, OutBranch::Throw)
        };
        if does_return {
            b.position_after(*call);
            b.ebb_split();

            b.change_op_make_tail_call();
        }
    }

}

fn resolve_renames(aliases: &mut HashSet<Value>, b: &Function, call: EbbCall) {
    let in_args = b.ebb_call_args(call);
    let target = b.ebb_call_target(call);
    let out_args = b.ebb_args(target);

    assert!(in_args.len() == out_args.len());

    for (ia, oa) in in_args.iter().zip(out_args.iter()) {
        if aliases.contains(ia) {
            aliases.insert(*oa);
        }
    }
}

fn does_return_val(b: &Function, call: Op, out: OutBranch) -> bool {
    let mut aliases = HashSet::new();
    let mut next;
    match out {
        OutBranch::Ok => {
            aliases.insert(b.op_writes(call)[0]);
            next = b.op_after(call).unwrap();
        }
        OutBranch::Throw => {
            aliases.insert(b.op_writes(call)[1]);
            let branch = b.op_branches(call)[0];
            resolve_renames(&mut aliases, b, branch);

            let target = b.ebb_call_target(branch);
            next = b.ebb_first_op(target);
        }
    }
    loop {
        let reads = b.op_reads(next);
        let writes = b.op_reads(next);
        match b.op_kind(next) {
            OpKind::Move => {
                if aliases.contains(&reads[0]) {
                    aliases.insert(writes[0]);
                }
                next = b.op_after(next).unwrap();
            },
            OpKind::Jump => {
                let ebb_call = b.op_branches(next)[0];
                resolve_renames(&mut aliases, b, ebb_call);
                let target = b.ebb_call_target(ebb_call);
                next = b.ebb_first_op(target);
            },
            OpKind::ReturnOk => {
                return out == OutBranch::Ok;
            },
            OpKind::ReturnThrow => {
                return out == OutBranch::Throw;
            },
            _ => {
                return false;
            },
        }
    }
}

//fn does_return_val(cfg: &FunctionCfg, in_edge: EdgeN, val: SSAVariable) -> bool {
//    let label = cfg.graph.edge_to(in_edge);
//    let block_container = &cfg.graph[label];
//    let block = block_container.inner.borrow();
//    assert!(block.ops.len() > 0);
//    let last = block.ops.last().unwrap();
//
//    let mut val = val;
//
//    // Follow potential phi node
//    if block.phi_nodes.len() > 0 {
//        if block.phi_nodes.len() != 1 {
//            return false;
//        }
//        let phi = &block.phi_nodes[0];
//        let entry = phi.entries.iter()
//            .find(|(l, _s)| *l == in_edge).unwrap();
//        if entry.1 == val {
//            val = phi.ssa;
//        } else {
//            return false;
//        }
//    }
//
//    // Validate ops are not logic
//    for op in block.ops.iter() {
//        if op.kind.is_logic() {
//            return false;
//        }
//    }
//
//    match last.kind {
//        OpKind::Jump => does_return_val(
//            cfg, block_container.outgoing[0].0, val),
//        OpKind::ReturnOk => last.reads[0] == Source::Variable(val),
//        OpKind::ReturnThrow => last.reads[0] == Source::Variable(val),
//        _ => false,
//    }
//}
//
///// Promotes function calls at the end of control flow to tail calls.
/////
///// Requires `propagate_atomics` to be run before.
//pub fn promote_tail_calls(cfg: &mut FunctionCfg) {
//    let mut calls = HashSet::new();
//
//    // 1. Identify all basic blocks which end in function calls
//    for block_container in cfg.graph.nodes() {
//        let block = block_container.inner.borrow();
//        let last_op = block.ops.last().unwrap();
//        match last_op.kind {
//            OpKind::Apply { .. } => {
//                calls.insert(block_container.label);
//            }
//            OpKind::Call { .. } => {
//                calls.insert(block_container.label);
//            }
//            _ => (),
//        }
//    }
//
//    let mut tail_calls = HashSet::new();
//
//    // 2. Identify tail calls by following function jumps until we
//    // either find control flow logic or hit ReturnOk and ReturnThrow.
//    for call_label in calls.iter() {
//        let call_block_container = &cfg.graph[*call_label];
//        let call_block = call_block_container.inner.borrow();
//        let last_op = call_block.ops.last().unwrap();
//
//        let ok_jump_edge = call_block_container.outgoing[0].0;
//        let returns_ok = does_return_val(
//            cfg, ok_jump_edge, last_op.writes[0]);
//
//        let error_jump_edge = call_block_container.outgoing[1].0;
//        let returns_error = does_return_val(
//            cfg, error_jump_edge, last_op.writes[1]);
//        //println!("{} {}", returns_ok, returns_error);
//
//        if returns_ok && returns_error {
//            tail_calls.insert(*call_label);
//        }
//    }
//
//    // 3. Rewrite CFG so that the tail calls are actual terminators
//    for call_label in tail_calls.iter() {
//        let (ok_edge, err_edge) = {
//            let call_block_container = &cfg.graph[*call_label];
//            let mut call_block = call_block_container.inner.borrow_mut();
//            let last_op = call_block.ops.last_mut().unwrap();
//            last_op.writes = vec![];
//            match last_op.kind {
//                OpKind::Call { ref mut tail_call } => *tail_call = true,
//                OpKind::Apply { ref mut tail_call } => *tail_call = true,
//                _ => unreachable!(),
//            }
//            (call_block_container.outgoing[0].0, call_block_container.outgoing[1].0)
//        };
//        cfg.remove_edge(ok_edge);
//        cfg.remove_edge(err_edge);
//    }
//
//    //println!("Identified tail calls: {:?}", tail_calls);
//
//}
