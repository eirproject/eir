use log::trace;

use std::collections::BTreeMap;

use libeir_ir::{Block, FunctionBuilder, Value};
use libeir_ir::{MangleTo, Mangler};

use libeir_util_datastructures::aux_traits::AuxImpl;

use super::chain_graph::{
    synthesis::{Instance, InstanceKind, SegmentBodyKind, SegmentHeadKind, Synthesis},
    Chain, ChainGraph, Node, NodeKind,
};

pub fn rewrite(
    b: &mut FunctionBuilder,
    map: &mut BTreeMap<Value, Value>,
    target: Block,
    graph: &ChainGraph,
    synthesis: &Synthesis,
) {
    //let segment_set_pool = synthesis.segment_set_pool.as_ref().unwrap();
    //let segments_back = synthesis.segments_back.as_ref().unwrap();

    trace!("REWRITE {}", target);
    //println!("{:#?}", synthesis);

    //assert!(synthesis.segments[synthesis.order[0]].kind.is_out());
    //assert!(
    //    synthesis
    //        .order
    //        .iter()
    //        .map(|s| &synthesis.segments[*s])
    //        .filter(|s| s.kind.is_out())
    //        .count() == 1
    //);

    let mut segment_map = BTreeMap::new();

    let mut global_map: BTreeMap<Instance, Value> = BTreeMap::new();
    let mut local_map: BTreeMap<Node, Value> = BTreeMap::new();

    let mut entry_map: BTreeMap<(Chain, usize), Value> = BTreeMap::new();

    let mut arg_buf = Vec::new();

    let mut walker = crate::util::Walker::new();

    let mut mangler = Mangler::new();

    for (chain, to_value) in synthesis.substitutions.iter() {
        let chain_entry = graph.get_chain_entry_block(*chain);
        let from_value = b.value(chain_entry);
        map.insert(from_value, *to_value);
    }

    for segment_id in synthesis.order.iter() {
        let segment = &synthesis.segments[*segment_id];

        let block = b.block_insert();
        segment_map.insert(*segment_id, block);

        match segment.head {
            SegmentHeadKind::Entry { chain } => {
                let in_args = segment.in_args.as_slice(&synthesis.instance_pool);
                for (idx, in_arg) in in_args.iter().enumerate() {
                    let instance = &synthesis.instances[*in_arg];

                    let key = (chain, idx);

                    let arg = b.block_arg_insert(block);
                    assert!(!entry_map.contains_key(&key));
                    entry_map.insert(key, arg);

                    global_map.insert(*in_arg, arg);

                    if instance.is_relevant() {
                        let node = instance.node();
                        let node_kind = graph.node(node);

                        match node_kind {
                            NodeKind::EntryArg(entry_arg) => {
                                assert!(entry_arg.chain == chain);
                                assert!(entry_arg.arg_index == idx);
                            }
                            _ => unreachable!(),
                        }
                    }
                }
            }
            SegmentHeadKind::Intermediate => {}
        }
    }

    trace!("ENTRY MAP {:?}", entry_map);

    for (from, to) in graph.iter_uniform_mappings() {
        let to_kind = graph.node(to);
        match to_kind {
            NodeKind::Scope(value) => {
                trace!("Uniform mapping: {} -> {} ({})", from, to, value);
                map.insert(from, *value);
            }
            NodeKind::EntryArg(entry_arg) => {
                if synthesis.substitutions.contains_key(&entry_arg.chain) {
                    continue;
                }

                let key = (entry_arg.chain, entry_arg.arg_index);
                trace!("Entry {:?}", key);
                let to_value = entry_map[&key];
                trace!("Uniform mapping: {} -> {} ({})", from, to, to_value);
                map.insert(from, to_value);
            }
            _ => unreachable!(),
        }
    }

    for segment_id in synthesis.order.iter() {
        local_map.clear();

        let segment = &synthesis.segments[*segment_id];
        let segment_chains = segment.chains.bind(&synthesis.chain_set_pool);
        let all_chains = segment_chains.size() == graph.chain_count();
        trace!("SEGMENT {} {:?}", segment_id, AuxImpl(segment, synthesis));

        let block = segment_map[segment_id];
        let block_val = b.value(block);

        let in_args = segment.in_args.as_slice(&synthesis.instance_pool);
        trace!("IN ARGS {:?}", in_args);

        match segment.head {
            SegmentHeadKind::Entry { chain } => {
                let old_block = graph.get_chain_entry_block(chain);
                let old_block_val = b.value(old_block);

                assert!(!map.contains_key(&old_block_val));
                map.insert(old_block_val, block_val);

                for (idx, in_arg) in in_args.iter().enumerate() {
                    let key = (chain, idx);
                    let instance = &synthesis.instances[*in_arg];

                    match instance {
                        InstanceKind::NotRelevant => (), // TODO
                        InstanceKind::Arg { node, .. } => {
                            let node = instance.node();
                            let arg = entry_map[&key];

                            global_map.insert(*in_arg, arg);
                            local_map.insert(node, arg);
                        }
                        _ => unreachable!(),
                    }
                }
            }
            SegmentHeadKind::Intermediate => {
                for (idx, in_arg) in in_args.iter().enumerate() {
                    let instance = &synthesis.instances[*in_arg];

                    let arg = b.block_arg_insert(block);

                    match instance {
                        InstanceKind::NotRelevant => (), // TODO
                        InstanceKind::Arg { node, .. } => {
                            let node = instance.node();

                            global_map.insert(*in_arg, arg);
                            local_map.insert(node, arg);
                        }
                        _ => unreachable!(),
                    }
                }
            }
        }

        for external in segment.externals.as_slice(&synthesis.instance_pool) {
            let val = global_map[external];
            let instance = &synthesis.instances[*external];
            local_map.insert(instance.node(), val);
        }

        walker.clear();
        for instance_id in segment.instances.as_slice(&synthesis.instance_pool) {
            let instance = &synthesis.instances[*instance_id];
            walker.put(instance.node());
        }

        let mut order: Vec<Node> = Vec::new();
        while let Some(node_id) = walker.next() {
            if local_map.contains_key(&node_id) {
                continue;
            }
            order.push(node_id);

            let node = graph.node(node_id);

            match node {
                NodeKind::Phi(phi) => {
                    let mut pred_node = None;
                    for (chain, n_pred_node) in phi.entries.iter() {
                        if segment_chains.contains(*chain) {
                            if let Some(old_pred_node) = pred_node {
                                assert!(*n_pred_node == old_pred_node);
                            }
                            pred_node = Some(*n_pred_node);
                        }
                    }
                    if let Some(pred_node) = pred_node {
                        walker.put(pred_node);
                    }
                }
                _ => {
                    for pred_node in graph.node(node_id).dependencies() {
                        walker.put(pred_node);
                    }
                }
            }
        }

        trace!("ORDER {:?}", order);

        for node_id in order.iter().rev() {
            match graph.node(*node_id) {
                NodeKind::Scope(value) => {
                    local_map.insert(*node_id, *value);
                }
                NodeKind::EntryArg(entry_arg) => {
                    assert!(local_map.contains_key(node_id));
                    //let key = (entry_arg.chain, entry_arg.arg_index);
                    //if !local_map.contains_key(node_id) {
                    //    let new_entry_arg = entry_map[&key];
                    //    local_map.insert(*node_id, new_entry_arg);
                    //}
                }
                NodeKind::Phi(phi) => {
                    trace!("PHI {:#?}, {:?}", phi, segment_chains);

                    if let Some(select_node) = phi
                        .entries
                        .iter()
                        .find(|(chain, _node)| segment_chains.contains(**chain))
                        .map(|(_chain, node)| *node)
                    {
                        let from_value = local_map[&select_node];
                        local_map.insert(*node_id, from_value);
                    } else {
                        unreachable!()
                        //let from_value = phi.value.unwrap();
                        //local_map.insert(*node_id, from_value);
                    }
                }
                NodeKind::Prim(prim) => {
                    let mut map_value = |val| -> Option<Value> {
                        let node = prim.dependencies.get(&val);
                        node.map(|node| local_map[node])
                    };
                    let new_prim = b.value_map(prim.prim, &mut map_value);
                    local_map.insert(*node_id, new_prim);
                }
                NodeKind::BlockCapture(bc) => {
                    let capture;
                    if all_chains || bc.dependencies.len() == 0 {
                        for (from_val, to_node) in bc.dependencies.iter() {
                            let to_val = local_map[to_node];
                            map.insert(*from_val, to_val);
                        }
                        capture = bc.capture;
                    } else {
                        mangler.start(MangleTo(bc.capture));
                        for (from_val, to_node) in bc.dependencies.iter() {
                            let to_val = local_map[to_node];
                            mangler.add_rename(MangleTo(*from_val), MangleTo(to_val));
                        }
                        trace!("blockcapturemangle!");
                        capture = mangler.run(b);
                    }
                    let capture_val = b.value(capture);
                    local_map.insert(*node_id, capture_val);
                }
            }
        }

        for instance_id in segment.instances.as_slice(&synthesis.instance_pool) {
            let instance = &synthesis.instances[*instance_id];
            global_map.insert(*instance_id, local_map[&instance.node()]);
        }

        match &segment.body {
            SegmentBodyKind::None => unreachable!(),
            SegmentBodyKind::Terminal { single: true } => {
                let mut map_value = |val| -> Option<Value> {
                    let node = graph.get_root(val);
                    node.map(|node| local_map[&node])
                };
                b.block_copy_body_map(target, block, &mut map_value);

                for (root_val, root_node) in graph.iter_roots() {
                    let to_val = local_map[&root_node];
                    map.insert(root_val, to_val);
                }
            }
            SegmentBodyKind::Terminal { single: false } => {
                let mut map_value = |val| -> Option<Value> {
                    let node = graph.get_root(val);
                    node.map(|node| local_map[&node])
                };
                b.block_copy_body_map(target, block, &mut map_value);
            }
            SegmentBodyKind::ToIntermediate { to, out_args } => {
                let to_block = segment_map[to];

                arg_buf.clear();
                for instance_id in out_args.as_slice(&synthesis.instance_pool) {
                    arg_buf.push(global_map[instance_id]);
                }

                b.op_call_flow(block, to_block, &arg_buf);
            }
        }
    }
}
