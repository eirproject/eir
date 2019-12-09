use std::collections::BTreeMap;

use libeir_ir::FunctionBuilder;
use libeir_ir::{Block, OpKind, CallKind};
use libeir_ir::{LiveBlockGraph, LiveValues};

use super::SimplifyCfgPass;
use super::analyze;

macro_rules! copy_map_fun_single {
    ($chain_analysis:expr, $entry_analysis:expr) => {
        |mut val| {
            loop {
                if let Some(v) = $chain_analysis.static_map.get(&val) {
                    val = *v;
                    continue;
                }
                if let Some(v) = $entry_analysis.mappings.get(&val) {
                    match v {
                        analyze::PhiSource::Scope(iv) => {
                            val = *iv;
                            continue;
                        },
                        analyze::PhiSource::EntryArg { arg, .. } => {
                            break Some(*arg);
                        }
                    }
                }
                break Some(val);
            }
        }
    }
}

macro_rules! copy_map_fun {
    ($chain_analysis:expr, $entry_analysis:expr) => {
        |mut val| {

            loop {
                // If the argument is of the original target block, it will loop around.
                // Do not map this value.
                if $chain_analysis.args.contains(&val) {
                    break Some(val);
                }

                if let Some(v) = $chain_analysis.static_map.get(&val) {
                    val = *v;
                    continue;
                }
                if let Some(v) = $entry_analysis.mappings.get(&val) {
                    match v {
                        analyze::PhiSource::Scope(iv) => {
                            val = *iv;
                            continue;
                        },
                        analyze::PhiSource::EntryArg { arg, .. } => {
                            break Some(*arg);
                        }
                    }
                }
                break Some(val);
            }
        }
    }
}

pub fn rewrite(
    target: Block,
    pass: &mut SimplifyCfgPass,
    analysis: &analyze::GraphAnalysis,
    live: &LiveValues,
    b: &mut FunctionBuilder,
) {
    let graph = b.fun().live_block_graph();
    let chain_analysis = analyze::analyze_chain(
        target, &b.fun(), &graph, &live, &analysis);
    dbg!(&chain_analysis);

    if chain_analysis.renames_required {
        rewrite_chain_generic(pass, &analysis, &chain_analysis, b);
    } else {
        // When the renames from the chain are not used outside of
        // the target block, we can generate better IR.
        rewrite_chain_norenames(pass, &analysis, &chain_analysis, b);
    }
}

fn rewrite_chain_generic(
    pass: &mut SimplifyCfgPass,
    analysis: &analyze::GraphAnalysis,
    chain_analysis: &analyze::ChainAnalysis,
    b: &mut FunctionBuilder,
) {
    println!("GENERIC");

    for (from, to) in chain_analysis.static_map.iter() {
        pass.map.insert(*from, *to);
    }

    if chain_analysis.entry_edges.len() == 0 { return; }

    if chain_analysis.entry_edges.len() == 1 /* || chain_analysis.args.len() == 0 */ {
        // This code path copies the body of the target into each callee block.
        // This is safe to do when any of the two below conditions are met:
        // * There is only one entry edge. In this case any required renames
        //   can become static renames.
        // * There are no required renames.

        for edge in chain_analysis.entry_edges.clone() {
            let entry_analysis = analyze::analyze_entry_edge(
                &analysis, &chain_analysis, edge);

            println!("Path 1");
            dbg!(&entry_analysis);

            // If the target is the same as the callee, then this is already optimal.
            if chain_analysis.target == entry_analysis.callee { return; }

            // In the case where there is only one entry edge, all mappings become static.
            for (from, to) in entry_analysis.mappings.iter() {
                match *to {
                    analyze::PhiSource::Scope(to_value) => {
                        pass.map.insert(*from, to_value);
                    }
                    analyze::PhiSource::EntryArg { called, arg, .. } => {
                        assert!(called == entry_analysis.callee);
                        pass.map.insert(*from, arg);
                    }
                }
            }

            // Clear the callee block and copy the target block body.
            b.block_clear(entry_analysis.callee);
            b.block_copy_body_map(
                chain_analysis.target,
                entry_analysis.callee,
                &mut copy_map_fun_single!(chain_analysis, entry_analysis),
            );
        }
    } else {

        // Create the new terminal block
        let new_target_block = b.block_insert();

        // Insert the arguments and insert the mappings for them
        let mut new_target_args = Vec::new();
        for value in chain_analysis.args.iter() {
            let mapped = b.block_arg_insert(new_target_block);
            new_target_args.push(mapped);
            pass.map.insert(*value, mapped);
        }

        // Copy the body of the target to the new target block.
        b.block_copy_body_map(chain_analysis.target, new_target_block,
                              &mut |val| Some(val));

        // Rewrite all entry edges
        for edge in chain_analysis.entry_edges.clone() {
            let entry_analysis = analyze::analyze_entry_edge(
                &analysis, &chain_analysis, edge);
            println!("Path 2");
            dbg!(&entry_analysis);

            //if chain_analysis.target == entry_analysis.callee { continue; }

            let new_intermediate = b.block_insert();
            let mut inter_map = BTreeMap::new();
            for prev_arg_n in 0..b.fun().block_args(entry_analysis.callee).len() {
                let prev_arg = b.fun().block_args(entry_analysis.callee)[prev_arg_n];
                let new_arg = b.block_arg_insert(new_intermediate);
                inter_map.insert(prev_arg, new_arg);
                if !pass.map.contains_key(&prev_arg) {
                    pass.map.insert(prev_arg, new_arg);
                }
            }
            pass.map.insert(
                b.value(entry_analysis.callee),
                b.value(new_intermediate),
            );

            let args: Vec<_> = entry_analysis.args.iter()
                .map(|v| {
                    b.value_map(
                        v.value(),
                        &mut copy_map_fun!(chain_analysis, entry_analysis),
                    )
                })
                .collect();
            b.op_call_flow(new_intermediate, new_target_block, &args);
            b.block_value_map(new_intermediate, &mut |v| {
                inter_map.get(&v).cloned().unwrap_or(v)
            });

        }

    }

}

fn rewrite_chain_norenames(
    pass: &mut SimplifyCfgPass,
    analysis: &analyze::GraphAnalysis,
    chain_analysis: &analyze::ChainAnalysis,
    b: &mut FunctionBuilder,
) {

    for (from, to) in chain_analysis.static_map.iter() {
        pass.map.insert(*from, *to);
    }

    match b.fun().block_kind(chain_analysis.target).unwrap() {
        OpKind::Call(CallKind::ControlFlow) => {
            for edge in chain_analysis.entry_edges.clone() {

                let entry_analysis = analyze::analyze_entry_edge(
                    &analysis, &chain_analysis, edge);
                println!("Path 3");
                dbg!(&entry_analysis);

                let call_target_equal_to_callee = {
                    let fun = b.fun();

                    let c = &fun.block_reads(chain_analysis.target)[1..];
                    let t = fun.block_args(entry_analysis.callee);

                    c.len() == t.len() && c.iter().zip(t.iter())
                        .all(|(from, to)| {
                            let mapped = chain_analysis.static_map.get(from)
                                .map(|v| analyze::PhiSource::Scope(*v))
                                .or_else(|| entry_analysis.mappings.get(from).cloned())
                                .unwrap_or(analyze::PhiSource::Scope(*from))
                                .value();
                            mapped == *to
                        })
                };

                if call_target_equal_to_callee {
                    // The callee can be replaced by the call target
                    let callee_val = b.value(entry_analysis.callee);
                    let call_target = b.fun().block_reads(
                        chain_analysis.target)[0];
                    pass.map.insert(callee_val, call_target);
                } else {
                    // If the callee is the target, we don't change anything
                    if entry_analysis.callee == chain_analysis.target { continue; }

                    b.block_clear(entry_analysis.callee);
                    b.block_copy_body_map(
                        chain_analysis.target,
                        entry_analysis.callee,
                        &mut copy_map_fun_single!(chain_analysis, entry_analysis),
                    );
                }

            }
        }
        _ => rewrite_chain_generic(pass, analysis, chain_analysis, b),
    }
}
