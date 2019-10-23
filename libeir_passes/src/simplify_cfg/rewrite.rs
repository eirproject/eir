use std::collections::BTreeSet;

use libeir_ir::FunctionBuilder;
use libeir_ir::{Value, OpKind};

use super::SimplifyCfgPass;
use super::analyze;

macro_rules! copy_map_fun {
    ($chain_analysis:expr, $entry_analysis:expr) => {
        |mut val| {
            // If the argument is of the original target block, it will loop around.
            // Do not map this value.
            if $chain_analysis.args.contains(&val) {
                return None;
            }

            loop {
                if let Some(v) = $chain_analysis.static_map.get(&val) {
                    val = *v;
                    continue;
                }
                if let Some(v) = $entry_analysis.mappings.get(&val) {
                    val = *v;
                    continue;
                }
                break;
            }
            Some(val)
        }
    }
}

pub fn rewrite_chain_generic(
    pass: &mut SimplifyCfgPass,
    analysis: &analyze::GraphAnalysis,
    chain_analysis: &analyze::ChainAnalysis,
    b: &mut FunctionBuilder,
) {

    for (from, to) in chain_analysis.static_map.iter() {
        pass.map.insert(*from, *to);
    }

    if chain_analysis.entry_edges.len() == 0 { return; }

    if chain_analysis.entry_edges.len() == 1 || chain_analysis.args.len() == 0 {

        let entry_analysis = analyze::analyze_entry_edge(
            &analysis, &chain_analysis, 0);
        println!("1");
        dbg!(&entry_analysis);

        if chain_analysis.target == entry_analysis.callee { return; }

        for value in chain_analysis.args.iter() {
            let mapped = entry_analysis.mappings[value];
            pass.map.insert(*value, mapped);
        }

        b.block_clear(entry_analysis.callee);
        b.block_copy_body_map(
            chain_analysis.target,
            entry_analysis.callee,
            &mut |val| Some(val),
        );

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

        b.block_copy_body_map(chain_analysis.target, new_target_block,
                              &mut |val| Some(val));

        for n in 0..chain_analysis.entry_edges.len() {
            let entry_analysis = analyze::analyze_entry_edge(
                &analysis, &chain_analysis, n);
            println!("2");
            dbg!(&entry_analysis);

            if chain_analysis.target == entry_analysis.callee { continue; }

            b.block_clear(entry_analysis.callee);
            // TODO: Map args with entry_analysis.mappings
            let args: Vec<_> = entry_analysis.args.iter()
                .map(|v| {
                    b.value_map(*v, &mut copy_map_fun!(chain_analysis, entry_analysis))
                })
                .collect();
            b.op_call(entry_analysis.callee, new_target_block,
                      &args);
        }

    }

}

pub fn rewrite_chain_norenames(
    pass: &mut SimplifyCfgPass,
    analysis: &analyze::GraphAnalysis,
    chain_analysis: &analyze::ChainAnalysis,
    b: &mut FunctionBuilder,
) {

    for (from, to) in chain_analysis.static_map.iter() {
        pass.map.insert(*from, *to);
    }

    match b.fun().block_kind(chain_analysis.target).unwrap() {
        OpKind::Call => {
            for n in 0..chain_analysis.entry_edges.len() {

                let entry_analysis = analyze::analyze_entry_edge(
                    &analysis, &chain_analysis, n);
                println!("3");
                dbg!(&entry_analysis);

                let call_target_equal_to_callee = {
                    let fun = b.fun();

                    let c = &fun.block_reads(chain_analysis.target)[1..];
                    let t = fun.block_args(entry_analysis.callee);

                    c.len() == t.len() && c.iter().zip(t.iter())
                        .all(|(from, to)| {
                            let mapped = chain_analysis.static_map.get(from)
                                .or_else(|| entry_analysis.mappings.get(from))
                                .unwrap_or(from);
                            mapped == to
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
                        &mut |mut val| {
                            loop {
                                if let Some(v) = chain_analysis.static_map.get(&val) {
                                    val = *v;
                                    continue;
                                }
                                if let Some(v) = entry_analysis.mappings.get(&val) {
                                    val = *v;
                                    continue;
                                }
                                break;
                            }
                            Some(val)
                        }
                    );
                }

            }
        }
        _ => rewrite_chain_generic(pass, analysis, chain_analysis, b),
    }
}
