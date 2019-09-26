use std::collections::{HashSet, BTreeMap, BTreeSet};

use petgraph::Graph;

use libeir_ir::{Function, FunctionBuilder, Mangler};
use libeir_ir::{Value, Block, OpKind};
use libeir_ir::{LiveValues, LiveBlockGraph};

use cranelift_entity::{PrimaryMap, entity_impl};

use super::FunctionPass;

mod analyze;

#[cfg(test)]
mod tests;

/// This pass is a powerful cleanup pass, it will do the following:
/// - Inline closures
/// - Remove needless call chains
pub struct SimplifyCfgPass {
    map: BTreeMap<Value, Value>,

    mangler: Mangler,
}

// Observations about the pass:
// * A call to a block can serve "two purposes" from the point of view of this pass.
//   1. A temporary rename within the call chain we are operating on. We don't really
//      care about the value in this case, it can be removed.
//   2. A bind that blocks in the scope of the target block use. In this case we need
//      to make sure the value is always bound to a singular value that the code after
//      uses.
//   3. Both. Needs to be handled exactly the same as 2.
//
//
// The functionality of the pass (rewriting call chains), operates as follows:
// 1. CFG analysis:
//    Locates subgraphs of the CFG that are call chains.
//    These call chains can take one of two forms:
//    * A cyclic graph:
//      This call chain ends in a cycle. This means there is a cycle without
//      control flow or side effects. Any entry edges to the call chain should
//      be rewritten to a call to a operation that sleeps the process forever.
//    * A tree:
//      This is a normal call chain ending in a single target block.
//
//      The algorithm proceeds as follows for each tree:
//      1. Value rename analysis:
//         Each edge in a call chain can have any number of value renames.
//         These form a set of conceptual "phi nodes",
//         `phi_map: Map<Value, Set<(Block, Value)>>`.
//      2. Let target_live be the live value set of the target block
//      3. Generate rename map:
//         Repeat until stable:
//         * For every key k in the union between phi_nodes and target_live:
//           * For every entry (_, v) in the target set:
//             * If v is in phi_nodes:
//               * Add the entries of v to the entries in k
//      4. Rewrite target:
//         Let there be two variables, call_target and call_args
//         * If the body of the target block is a call to a value
//           and
//           the set of args in the call is equal to the live set of the target:
//           Set call_target to the value of the call, and call_args to the args.
//         * Else:
//           Insert a new block with n arguments
//               where n is the number of elements in the rename map
//           Insert renames for the args into the mangler
//           Set call_target to the new block, and call_args to the elements in the
//           rename map
//           Copy target body to the new block
//      5. Rewrite calls
//         * If:
//           * The call target is a value
//           * and for every read in the call that is a block, none of the phi
//             values for the chain are live in that block
//           Then:
//           * Replace any callee blocks directly with the value if the sigature matches
//           * Else, replace the body of callee blocks a call to the value
//         * Else, generate a new target block with all the chain phis as arguments,
//           insert mangle mappings for those new arguments.
//           For every entry edge into the chain, clear the body of the callee and
//           generate a call to the new target block

impl SimplifyCfgPass {

    pub fn new() -> Self {
        SimplifyCfgPass {
            map: BTreeMap::new(),
            mangler: Mangler::new(),
        }
    }

}

impl FunctionPass for SimplifyCfgPass {
    fn run_function_pass(&mut self, b: &mut FunctionBuilder) {
        println!("SimplifyCfgPass");
        //self.simplify_cfg(b);
        self.simplify_cfg_2(b);
    }
}

impl SimplifyCfgPass {

    fn simplify_cfg_2(&mut self, b: &mut FunctionBuilder) {
        // TODO: Allocate data structures in pass

        self.map.clear();

        let entry = b.fun().block_entry();
        let graph = b.fun().live_block_graph();
        let live = b.fun().live_values();

        let analysis = analyze::analyze_graph(b.fun(), &graph);

        println!("Before simplify: {}", b.fun().to_text());
        println!("{:#?}", analysis);

        for (target, blocks) in analysis.chains.iter() {
            // TODO: Temporarily skip a chain if it includes the entry block.
            // We should fix this by instead generating a new entry block.
            if blocks.contains(&entry) { continue; }

            // TODO remove
            let graph = b.fun().live_block_graph();
            let chain_analysis = analyze::analyze_chain(
                *target, &b.fun(), &graph, &live, &analysis);

            if chain_analysis.renames_required {
                self.rewrite_chain_generic(&analysis, &chain_analysis, b);
            } else {
                // When the renames from the chain are not used outside of
                // the target block, we can generate better IR.
                self.rewrite_chain_norenames(&analysis, &chain_analysis, b);
            }

        }

        println!("Before simplify mangle: {}", b.fun().to_text());

        self.mangler.start(entry, b);
        self.mangler.copy_entry(b);
        println!("Map: {:?}", self.map);
        for (from, to) in self.map.iter() {
            self.mangler.add_rename(*from, *to);
        }
        let new_entry = self.mangler.run(b);
        b.block_set_entry(new_entry);

        println!("After simplify: {}", b.fun().to_text());
    }

    fn rewrite_chain_generic(
        &mut self,
        analysis: &analyze::GraphAnalysis,
        chain_analysis: &analyze::ChainAnalysis,
        b: &mut FunctionBuilder,
    ) {

        println!("====");
        println!("{:#?}", chain_analysis);

        for (from, to) in chain_analysis.static_map.iter() {
            self.map.insert(*from, *to);
        }

        if chain_analysis.entry_edges.len() == 1 || chain_analysis.args.len() == 0 {

            let entry_analysis = analyze::analyze_entry_edge(
                &analysis, &chain_analysis, 0);
            println!("EE: {:#?}", entry_analysis);

            if chain_analysis.target == entry_analysis.callee { return; }

            for value in chain_analysis.args.iter() {
                let mapped = entry_analysis.mappings[value];
                self.map.insert(*value, mapped);
            }

            b.block_clear(entry_analysis.callee);
            b.block_copy_body_map(
                chain_analysis.target,
                entry_analysis.callee,
                &|val| Some(val),
            );

        } else {

            // Create the new terminal block
            let new_target_block = b.block_insert();

            // Insert the arguments and insert the mappings for them
            let mut new_target_args = Vec::new();
            for value in chain_analysis.args.iter() {
                let mapped = b.block_arg_insert(new_target_block);
                new_target_args.push(mapped);
                self.map.insert(*value, mapped);
            }

            b.block_copy_body_map(chain_analysis.target, new_target_block,
                                  &|val| Some(val));

            for n in 0..chain_analysis.entry_edges.len() {
                let entry_analysis = analyze::analyze_entry_edge(
                    &analysis, &chain_analysis, n);
                println!("EE: {:#?}", entry_analysis);

                if chain_analysis.target == entry_analysis.callee { continue; }

                b.block_clear(entry_analysis.callee);
                // TODO: Map args with entry_analysis.mappings
                b.op_call(entry_analysis.callee, new_target_block,
                          &entry_analysis.args);
            }

        }

    }

    fn rewrite_chain_norenames(
        &mut self,
        analysis: &analyze::GraphAnalysis,
        chain_analysis: &analyze::ChainAnalysis,
        b: &mut FunctionBuilder,
    ) {
        println!("====");
        println!("{:#?}", chain_analysis);

        for (from, to) in chain_analysis.static_map.iter() {
            self.map.insert(*from, *to);
        }

        match b.fun().block_kind(chain_analysis.target).unwrap() {
            OpKind::Call => {
                for n in 0..chain_analysis.entry_edges.len() {

                    let entry_analysis = analyze::analyze_entry_edge(
                        &analysis, &chain_analysis, n);
                    println!("EE: {:#?}", entry_analysis);

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
                        self.map.insert(callee_val, call_target);
                    } else {
                        // If the callee is the target, we don't change anything
                        if entry_analysis.callee == chain_analysis.target { continue; }

                        b.block_clear(entry_analysis.callee);
                        b.block_copy_body_map(
                            chain_analysis.target,
                            entry_analysis.callee,
                            &|val| Some(entry_analysis.mappings.get(&val)
                                        .cloned().unwrap_or(val))
                        );
                    }

                }
            }
            _ => self.rewrite_chain_generic(analysis, chain_analysis, b),
        }
    }

}
