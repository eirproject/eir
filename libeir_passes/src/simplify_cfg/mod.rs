#![warn(warnings)]

use std::collections::BTreeMap;

use log::{debug, trace};

use bumpalo::Bump;
use fnv::FnvBuildHasher;
use hashbrown::HashMap;
type BFnvHashMap<'bump, K, V> = HashMap<K, V, FnvBuildHasher, &'bump Bump>;

use libeir_ir::Value;
use libeir_ir::{FunctionBuilder, MangleTo, Mangler, StandardFormatConfig};

use super::FunctionPass;

mod analyze;
mod chain_graph;
mod rewrite;

use chain_graph::synthesis::SynthesisStrategy;

#[cfg(test)]
mod tests;

/// This pass is a powerful cleanup pass, it will do the following:
/// - Inline closures
/// - Remove needless call chains
pub struct SimplifyCfgPass {
    map: BTreeMap<Value, Value>,

    mangler: Mangler,

    bump: Option<Bump>,
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
            bump: Some(Bump::new()),
        }
    }
}

impl FunctionPass for SimplifyCfgPass {
    fn name(&self) -> &str {
        "simplify_cfg"
    }
    fn run_function_pass(&mut self, b: &mut FunctionBuilder) {
        self.simplify_cfg(b);
    }
}

impl SimplifyCfgPass {
    fn simplify_cfg(&mut self, b: &mut FunctionBuilder) {
        let mut bump = self.bump.take().unwrap();

        let entry = b.fun().block_entry();
        let graph = b.fun().live_block_graph();
        let live = b.fun().live_values();

        let block_order: Vec<_> = graph.dfs_post_order_iter().collect();
        trace!("BLOCK ORDER {:?}", block_order);

        trace!("{}", b.fun().to_text(&mut StandardFormatConfig::default()));

        {
            let analysis = analyze::analyze_graph(&bump, b.fun(), &graph);
            trace!("analysis = {:#?}", analysis);
            trace!("analysis done");

            for block in block_order.iter() {
                if let Some(_blocks) = analysis.trees.get(block) {
                    let target = block;

                    // Synthesize CFG for chain
                    let graph = b.fun().live_block_graph();
                    let chain_graph =
                        analyze::analyze_chain(&bump, *target, &b.fun(), &graph, &live, &analysis);

                    let synthesis_impl = chain_graph::synthesis::compound::CompoundStrategy;
                    let mut synthesis = synthesis_impl
                        .try_run(&chain_graph, b.fun(), &live)
                        .unwrap();
                    synthesis.postprocess(&chain_graph);

                    trace!("{:#?}", synthesis);

                    //// .. and apply it to the CFG.
                    rewrite::rewrite(b, &mut self.map, *target, &chain_graph, &synthesis);

                    trace!("{}", b.fun().to_text_standard());
                }
            }

            trace!("rewrite done");

            self.mangler.start(MangleTo(entry));
            for (from, to) in self.map.iter() {
                self.mangler.add_rename(MangleTo(*from), MangleTo(*to));
            }

            let new_entry = self.mangler.run(b);
            b.block_set_entry(new_entry);

            trace!("{}", b.fun().to_text_standard());
        }

        self.map.clear();
        bump.reset();
        self.bump = Some(bump);
    }
}
