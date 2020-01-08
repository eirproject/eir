use std::collections::BTreeMap;

use bumpalo::{Bump, collections::HashMap as BHashMap};
use fnv::FnvBuildHasher;
type BFnvHashMap<'bump, K, V> = BHashMap<K, V, &'bump Bump, FnvBuildHasher>;

use libeir_ir::{FunctionBuilder, Mangler, MangleTo};
use libeir_ir::Value;

use super::FunctionPass;

mod analyze;
mod rewrite;

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

        {
            let analysis = analyze::analyze_graph(&bump, b.fun(), &graph);
            //dbg!(&analysis);

            for (target, _blocks) in analysis.chains.iter() {
                //let graph = b.fun().live_block_graph();
                //let chain_analysis = analyze::analyze_chain(
                //    *target, &b.fun(), &graph, &live, &analysis);
                //dbg!(&chain_analysis);

                //for edge in chain_analysis.entry_edges.iter() {
                //    let entry_analysis = analyze::analyze_entry_edge(
                //        &analysis, &chain_analysis, *edge);
                //    dbg!(&entry_analysis);
                //}

                rewrite::rewrite(
                    &bump,
                    *target,
                    self,
                    &analysis,
                    &live,
                    b,
                );
            }

            //let mut new_entry = entry;
            //println!("BEF: {}", new_entry);
            //loop {
            //    let entry_val = b.fun().block_value(new_entry);
            //    if let Some(to_val) = self.map.get(&entry_val) {
            //        new_entry = b.fun().value_block(*to_val).unwrap();
            //    } else {
            //        break;
            //    }
            //}
            //println!("AFT: {}", new_entry);

            self.mangler.start(MangleTo(entry));
            for (from, to) in self.map.iter() {
                self.mangler.add_rename(MangleTo(*from), MangleTo(*to));
            }

            let mut print_ctx = libeir_ir::text::printer::ToEirTextContext::new();
            let mut out_str = Vec::new();
            for block in b.fun().block_iter() {
                use libeir_ir::text::printer::ToEirTextFun;
                block.to_eir_text_fun(&mut print_ctx, b.fun(), 0, &mut out_str).unwrap();
                out_str.push('\n' as u8);
            }
            let new_entry = self.mangler.run(b);
            b.block_set_entry(new_entry);

            let mut print_ctx = libeir_ir::text::printer::ToEirTextContext::new();
            let mut out_str = Vec::new();
            for block in b.fun().block_iter() {
                use libeir_ir::text::printer::ToEirTextFun;
                block.to_eir_text_fun(&mut print_ctx, b.fun(), 0, &mut out_str).unwrap();
                out_str.push('\n' as u8);
            }
        }

        self.map.clear();
        bump.reset();
        self.bump = Some(bump);
    }

}
