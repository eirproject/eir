use std::collections::BTreeSet;

use libeir_ir::FunctionBuilder;
use libeir_ir::{Block, OpKind};
use libeir_ir::{MangleTo, Mangler};

use super::FunctionPass;

#[cfg(test)]
mod tests;

/// Very basic closure inlining pass.
pub struct NaiveInlineClosuresPass {
    calls_buf: Vec<(Block, Block)>,
    mangler: Mangler,
}

impl NaiveInlineClosuresPass {
    pub fn new() -> Self {
        NaiveInlineClosuresPass {
            calls_buf: Vec::new(),
            mangler: Mangler::new(),
        }
    }
}

impl FunctionPass for NaiveInlineClosuresPass {
    fn name(&self) -> &str {
        "naive_inline_closures"
    }
    fn run_function_pass(&mut self, b: &mut FunctionBuilder) {
        self.inline_closures(b);
    }
}

impl NaiveInlineClosuresPass {
    pub fn inline_closures(&mut self, b: &mut FunctionBuilder) {
        self.calls_buf.clear();

        let live_block_graph = b.fun().live_block_graph();

        let sccs = petgraph::algo::kosaraju_scc(&live_block_graph);

        let mut in_big_scc = BTreeSet::new();
        for scc in sccs.iter() {
            if scc.len() > 1 {
                for node in scc.iter() {
                    in_big_scc.insert(*node);
                }
            }
        }

        for block in b.fun().block_graph().dfs_post_order_iter() {
            // We perform inlining if the block satisfies the following
            // conditions:
            // 1. The block is a call operation
            // 2. The target of the call operation is another block
            // 3. There is at least one block argument to the block
            // 4. The target block does not form a cycle to itself in the block graph

            // OP must be Call
            if let OpKind::Call(_) = b.fun().block_kind(block).unwrap() {
                let reads = b.fun().block_reads(block);

                // Call target must be block
                let target = match b.fun().value_block(reads[0]) {
                    Some(t) => t,
                    None => continue,
                };

                // Arguments must contain block

                let contains_block = reads[1..].iter().any(|a| b.fun().value_block(*a).is_some());
                if !contains_block {
                    continue;
                }

                // Check if the target block is in a SCC with other nodes than
                // just itself. If this is the case, then it might call itself,
                // and we can't do inlining naively.
                // TODO: This might have false positives?
                if in_big_scc.contains(&target) {
                    continue;
                }

                self.calls_buf.push((block, target));
            }
        }

        for (block, target) in self.calls_buf.iter().cloned() {
            // Signature of new entry block has no arguments
            let new_target = b.block_insert();
            b.block_copy_body_map(target, new_target, |v| Some(v));

            self.mangler.start(MangleTo(new_target));

            // Add renames to current call arguments
            {
                let source_args = &b.fun().block_reads(block)[1..];
                let target_args = b.fun().block_args(target);
                assert!(source_args.len() == target_args.len());

                for (from, to) in target_args.iter().zip(source_args.iter()) {
                    self.mangler
                        .add_rename_nofollow(MangleTo(*from), MangleTo(*to));
                }
            }

            // Run mangling on the scope
            let new_block = self.mangler.run(b);

            // Clear the current block and insert a call to the new block
            b.block_clear(block);
            b.op_call_flow(block, new_block, &[]);
        }
    }
}
