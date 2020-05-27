use super::super::{Chain, ChainGraph};
use super::{Synthesis, SynthesisStrategy};
use crate::util::Walker;
use libeir_ir::Function;

pub fn can_subsitute(graph: &ChainGraph, fun: &Function, chain: Chain) -> bool {
    let target = graph.target_block;
    let target_reads = fun.block_reads(target);
    let chain_data = &graph.chains[chain];

    if fun.block_kind(target).unwrap().is_call() && chain_data.args.len() == target_reads.len() - 1
    {
        chain_data
            .args
            .iter()
            .zip(target_reads.iter().skip(1))
            .map(|(ch, val)| -> Option<()> {
                let ch = (*ch)?;

                let node;
                if let Some(to) = graph.get_uniform(*val) {
                    node = to;
                } else {
                    let start_node = graph.get_root(*val)?;
                    node = graph.follow_chain(start_node, chain);
                }

                if node == ch {
                    Some(())
                } else {
                    None
                }
            })
            .all(|v| v.is_some())
    } else {
        false
    }
}

/// A synthesis strategy that only handles the case of a single entry.
/// This will inline the destination into the entry.
///
///   Entry:
///     Target OP
///
pub struct SingleStrategy;

impl SynthesisStrategy for SingleStrategy {
    fn try_run(&self, graph: &ChainGraph, fun: &Function) -> Option<Synthesis> {
        if graph.chains.len() != 1 {
            return None;
        }
        let chain = graph.chains.keys().next().unwrap();

        let mut synthesis = Synthesis::new();

        let target = graph.target_block;
        let target_reads = fun.block_reads(target);

        if can_subsitute(graph, fun, chain) {
            // If the operation in the target is a call, and all arguments passed to
            // the call are equal to the entry edge, we can do a simple
            // substitution.

            synthesis.substitute(chain, target_reads[0]);
        } else {
            let segment = synthesis.create_entry_segment(chain, graph);
            let mut sb = synthesis.segment(segment);

            for target in graph.entry_edges.values() {
                sb.push_instance(*target);
            }

            sb.finish_target(true);

            synthesis.visit_segment(segment);
        }

        Some(synthesis)
    }
}
