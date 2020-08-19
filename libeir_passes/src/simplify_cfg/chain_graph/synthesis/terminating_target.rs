use log::trace;

use libeir_ir::{Function, LiveValues};

use super::super::ChainGraph;
use super::single::can_subsitute;
use super::{Synthesis, SynthesisStrategy};

/// A synthesis strategy that specializes the target into every entry.
/// This is only valid for target blocks that terminate/don't use any values
/// from the chain.
///
///   Entry1:          Entry2:
///     Target OP        Target OP
///    
pub struct TerminatingTargetStrategy;

impl SynthesisStrategy for TerminatingTargetStrategy {
    fn try_run(&self, graph: &ChainGraph, fun: &Function, live: &LiveValues) -> Option<Synthesis> {
        // TODO change this to check outgoing values instead

        let target = graph.target_block;
        let target_reads = fun.block_reads(target);

        // This strategy can only be run if NONE of the values defined
        // conditionally in the graph are used outside of the target block.
        // We validate this by going through all outgoing edges, and checking
        // that none of the graph entry edges are live at those block heads.
        let bg = fun.block_graph();
        for outgoing in bg.outgoing(target) {
            for val in live.live_at(outgoing).iter() {
                if graph.entry_edges.contains_key(&val) {
                    return None;
                }
            }
        }

        trace!("TERMINATING TARGET STRATEGY");

        let mut synthesis = Synthesis::new();

        for chain in graph.chains.keys() {
            if can_subsitute(graph, fun, chain) {
                synthesis.substitute(chain, target_reads[0]);
            } else {
                let segment = synthesis.create_entry_segment(chain, graph);
                let mut sb = synthesis.segment(segment);

                for target in graph.entry_edges.values() {
                    sb.push_instance(*target);
                }

                sb.finish_target(false);

                synthesis.visit_segment(segment);
            }
        }

        Some(synthesis)
    }
}
