use log::trace;

use libeir_ir::Function;

use crate::util::Walker;
use super::{SynthesisStrategy, Synthesis};
use super::single::can_subsitute;
use super::super::ChainGraph;

/// A synthesis strategy that specializes the target into every entry.
/// This is only valid for target blocks that terminate/don't use any values
/// from the chain.
///
///   Entry1:          Entry2:
///     Target OP        Target OP
///    
pub struct TerminatingTargetStrategy;

impl SynthesisStrategy for TerminatingTargetStrategy {

    fn try_run(&self, graph: &ChainGraph, fun: &Function) -> Option<Synthesis> {
        // TODO change this to check outgoing values instead

        let target = graph.target_block;
        let target_reads = fun.block_reads(target);

        if !(fun.block_kind(target).unwrap().is_call()
             && fun.value_kind(target_reads[0]).is_arg())
        {
            return None;
        }
        trace!("TERMINATING TARGET STRATEGY");

        let mut synthesis = Synthesis::new();

        for chain in graph.chains.keys() {
            if can_subsitute(graph, fun, chain){
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
