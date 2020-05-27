use log::trace;

use super::super::ChainGraph;
use super::{Synthesis, SynthesisStrategy};
use libeir_ir::Function;

/// Synthesis strategy that dispatches to any number of other strategies
/// depending on the chain graph.
pub struct CompoundStrategy;

impl SynthesisStrategy for CompoundStrategy {
    fn try_run(&self, graph: &ChainGraph, fun: &Function) -> Option<Synthesis> {
        trace!("TS1");
        // Try the single strategy.
        // Will only succeed if there is a single entry.
        let single_strategy = super::single::SingleStrategy;
        if let Some(s) = single_strategy.try_run(graph, fun) {
            return Some(s);
        }

        trace!("TS2");
        let single_strategy = super::terminating_target::TerminatingTargetStrategy;
        if let Some(s) = single_strategy.try_run(graph, fun) {
            return Some(s);
        }

        trace!("TS3");
        // Fall back to simple strategy.
        // This will work in every case (except for single entry, this is
        // handled by SingleStrategy above).
        let simple_strategy = super::simple::SimpleStrategy;
        Some(simple_strategy.try_run(graph, fun).unwrap())
    }
}
