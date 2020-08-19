use log::trace;

use super::super::ChainGraph;
use super::{Synthesis, SynthesisStrategy};
use libeir_ir::{Function, LiveValues};

/// Synthesis strategy that dispatches to any number of other strategies
/// depending on the chain graph.
pub struct CompoundStrategy;

impl SynthesisStrategy for CompoundStrategy {
    fn try_run(&self, graph: &ChainGraph, fun: &Function, live: &LiveValues) -> Option<Synthesis> {
        trace!("Trying single entry strategy...");
        // Try the single strategy.
        // Will only succeed if there is a single entry.
        let single_strategy = super::single::SingleStrategy;
        if let Some(s) = single_strategy.try_run(graph, fun, live) {
            trace!("Single entry strategy succeeded!");
            return Some(s);
        }

        trace!("Trying terminating target strategy...");
        let single_strategy = super::terminating_target::TerminatingTargetStrategy;
        if let Some(s) = single_strategy.try_run(graph, fun, live) {
            trace!("Terminating target strategy succeeded!");
            return Some(s);
        }

        trace!("Falling back to simple strategy...");
        // Fall back to simple strategy.
        // This will work in every case (except for single entry, this is
        // handled by SingleStrategy above).
        let simple_strategy = super::simple::SimpleStrategy;
        Some(simple_strategy.try_run(graph, fun, live).unwrap())
    }
}
