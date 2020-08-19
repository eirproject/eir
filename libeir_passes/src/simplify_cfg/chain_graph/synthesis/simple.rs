use log::trace;

use std::collections::BTreeSet;

use cranelift_entity::EntityList;
use libeir_ir::{Function, LiveValues};
use libeir_util_datastructures::pooled_entity_set::EntitySet;

use super::super::{ChainGraph, NodeKind};
use super::{SegmentHeadKind, Synthesis, SynthesisStrategy};
use crate::util::Walker;

/// A dead simple synthesis strategy. Works in every case.
/// All nodes before the first phi will be specialized.
///
/// Will always generate CFGs of this form:
///
///    Entry1    Entry2    Entry3
///      |         |         |
///      --------| | |--------
///              v v v
///              Target
pub struct SimpleStrategy;

impl SynthesisStrategy for SimpleStrategy {
    fn try_run(
        &self,
        graph: &ChainGraph,
        _fun: &Function,
        _live: &LiveValues,
    ) -> Option<Synthesis> {
        trace!("SIMPLE STRATEGY");

        let mut synthesis = Synthesis::new();

        let mut visited_set = BTreeSet::new();
        let mut before_ord = EntityList::new();
        let mut border_set = BTreeSet::new();

        // Walk and find all nodes we schedule in the target block, along with
        // the set of nodes that need actual phi nodes/call arguments.
        let mut walker = Walker::with(graph.entry_edges.values().cloned().collect());
        while let Some(node) = walker.next() {
            let node_data = &graph.nodes[node];
            visited_set.insert(node);
            before_ord.push(node, &mut synthesis.node_pool);
            if node_data.is_phi() {
                border_set.insert(node);
            } else {
                for dep in node_data.dependencies() {
                    walker.put(dep);
                }
            }
        }

        let out_segment = synthesis.create_intermediate_segment();
        {
            let mut osb = synthesis.segment(out_segment);

            // The border set are the arguments on the new target block
            for arg in border_set.iter() {
                osb.push_in_arg(*arg);
            }

            // Create the root values in the block
            for target in graph.entry_edges.values() {
                osb.push_instance(*target);
            }

            // This is the single target block, full mappings
            osb.finish_target(true);

            // Order doesn't matter, strategy doesn't use externals
            synthesis.visit_segment(out_segment);
        }

        for (chain, _chain_data) in graph.chains.iter() {
            let segment = synthesis.create_entry_segment(chain, graph);

            let mut sb = synthesis.segment(segment);
            for out in border_set.iter() {
                let instance = sb.push_instance(*out);
                sb.push_out_arg(instance);
            }

            sb.finish_to(out_segment);

            synthesis.visit_segment(segment);
        }

        Some(synthesis)
    }
}
