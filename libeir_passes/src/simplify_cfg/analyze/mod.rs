use std::collections::{BTreeSet, VecDeque};

use bumpalo::{Bump, collections::Vec as BVec};
use super::BFnvHashMap;

use libeir_ir::{Block, Value, OpKind, ValueKind, CallKind};
use libeir_ir::{Function, LiveBlockGraph, LiveValues};

mod call;
mod if_bool;
mod unpack_value_list;

type BlockEdge = (Block, Block);

#[derive(Debug)]
pub struct GraphAnalysis<'bump> {
    pub entry: Block,

    pub static_branches: BFnvHashMap<'bump, Block, Value>,
    pub static_branches_blocks: BFnvHashMap<'bump, Block, Block>,

    pub chains: BFnvHashMap<'bump, Block, Chain<'bump>>,

    pub phis: BFnvHashMap<'bump, Value, CondValue<'bump>>,
    pub static_renames: BFnvHashMap<'bump, Value, Value>,
}

#[derive(Debug)]
pub struct Chain<'bump> {
    pub entry_edges: BFnvHashMap<'bump, Block, ()>,
    pub edges: BFnvHashMap<'bump, BlockEdge, ()>,
    pub blocks: BFnvHashMap<'bump, Block, ()>,
}

impl<'bump> GraphAnalysis<'bump> {

    fn is_value_relevant(&self, chain_target: Block, value: Value) -> bool {
        if let Some(cond) = self.phis.get(&value) {
            let chain = &self.chains[&chain_target];

            // Check if it's instead relevant to another chain
            if !chain.blocks.contains_key(&cond.block) && cond.block != chain_target {
                false
            } else {
                true
            }
        } else {
            false
        }
    }

}

#[derive(Debug)]
pub struct ChainAnalysis<'bump> {
    pub target: Block,

    pub blocks: BFnvHashMap<'bump, Block, ()>,
    pub edges: BFnvHashMap<'bump, BlockEdge, ()>,
    pub entry_edges: BFnvHashMap<'bump, Block, ()>,

    //pub orig_args: BTreeSet<Value>,
    pub args: BFnvHashMap<'bump, Value, ()>,

    pub cond_map: BFnvHashMap<'bump, Value, CondValue<'bump>>,
    pub static_map: BFnvHashMap<'bump, Value, Value>,

    pub renames_required: bool,
}

#[derive(Debug)]
pub struct CondValue<'bump> {
    pub value: Value,
    pub value_num: Option<usize>,
    pub block: Block,
    pub sources: BFnvHashMap<'bump, Block, PhiSource>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum PhiSource {
    /// The phi source is a value from the scope, not partial
    /// of the chain.
    Scope(Value),

    /// The phi source is renamed to an argument of a block that
    /// is part of the chain.
    EntryArg {
        /// The block that is called as the entry edge.
        /// This MUST be part of the chain.
        called: Block,
        /// The argument value of the above index
        arg: Value,
    },
}
impl PhiSource {
    pub fn is_scope(&self) -> bool {
        match self {
            PhiSource::Scope(_) => true,
            _ => false,
        }
    }
    pub fn scope(&self) -> Option<Value> {
        match self {
            PhiSource::Scope(val) => Some(*val),
            _ => None,
        }
    }
    pub fn value(&self) -> Value {
        match self {
            PhiSource::Scope(val) => *val,
            PhiSource::EntryArg { arg, .. } => *arg,
        }
    }
}

#[derive(Debug)]
pub struct EntryEdgeAnalysis<'bump> {
    //pub caller: Block,
    pub callee: Block,

    // The mappings that are specific for this entry edge.
    // Should not be inserted into the global map, the args should be
    // `map_value`d through this map.
    pub mappings: BFnvHashMap<'bump, Value, PhiSource>,

    // The arguments that should be used when calling the target block.
    // These need to be called with `map_value` in case the value is a primop.
    pub args: BVec<'bump, PhiSource>,

    //pub looping_vars: BTreeSet<Value>,
}

struct AnalysisContext<'bump, 'a> {
    bump: &'bump Bump,

    pub fun: &'a Function,
    pub graph: &'a LiveBlockGraph<'a>,

    current: Option<Block>,

    static_renames: &'a mut BFnvHashMap<'bump, Value, Value>,
    static_branches: &'a mut BFnvHashMap<'bump, Block, Value>,
    phis: &'a mut BFnvHashMap<'bump, Value, CondValue<'bump>>,
}
impl<'bump, 'a> AnalysisContext<'bump, 'a> {

    fn init_block(&mut self, block: Block) {
        self.current = Some(block);
    }

    /// Follows the provided value according to the current static mappings
    pub fn follow(&self, mut val: Value) -> Value {
        while let Some(next) = self.static_renames.get(&val) {
            val = *next;
        }
        val
    }

    /// The current block statically branches to the target value
    pub fn set_branch(&mut self, target: Value) {
        self.static_branches.insert(self.current.unwrap(), target);
    }

    pub fn add_rename(
        &mut self,
        callee: Block,
        caller_read: Value, callee_arg_num: usize,
    ) {
        let caller = self.current.unwrap();
        let callee_arg = self.fun.block_args(callee)[callee_arg_num];

        #[cfg(debug)]
        {
            if let Some(p) = self.static_branches.get(caller) {
                assert!(self.fun.value_block(*p) == Some(callee));
            } else {
                panic!();
            }
        }

        let incoming_count = self.graph.incoming(callee).count();
        let incoming = self.graph.incoming(callee).next();
        assert!(incoming_count > 0);
        if incoming_count == 1 && incoming.unwrap() == caller {
            self.static_renames.insert(callee_arg, caller_read);
        }

        if !self.phis.contains_key(&callee_arg) {
            let cond = CondValue {
                block: callee,
                value: callee_arg,
                value_num: Some(callee_arg_num),
                sources: BFnvHashMap::with_hasher_in(self.bump, Default::default()),
            };
            self.phis.insert(callee_arg, cond);
        }
        self.phis
            .get_mut(&callee_arg).unwrap()
            .sources.insert(caller, PhiSource::Scope(caller_read));
    }

}

pub fn analyze_graph<'bump, 'fun>(
    bump: &'bump Bump,
    fun: &'fun Function,
    graph: &'fun LiveBlockGraph,
) -> GraphAnalysis<'bump>
{
    let entry = fun.block_entry();

    let mut static_branches = BFnvHashMap::with_hasher_in(bump, Default::default());
    let mut phis = BFnvHashMap::with_hasher_in(bump, Default::default());
    let mut static_renames = BFnvHashMap::with_hasher_in(bump, Default::default());

    // Find all blocks we care about, these are the ones we iterate over
    let mut relevant_blocks = BTreeSet::new();
    for block in graph.dfs_iter() {
        match fun.block_kind(block).unwrap() {
            OpKind::Call(CallKind::ControlFlow) => {
                relevant_blocks.insert(block);
            }
            OpKind::IfBool => {
                relevant_blocks.insert(block);
            }
            OpKind::UnpackValueList(_) => {
                relevant_blocks.insert(block);
            }
            _ => (),
        }
    }

    let mut ctx = AnalysisContext {
        bump,

        fun,
        graph,

        current: None,

        static_renames: &mut static_renames,
        static_branches: &mut static_branches,
        phis: &mut phis,
    };

    // Propagate value renames until we reach an equilibrium.
    // This will perform branch elimination and constant propagation
    // in one single step.
    let mut handled_blocks = BTreeSet::new();
    loop {
        let mut changed = false;

        for block in relevant_blocks.iter() {
            // If the block is already statically resolved, we can skip it.
            if handled_blocks.contains(block) { continue; }

            ctx.init_block(*block);

            let res = match fun.block_kind(*block).unwrap() {
                OpKind::Call(CallKind::ControlFlow) =>
                    self::call::propagate(&mut ctx, *block),
                OpKind::IfBool =>
                    self::if_bool::propagate(&mut ctx, *block),
                OpKind::UnpackValueList(n) =>
                    self::unpack_value_list::propagate(&mut ctx, *block, *n),
                _ => unreachable!(),
            };

            // If the propagation implementation indicated that it has been
            // resolved, then we have a change in this iteration.
            if res {
                changed = true;
                handled_blocks.insert(*block);
            }
        }

        // If this iteration contains no changes, then we are done.
        if !changed { break; }
    }

    // Populate incoming edges in phis
    for phi in ctx.phis.values_mut() {
        for incoming in graph.incoming(phi.block) {
            if !phi.sources.contains_key(&incoming) {
                phi.sources.insert(incoming, PhiSource::EntryArg {
                    arg: phi.value,
                    called: phi.block,
                });
            }
        }
    }

    // Generate `static_branches_blocks`
    let mut static_branches_blocks = BFnvHashMap::with_hasher_in(
        bump, Default::default());
    for (from, to) in static_branches.iter() {
        if let Some(to_block) = fun.value_block(*to) {
            static_branches_blocks.insert(*from, to_block);
        }
    }

    // Group chains
    // TODO: Handle cycles
    let mut chains = BFnvHashMap::with_hasher_in(
        bump, Default::default());
    for (from, to) in static_branches.iter() {
        let mut next_target_value = *to;
        let mut target_block = *from;

        let mut to_has_block = None;

        // Walk chain of branches until target is reached
        loop {

            // If the next value is a block
            if let Some(to_block) = fun.value_block(next_target_value) {
                // .. the target is that block
                target_block = to_block;

                if to_has_block.is_none() {
                    to_has_block = Some(to_block);
                }

                // If that block has an outgoing branch,
                // walk that next and continue
                if let Some(next) = static_branches.get(&to_block) {
                    next_target_value = *next;
                    continue;
                }
            }

            // If the chains map doesn't contain the target, seed it
            if !chains.contains_key(&target_block) {
                let mut chain = Chain {
                    edges: BFnvHashMap::with_hasher_in(bump, Default::default()),
                    blocks: BFnvHashMap::with_hasher_in(bump, Default::default()),
                    entry_edges: BFnvHashMap::with_hasher_in(bump, Default::default()),
                };
                chain.blocks.insert(target_block, ());
                chains.insert(target_block, chain);
            }

            // Update the current chain with this edge
            let chain = chains.get_mut(&target_block).unwrap();
            chain.blocks.insert(*from, ());
            if let Some(to_block) = to_has_block {
                chain.edges.insert((*from, to_block), ());
            }

            break;
        }
    }

    // Find incoming edges into the chain
    for chain in chains.values_mut() {
        for block in chain.blocks.keys() {
            if *block == entry {
                chain.entry_edges.insert(*block, ());
            }
            for incoming in graph.incoming(*block) {
                let edge = (incoming, *block);
                if chain.edges.contains_key(&edge) {
                    continue;
                }
                chain.entry_edges.insert(*block, ());
            }
        }
    }

    GraphAnalysis {
        entry,
        static_branches,
        static_branches_blocks,
        phis,
        static_renames,
        chains,
    }
}

fn expand_phi<'bump>(
    bump: &'bump Bump,
    phi_entry: Value,
    target: Block,
    graph: &LiveBlockGraph,
    analysis: &GraphAnalysis
) -> BFnvHashMap<'bump, Block, PhiSource>
{

    // The root phis form the initial set we want to walk down from
    let mut to_walk = Vec::new();
    to_walk.push(phi_entry);

    let mut added = BFnvHashMap::with_hasher_in(bump, Default::default());
    let mut entries: BFnvHashMap<Block, PhiSource> =
        BFnvHashMap::with_hasher_in(bump, Default::default());

    // While we have values to walk, get the next one.
    // Then add the mappings to the entries and walk downwards.
    while let Some(v) = to_walk.pop() {
        if added.contains_key(&v) { continue; }
        added.insert(v, ());

        if let Some(cond_value) = analysis.phis.get(&v) {
            for (_, v) in cond_value.sources.iter() {
                if let PhiSource::Scope(value) = v {
                    to_walk.push(*value);
                }
            }
            entries.extend(
                cond_value.sources.iter()
                    .map(|(k, v)| (*k, *v))
            );
        }
    }

    entries
}

pub fn analyze_chain<'bump>(
    bump: &'bump Bump,
    target: Block,
    fun: &Function,
    graph: &LiveBlockGraph,
    live: &LiveValues,
    analysis: &'bump GraphAnalysis,
) -> ChainAnalysis<'bump> {
    let chain = &analysis.chains[&target];

    // Get the live values in the body of the block.
    let mut primary_conds = BTreeSet::new();
    let target_live = live.live_in(target);
    primary_conds.extend(target_live.iter());

    // These are value mappings for the chain that don't depend on any control
    // flow in the chain.
    let mut static_map: BFnvHashMap<Value, Value> =
        BFnvHashMap::with_hasher_in(bump, Default::default());

    // These are value mappings that do depend on control flow in the chain.
    // The values in here refer to a phi node in `self.phis`.
    let mut cond_map = BFnvHashMap::with_hasher_in(bump, Default::default());

    // The arguments that need to placed to the new target block.
    let mut args = BFnvHashMap::with_hasher_in(bump, Default::default());

    // Using the live values in the target block as a seed, walk through
    // and combine phis. All values referenced in the target that have
    // phis for it, need to be added as arguments to the new target block
    // we generate.
    // For primops, we need to recurse down and handle the reads of the
    // PHI as well. Whenever we cross the phi boundary, the args from that
    // primop no longer should be added to the args set, since that primop
    // needs to be placed before the join edge in the generated IR.
    let mut to_visit: VecDeque<_> = primary_conds.iter().map(|v| (true, *v)).collect();
    let mut visited = BTreeSet::new();
    while let Some((as_arg, read)) = to_visit.pop_front() {

        // If the value is already visited, skip
        if visited.contains(&read) { continue; }
        visited.insert(read);

        let block;
        let mut arg_num = None;
        match fun.value_kind(read) {
            // If the value is an argument, it might be a phi
            ValueKind::Argument(arg_block, arg_num_n) => {
                block = arg_block;
                arg_num = Some(arg_num_n);
            }
            // If the value is a primop, we need to visit its reads
            ValueKind::PrimOp(prim) => {
                let needs_as_arg = as_arg &&
                    fun.primop_reads(prim).iter()
                    .any(|v| analysis.is_value_relevant(target, *v));

                for read in fun.primop_reads(prim) {
                    to_visit.push_back((needs_as_arg, *read));
                }
                continue;
            }
            // Other alternatives are Const and Block, none of which can contain
            // values we care about.
            _ => continue,
        }

        // If the value is not relevant to the chain, we skip it
        if !analysis.is_value_relevant(target, read) { continue; }

        // Expand phi chains
        let entries = expand_phi(bump, read, target, graph, analysis);

        let first_value = entries.iter().next().unwrap().1;
        if entries.iter().all(|(_, v)| *v == *first_value) && first_value.is_scope() {
            let first_value_scope = first_value.scope().unwrap();

            // If the mappings for all phi sources are identical, this is a universal
            // mapping.
            static_map.insert(read, first_value_scope);

            // If the phi targets a primop, it might reference other phis.
            // We need to walk its destinations.
            // We might need the subvalues as arguments if we have a static
            // mapping since we would like to insert any potential primop
            // in the target block in this case.
            to_visit.push_back((as_arg, first_value_scope));
        } else {
            // If the phi targets a primop, it might reference other phis.
            // We need to walk its destinations.
            // We don't need the subvalues as args since the primop needs to be in
            // the callee block regardless.
            for (_, v) in entries.iter() {
                if let PhiSource::Scope(iv) = v {
                    to_visit.push_back((false, *iv));
                }
            }

            // Else, this is a conditional mapping.
            // This should be mapped to an argument on the new target block.
            cond_map.insert(read, CondValue {
                value: read,
                value_num: arg_num,
                block: block,
                sources: entries,
            });

            if as_arg {
                // The value needs to be added as an argument
                args.insert(read, ());
            }
        }

    }

    let mut renames_required = false;
    // If number of target block arguments is not above 0,
    // then no renames CAN be required.
    if args.len() > 0 {
        // Walk each of the reads in the target, if the value is a block...
        let _ = fun.block_walk_nested_values::<_, ()>(target, &mut |val| {
            let mapped = static_map.get(&val).cloned().unwrap_or(val);
            if let Some(block) = fun.value_block(mapped) {
                // .. if any one of the values in the cond map are live
                // in that map, we need to respect renames.
                for value in cond_map.keys() {
                    if live.is_live_at(block, *value) {
                        renames_required = true;
                        return Err(());
                    }
                }
            }
            Ok(())
        });
    }

    ChainAnalysis {
        target,

        blocks: chain.blocks.clone(),
        entry_edges: chain.entry_edges.clone(),
        edges: chain.edges.clone(),

        args,
        cond_map,
        static_map,
        // TODO:
        // Whether any blocks in the scope of the target block requires
        // the renames to be present.
        // If this is false, it is possible to generate better control
        // flow.
        // Loop through every read in the target, if the read value is a
        // block, check that none of the live values in that block are
        // in the cond_map.
        renames_required,
    }
}

pub fn analyze_entry_edge<'bump>(
    bump: &'bump Bump,
    analysis: &GraphAnalysis,
    chain_analysis: &ChainAnalysis,
    callee: Block,
) -> EntryEdgeAnalysis<'bump> {

    let mut o_map: BFnvHashMap<Value, PhiSource> =
        BFnvHashMap::with_hasher_in(bump, Default::default());
    o_map.extend(
        chain_analysis.cond_map.values()
            .filter_map(|cond_value| {
                let mut curr = callee;
                assert!(!chain_analysis.static_map.contains_key(&cond_value.value));

                loop {
                    if let Some(source) = cond_value.sources.get(&curr) {
                        break Some((cond_value.value, *source));
                    }

                    if let Some(next) = analysis.static_branches_blocks.get(&curr) {
                        // Follow the chain of static branches
                        curr = *next;
                    } else {
                        // If we reached the end of the chain without finding the phi,
                        // then this is only relevant for another subbranch.
                        break None;
                    }
                }
            })
    );

    // Specialize each of the phis for the current entry edge
    let mut map: BFnvHashMap<Value, PhiSource> =
        BFnvHashMap::with_hasher_in(bump, Default::default());
    map.extend(
        chain_analysis.cond_map.values()
            .filter_map(|cond_value| {
                let mut curr = callee;
                assert!(!chain_analysis.static_map.contains_key(&cond_value.value));

                loop {
                    if let Some(source) = cond_value.sources.get(&curr) {
                        break Some((cond_value.value, *source));
                    }

                    if let Some(next) = analysis.static_branches_blocks.get(&curr) {
                        // Follow the chain of static branches
                        curr = *next;
                    } else {
                        // If we reached the end of the chain without finding the phi,
                        // then this is only relevant for another subbranch.
                        break None;
                    }
                }
            })
    );

    // Produce the argument list the target block should be called with.
    let mut calling_args = BVec::new_in(bump);
    calling_args.extend(
        chain_analysis.args.keys()
            .map(|v| {
                o_map.get(v).cloned().unwrap_or(PhiSource::Scope(*v))
            })
    );

    EntryEdgeAnalysis {
        callee,

        mappings: map,
        args: calling_args,
    }
}
