use std::collections::{BTreeMap, BTreeSet, VecDeque};

use libeir_ir::{Block, Value, OpKind, ValueKind, PrimOpKind};
use libeir_ir::{Function, LiveBlockGraph, LiveValues};

mod call;
mod if_bool;
mod unpack_value_list;

#[derive(Debug)]
pub struct GraphAnalysis {
    pub entry: Block,

    pub static_branches: BTreeMap<Block, Value>,
    pub static_branches_blocks: BTreeMap<Block, Block>,

    pub chains: BTreeMap<Block, BTreeSet<Block>>,
    pub phis: BTreeMap<Value, (Block, BTreeMap<Block, Value>)>,
    pub static_renames: BTreeMap<Value, Value>,
}

impl GraphAnalysis {

    fn is_value_relevant(&self, chain: Block, value: Value) -> bool {
        if let Some((block, _)) = self.phis.get(&value) {
            let chain_blocks = &self.chains[&chain];

            // Check if it's instead relevant to another chain
            if !chain_blocks.contains(block) && *block != chain {
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
pub struct ChainAnalysis {
    pub target: Block,
    pub blocks: BTreeSet<Block>,

    // None represents function entry
    pub entry_edges: Vec<(Option<Block>, Block)>,

    pub orig_args: BTreeSet<Value>,
    pub args: BTreeSet<Value>,

    pub cond_map: BTreeMap<Value, (Block, BTreeMap<Block, Value>)>,
    pub static_map: BTreeMap<Value, Value>,

    pub renames_required: bool,
}

#[derive(Debug)]
pub struct EntryEdgeAnalysis {
    //pub caller: Block,
    pub callee: Block,

    // The mappings that are specific for this entry edge.
    // Should not be inserted into the global map, the args should be
    // `map_value`d through this map.
    pub mappings: BTreeMap<Value, Value>,

    // The arguments that should be used when calling the target block.
    // These need to be called with `map_value` in case the value is a primop.
    pub args: Vec<Value>,
}

fn follow(renames: &BTreeMap<Value, Value>, mut val: Value) -> Value {
    while let Some(next) = renames.get(&val) {
        val = *next;
    }
    val
}

struct AnalysisContext<'a> {
    pub fun: &'a Function,
    pub graph: &'a LiveBlockGraph<'a>,

    current: Option<Block>,

    static_renames: &'a mut BTreeMap<Value, Value>,
    static_branches: &'a mut BTreeMap<Block, Value>,
    phis: &'a mut BTreeMap<Value, (Block, BTreeMap<Block, Value>)>,
}
impl<'a> AnalysisContext<'a> {

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
        caller_read: Value, callee_arg: Value,
    ) {
        let caller = self.current.unwrap();

        #[cfg(debug)]
        {
            if let Some(p) = self.static_branches.get(caller) {
                assert!(self.fun.value_block(*p) == Some(callee));
            } else {
                panic!();
            }
        }

        let incoming_count = self.graph.incoming(callee).count();
        assert!(incoming_count > 0);
        if incoming_count == 1 {
            assert!(self.graph.incoming(callee).next() == Some(caller));
            self.static_renames.insert(callee_arg, caller_read);
        }

        if !self.phis.contains_key(&callee_arg) {
            self.phis.insert(callee_arg, (callee, BTreeMap::new()));
        }
        self.phis.get_mut(&callee_arg).unwrap().1.insert(caller, caller_read);
    }

}

pub fn analyze_graph(
    fun: &Function,
    graph: &LiveBlockGraph,
) -> GraphAnalysis
{
    let entry = fun.block_entry();

    let mut static_branches = BTreeMap::new();
    let mut phis = BTreeMap::new();
    let mut static_renames = BTreeMap::new();

    // Find all blocks we care about, these are the ones we iterate over
    let mut relevant_blocks = BTreeSet::new();
    for block in graph.dfs_iter() {
        match fun.block_kind(block).unwrap() {
            OpKind::Call => {
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
                OpKind::Call =>
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

    // Generate `static_branches_blocks`
    let mut static_branches_blocks = BTreeMap::new();
    for (from, to) in static_branches.iter() {
        if let Some(to_block) = fun.value_block(*to) {
            static_branches_blocks.insert(*from, to_block);
        }
    }

    // Group chains
    // TODO: Handle cycles
    let mut chains = BTreeMap::new();
    for (from, to) in static_branches.iter() {
        let mut to = *to;
        let mut to_b = *from;
        loop {
            if let Some(to_block) = fun.value_block(to) {
                to_b = to_block;
                if let Some(to_next) = static_branches.get(&to_block) {
                    to = *to_next;
                    continue;
                }
            }

            if !chains.contains_key(&to_b) {
                chains.insert(to_b, BTreeSet::new());
            }
            chains.get_mut(&to_b).unwrap().insert(*from);
            break;
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

fn expand_phi(phi_entry: Value, target: Block, analysis: &GraphAnalysis) -> BTreeMap<Block, Value> {

    // The root phis form the initial set we want to walk down from
    let mut to_walk = Vec::new();
    to_walk.push(phi_entry);

    let mut added = BTreeSet::new();
    let mut entries = BTreeMap::new();//analysis.phis[&phi_entry].1.clone();

    // While we have values to walk, get the next one.
    // Then add the mappings to the entries and walk downwards.
    while let Some(v) = to_walk.pop() {
        if added.contains(&v) { continue; }
        added.insert(v);

        if let Some((_block, new_entries)) = analysis.phis.get(&v) {
            for (_, v) in new_entries.iter() {
                to_walk.push(*v);
            }
            entries.extend(new_entries.iter());
        }
    }

    entries
}

pub fn analyze_chain(
    target: Block,
    fun: &Function,
    graph: &LiveBlockGraph,
    live: &LiveValues,
    analysis: &GraphAnalysis,
) -> ChainAnalysis {
    let chain_blocks = &analysis.chains[&target];

    // Find the entry points into the chain
    let mut entry_edges = BTreeSet::new();
    if chain_blocks.contains(&analysis.entry) {
        entry_edges.insert((None, analysis.entry));
    }
    for chain_block in chain_blocks.iter() {
        for incoming_block in graph.incoming(*chain_block) {
            if chain_blocks.contains(&incoming_block) { continue; }
            entry_edges.insert((Some(incoming_block), *chain_block));
        }
    }

    let target_live = &live.live_in[&target];

    // Get the live values in the body of the block.
    let mut primary_conds = BTreeSet::new();
    primary_conds.extend(target_live.iter(&live.pool));

    // These are value mappings for the chain that don't depend on any control
    // flow in the chain.
    let mut static_map: BTreeMap<Value, Value> = BTreeMap::new();

    // These are value mappings that do depend on control flow in the chain.
    // The values in here refer to a phi node in `self.phis`.
    let mut cond_map = BTreeMap::new();

    // The arguments that need to placed to the new target block.
    let mut args = BTreeSet::new();

    let orig_args = fun.block_args(target).iter().cloned().collect();

    let mut to_visit: VecDeque<_> = primary_conds.iter().map(|v| (true, *v)).collect();
    let mut visited = BTreeSet::new();
    while let Some((as_arg, read)) = to_visit.pop_front() {

        // If the value is already visited, skip
        if visited.contains(&read) { continue; }
        visited.insert(read);

        let block;
        match fun.value_kind(read) {
            // If the value is an argument, it might be a phi
            ValueKind::Argument(arg_block, _) => block = arg_block,
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
            // Else, skip
            _ => continue,
        }

        // If the value is not relevant to the chain, we skip it
        if !analysis.is_value_relevant(target, read) { continue; }

        // Expand phi chains
        let entries = expand_phi(read, target, analysis);

        let first_value = entries.iter().next().unwrap().1;
        if entries.iter().all(|(_, v)| *v == *first_value) {
            // If the mappings for all phi sources are identical, this is a universal
            // mapping.
            static_map.insert(read, *first_value);

            // If the phi targets a primop, it might reference other phis.
            // We need to walk its destinations.
            // We might need the subvalues as arguments if we have a static
            // mapping since we would like to insert any potential primop
            // in the target block in this case.
            to_visit.push_back((as_arg, *first_value));
        } else {
            // If the phi targets a primop, it might reference other phis.
            // We need to walk its destinations.
            // We don't need the subvalues as args since the primop needs to be in
            // the callee block regardless.
            for (_, v) in entries.iter() {
                to_visit.push_back((false, *v));
            }

            // Else, this is a conditional mapping.
            // This should be mapped to an argument on the new target block.
            cond_map.insert(read, (block, entries));

            if as_arg {
                // The value needs to be added as an argument
                args.insert(read);
            }
        }

    }

    let mut renames_required = false;
    // If number of target block arguments is not above 0,
    // then no renames CAN be required.
    if args.len() > 0 {
        fun.block_walk_nested_values::<_, Result<(), ()>>(target, &mut |val| {
            if fun.value_block(val).is_some() {
                // TODO
                renames_required = true;
            }
            Ok(())
        }).unwrap();
    }

    ChainAnalysis {
        target,
        blocks: chain_blocks.clone(),
        entry_edges: entry_edges.iter().cloned().collect(),
        args,
        orig_args,
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

pub fn analyze_entry_edge(
    analysis: &GraphAnalysis,
    chain_analysis: &ChainAnalysis,
    num: usize,
) -> EntryEdgeAnalysis {
    let (caller, callee) = chain_analysis.entry_edges[num];

    // Specialize each of the phis for the current entry edge
    let map: BTreeMap<Value, Value> = chain_analysis.cond_map.iter()
        .filter_map(|(value, (val_block, phis))| {
            let mut curr = callee;

            if caller == Some(chain_analysis.target) {
                if *val_block == caller.unwrap() {
                    println!("Bail!");
                    return None;
                }
            }

            loop {
                if let Some(i) = phis.get(&curr) {
                    break Some((*value, *i));
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
        }).collect();

    // Produce the argument list the target block should be called with.
    let calling_args: Vec<_> = chain_analysis.args.iter()
        .map(|v| {
            map.get(v).cloned().unwrap_or(*v)
        }).collect();

    EntryEdgeAnalysis {
        //caller,
        callee,

        mappings: map,
        args: calling_args,
    }
}
