use std::collections::{BTreeMap, BTreeSet, VecDeque};

use bumpalo::{collections::Vec as BVec, Bump};

use libeir_util_datastructures::pooled_entity_set::{EntitySet, EntitySetPool};

use super::BFnvHashMap;

use libeir_ir::{Block, CallKind, OpKind, Value, ValueKind};
use libeir_ir::{Function, LiveBlockGraph, LiveValues};

use log::trace;

use super::chain_graph::{Chain, Node};

mod call;
mod if_bool;
mod unpack_value_list;

type BlockEdge = (Block, Block);

#[derive(Debug)]
pub struct GraphAnalysis<'bump> {
    pub entry: Block,

    pub static_branches: BFnvHashMap<'bump, Block, Value>,
    pub static_branches_blocks: BFnvHashMap<'bump, Block, Block>,

    pub trees: BFnvHashMap<'bump, Block, TreeData<'bump>>,

    pub phis: BFnvHashMap<'bump, Value, CondValue<'bump>>,
    pub static_renames: BFnvHashMap<'bump, Value, Value>,
}

#[derive(Debug)]
pub struct TreeData<'bump> {
    pub entry_edges: BFnvHashMap<'bump, Block, ()>,
    pub edges: BFnvHashMap<'bump, BlockEdge, ()>,
    pub blocks: BFnvHashMap<'bump, Block, ()>,
    pub entry_values: BFnvHashMap<'bump, Value, (Block, usize)>,
}

impl<'bump> GraphAnalysis<'bump> {
    fn is_before(&self, lhs: Block, rhs: Block) -> bool {
        let mut curr = lhs;

        loop {
            if let Some(prev) = self.static_branches_blocks.get(&curr) {
                curr = *prev;
            } else {
                return false;
            }

            if curr == rhs {
                return true;
            }
        }
    }

    fn is_value_relevant(&self, tree_target: Block, value: Value) -> bool {
        if let Some(cond) = self.phis.get(&value) {
            let tree = &self.trees[&tree_target];

            // Check if it's instead relevant to another tree
            if !tree.blocks.contains_key(&cond.block) && cond.block != tree_target {
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
    pub value_index: usize,
    pub block: Block,
    pub sources: BFnvHashMap<'bump, Block, PhiSource>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct PhiSource {
    /// The value on the other side of the phi.
    /// If this is None, then this is an anonymous entry edge.
    value: Option<Value>,
    /// The block calling the phi.
    /// If this is None, then this is an anonymous entry edge.
    caller: Option<Block>,
    /// The block that is called as the entry edge.
    /// This MUST be part of the chain.
    called: Block,
    /// The index of the argment on the called block.
    arg_index: usize,
    /// The argument value itself.
    arg: Value,
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

    pub fn add_rename(&mut self, callee: Block, caller_read: Value, callee_arg_num: usize) {
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
                value_index: callee_arg_num,
                sources: BFnvHashMap::with_hasher_in(Default::default(), self.bump),
            };
            self.phis.insert(callee_arg, cond);
        }

        let phi_source = PhiSource {
            value: Some(caller_read),
            caller: Some(caller),
            called: callee,
            arg_index: callee_arg_num,
            arg: callee_arg,
        };
        self.phis
            .get_mut(&callee_arg)
            .unwrap()
            .sources
            .insert(caller, phi_source);
    }
}

pub fn analyze_graph<'bump, 'fun>(
    bump: &'bump Bump,
    fun: &'fun Function,
    graph: &'fun LiveBlockGraph,
) -> GraphAnalysis<'bump> {
    let entry = fun.block_entry();

    let mut static_branches = BFnvHashMap::with_hasher_in(Default::default(), bump);
    let mut phis = BFnvHashMap::with_hasher_in(Default::default(), bump);
    let mut static_renames = BFnvHashMap::with_hasher_in(Default::default(), bump);

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
            if handled_blocks.contains(block) {
                continue;
            }

            ctx.init_block(*block);

            let res = match fun.block_kind(*block).unwrap() {
                OpKind::Call(CallKind::ControlFlow) => self::call::propagate(&mut ctx, *block),
                OpKind::IfBool => self::if_bool::propagate(&mut ctx, *block),
                OpKind::UnpackValueList(n) => {
                    self::unpack_value_list::propagate(&mut ctx, *block, *n)
                }
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
        if !changed {
            break;
        }
    }

    trace!("======== Results after initial traversal:");
    trace!("Phis: {:?}", ctx.phis);
    trace!("Static renames: {:?}", ctx.static_renames);
    trace!("========");

    // Populate incoming edges in phis
    for phi in ctx.phis.values_mut() {
        for incoming in graph.incoming(phi.block) {
            if !phi.sources.contains_key(&incoming) {
                phi.sources.insert(
                    incoming,
                    PhiSource {
                        // Not relevant since the edge is not part of the tree
                        value: None,
                        caller: None,

                        called: phi.block,
                        arg_index: phi.value_index,
                        arg: phi.value,
                    },
                );
            }
        }
    }

    // Generate `static_branches_blocks`
    let mut static_branches_blocks = BFnvHashMap::with_hasher_in(Default::default(), bump);
    for (from, to) in static_branches.iter() {
        if let Some(to_block) = fun.value_block(*to) {
            static_branches_blocks.insert(*from, to_block);
        }
    }

    // Group chains
    // TODO: Handle cycles
    let mut trees = BFnvHashMap::with_hasher_in(Default::default(), bump);
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
            if !trees.contains_key(&target_block) {
                let mut tree = TreeData {
                    edges: BFnvHashMap::with_hasher_in(Default::default(), bump),
                    blocks: BFnvHashMap::with_hasher_in(Default::default(), bump),
                    entry_edges: BFnvHashMap::with_hasher_in(Default::default(), bump),
                    entry_values: BFnvHashMap::with_hasher_in(Default::default(), bump),
                };
                tree.blocks.insert(target_block, ());
                trees.insert(target_block, tree);
            }

            // Update the current chain with this edge
            let tree = trees.get_mut(&target_block).unwrap();
            tree.blocks.insert(*from, ());
            if let Some(to_block) = to_has_block {
                tree.edges.insert((*from, to_block), ());
            }

            break;
        }
    }

    // Find incoming edges into the chain
    for tree in trees.values_mut() {
        for block in tree.blocks.keys() {
            if *block == entry {
                tree.entry_edges.insert(*block, ());
                for (arg_idx, arg) in fun.block_args(*block).iter().enumerate() {
                    tree.entry_values.insert(*arg, (*block, arg_idx));
                }
            }
            for incoming in graph.incoming(*block) {
                let edge = (incoming, *block);
                if tree.edges.contains_key(&edge) {
                    continue;
                }

                tree.entry_edges.insert(*block, ());
                for (arg_idx, arg) in fun.block_args(*block).iter().enumerate() {
                    tree.entry_values.insert(*arg, (*block, arg_idx));
                }
            }
        }
    }

    let analysis = GraphAnalysis {
        entry,
        static_branches,
        static_branches_blocks,
        phis,
        static_renames,
        trees,
    };

    trace!("GRAPH ANALYSIS: {:?}", analysis);

    analysis
}

pub struct ChainMapping {
    entry_to_chain: BTreeMap<Block, Chain>,
    block_to_entries: BTreeMap<Block, BTreeSet<Block>>,
}
impl ChainMapping {
    pub fn iter_entries_for<'a>(&'a self, block: Block) -> impl Iterator<Item = Block> + 'a {
        self.block_to_entries[&block].iter().cloned()
    }
    pub fn entry_to_chain(&self, entry: Block) -> Chain {
        self.entry_to_chain[&entry]
    }
}

pub fn build_node<'bump>(
    bump: &'bump Bump,
    fun: &Function,
    //graph: &LiveBlockGraph,
    live: &LiveValues,
    analysis: &'bump GraphAnalysis,
    target: Block,
    block_graph: &ChainMapping,
    last_loc: Block,
    value: Value,
    chain_graph: &mut super::chain_graph::ChainGraph,
    existing_nodes: &mut BTreeMap<Value, Node>,
) -> (bool, Node) {
    trace!("{:?} {:?}", value, fun.value_kind(value));

    let tree = &analysis.trees[&target];

    match fun.value_kind(value) {
        ValueKind::Argument(block, arg_index) => {
            trace!("ARGUMENT {} {}", block, arg_index);

            trace!("A1");

            // If it's not an argument of a block in the chain, we simply refer
            // to the scope.
            if !tree.blocks.contains_key(&block) {
                return (false, chain_graph.insert_scoped(value));
            }

            if !(block == last_loc || analysis.is_before(block, last_loc)) {
                return (false, chain_graph.insert_scoped(value));
            }

            trace!("A2 last_loc: {}", last_loc);

            // Look up existing node
            if let Some(existing) = existing_nodes.get(&value) {
                return (true, *existing);
            }

            trace!("A6");

            // If the value is a phi, we build recursively
            let v_phi = chain_graph.insert_phi(0, value);

            // If there exists a phi for the value, add those to phi
            if let Some(phi) = analysis.phis.get(&value) {
                for (from_block, source) in phi.sources.iter() {
                    if let Some(from_val) = source.value {
                        let (_req, node) = build_node(
                            bump,
                            fun,
                            //graph,
                            live,
                            analysis,
                            target,
                            block_graph,
                            *from_block,
                            from_val,
                            chain_graph,
                            existing_nodes,
                        );
                        for entry in block_graph.iter_entries_for(*from_block) {
                            let chain = block_graph.entry_to_chain(entry);
                            chain_graph.phi_add_entry(v_phi, chain, node);
                        }
                    }
                }
            }

            // If current block is an entry, add that to the phi
            if tree.entry_edges.contains_key(&block) {
                let chain = block_graph.entry_to_chain(block);
                let node = chain_graph.insert_chain_entry_arg(chain, arg_index, value);
                chain_graph.phi_add_entry(v_phi, chain, node);
            }

            existing_nodes.insert(value, v_phi);
            (true, v_phi)
        }
        ValueKind::Block(block) => {
            // Look up existing node
            if let Some(existing) = existing_nodes.get(&value) {
                return (true, *existing);
            }

            let env = live.live_at(block);

            let v_block = chain_graph.insert_block_capture(value, block);
            for dep_value in env.iter() {
                let (_req, node) = build_node(
                    bump,
                    fun,
                    //graph,
                    live,
                    analysis,
                    target,
                    block_graph,
                    last_loc,
                    dep_value,
                    chain_graph,
                    existing_nodes,
                );
                chain_graph.add_dep(v_block, node, dep_value);
            }

            existing_nodes.insert(value, v_block);
            (true, v_block)
        }
        ValueKind::PrimOp(prim) => {
            // Look up existing node
            if let Some(existing) = existing_nodes.get(&value) {
                return (true, *existing);
            }

            let v_prim = chain_graph.insert_prim(value);
            let mut required = false;
            for p_read in fun.primop_reads(prim) {
                let (req, dep_node) = build_node(
                    bump,
                    fun,
                    //graph,
                    live,
                    analysis,
                    target,
                    block_graph,
                    last_loc,
                    *p_read,
                    chain_graph,
                    existing_nodes,
                );
                chain_graph.add_dep(v_prim, dep_node, *p_read);
                if req {
                    required = true;
                }
            }

            existing_nodes.insert(value, v_prim);
            (required, v_prim)
        }
        ValueKind::Const(_) => (false, chain_graph.insert_scoped(value)),
    }
}

pub fn analyze_chain<'bump>(
    bump: &'bump Bump,
    target: Block,
    fun: &Function,
    graph: &LiveBlockGraph,
    live: &LiveValues,
    analysis: &'bump GraphAnalysis,
) -> super::chain_graph::ChainGraph {
    let tree = &analysis.trees[&target];

    let mut chain_graph = super::chain_graph::ChainGraph::new(target);

    // Mapping from entry `Block`s to its `Chain`.
    let mut entry_to_chain: BTreeMap<Block, Chain> = BTreeMap::new();
    // Mapping from all `Block`s to all `Chain`s each is part of.
    let mut block_to_entries: BTreeMap<Block, BTreeSet<Block>> = BTreeMap::new();

    for (entry_block, _) in tree.entry_edges.iter() {
        let mut block_chain = Vec::new();

        block_chain.push(*entry_block);

        // Walk blocks in order, adding to chain list
        let mut curr = *entry_block;
        loop {
            if let Some(next) = analysis.static_branches_blocks.get(&curr) {
                curr = *next;
                block_chain.push(curr);
                if curr == target {
                    break;
                }
            } else {
                break;
            }
        }

        let num_args = fun.block_args(*entry_block).len();

        // Create map of `Block` to all `Chain`s it's part of.
        for block in block_chain.iter() {
            block_to_entries
                .entry(*block)
                .or_insert_with(|| BTreeSet::new())
                .insert(*entry_block);
        }

        // Create chain in graph
        let chain = chain_graph.entry_chain(num_args, block_chain.clone());

        assert!(!entry_to_chain.contains_key(entry_block));
        entry_to_chain.insert(*entry_block, chain);
    }

    let chain_mapping = ChainMapping {
        entry_to_chain,
        block_to_entries,
    };

    let mut existing_nodes = BTreeMap::new();

    for root_value in live.live_in(target).iter() {
        let (required, node) = build_node(
            bump,
            fun,
            live,
            analysis,
            target,
            &chain_mapping,
            target,
            root_value,
            &mut chain_graph,
            &mut existing_nodes,
        );
        if required {
            chain_graph.mark_root(root_value, node);
        }
    }

    //chain_graph.gen_dot(&std::path::Path::new("woo_bef.png"));
    chain_graph.process();
    //chain_graph.gen_dot(&std::path::Path::new("woo.png"));

    chain_graph
}
