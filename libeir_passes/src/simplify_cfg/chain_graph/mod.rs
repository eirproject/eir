use log::trace;

use std::collections::{BTreeMap, BTreeSet};

use libeir_ir::{Value, Block, PrimOp};
use cranelift_entity::{PrimaryMap, entity_impl};
use libeir_util_datastructures::pooled_entity_set::{EntitySetPool, EntitySet};

use libeir_util_dot_graph::{GraphPrinter, DisplayNid, PrefixedNid, NodeId};

use crate::util::Walker;
use super::analyze::PhiSource;

pub mod synthesis;

/// Disambiguate between a value that originates within the tree, and one that
/// is referenced externally.
///
/// This is required because of tight loops, a member value may also be accessed
/// from the last iteration as scoped.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum ValueRef {
    Member(Value),
    Scope(Value),
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Node(u32);
entity_impl!(Node, "node");
impl NodeId for Node {
    fn make_id(&self, out: &mut String) {
        use std::fmt::Write;
        write!(out, "{}", self).unwrap();
    }
}

#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Chain(u32);
entity_impl!(Chain, "chain");
impl NodeId for Chain {
    fn make_id(&self, out: &mut String) {
        use std::fmt::Write;
        write!(out, "{}", self).unwrap();
    }
}

/// Graph representing value uses in a single static call chain.
///
/// Graph has 5 node types:
/// * Root value. These are externally referencable nodes. They are always tied
///   to a single source cfg value.
///   They have a single outgoing edge and no incoming edges.
/// * Leaf value. These are nodes referencing external values in the source CFG.
///   They have N incoming edges and no outgoing edges.
/// * Phi node. Phi nodes perform selection on values from incoming blocks.
///   They have N incoming edges and M outgoing edges.
/// * PrimOp node. These construct a primop from M referenced values.
///   They have N incoming edges and M outgoing edges.
/// * BlockCapture node. These capture a block as a closure with M referenced
///   values in the environment.
///   They have B incoming edges and M outgoing edges.
///
/// The primary purpose of this graph is to do reduction of the value use graph
/// independently of the original control flow. This enables us to later
/// synthesize a new CFG that performs the same semantically while potentially
/// being more optimal.
///
/// The graph is operated on with a couple of main operations, usually in order:
/// 1. Graph construction. See public helper functions on the graph.
/// 2. Graph reduction. See `reduce_phis` function.
/// 3. CFG synthesis. Generates a new CFG from the reduced value use graph,
///    according to the strategy used. The strategy needs to decide to schedule
///    a node before or after phi selection.
///    * When scheduled after phi selection, the node will only occur once in
///      the target CFG, but the resulting CFG may be more complex.
///    * Scheduling a node before phi selection may result in radically simpler
///      target CFGs from the perspective of the chain itself, but will perform
///      specialization on the different nodes and can thus result in larger
///      CFG size overall.
pub struct ChainGraph {
    target_block: Block,

    /// Which nodes in the graph are currently active.
    /// This is mainly maintained for printing.
    active_nodes: BTreeSet<Node>,

    values: BTreeMap<Value, Node>,
    terminal_values: BTreeMap<Value, Node>,

    nodes: PrimaryMap<Node, NodeKind>,

    /// These are the roots of the chain graph, the entry edges.
    entry_edges: BTreeMap<Value, Node>,
    /// These are entry edges that has been found to map to a single terminal.
    uniform_mappings: BTreeMap<Value, Node>,

    chains: PrimaryMap<Chain, ChainData>,

    back_edges: BTreeMap<Node, Node>,

    aliases: BTreeMap<Node, Node>,
    node_pool: EntitySetPool<Node>,
}

pub struct ChainData {
    /// Only there for debug printing.
    blocks: Vec<Block>,
    /// The arguments to the chain entry block.
    /// If `Some(Node)`, this is the `ChainEntry` node with this index.
    args: Vec<Option<Node>>,
}

/// Public API
impl ChainGraph {

    pub fn new(target_block: Block) -> Self {
        ChainGraph {
            target_block,
           
            active_nodes: BTreeSet::new(),

            values: BTreeMap::new(),
            terminal_values: BTreeMap::new(),

            nodes: PrimaryMap::new(),

            entry_edges: BTreeMap::new(),

            chains: PrimaryMap::new(),

            back_edges: BTreeMap::new(),

            uniform_mappings: BTreeMap::new(),

            aliases: BTreeMap::new(),
            node_pool: EntitySetPool::new(),
        }
    }


    pub fn propagate_alias(&self, mut node: Node) -> Node {
        loop {
            if let Some(nnode) = self.aliases.get(&node) {
                node = *nnode;
            } else {
                break node;
            }
        }
    }

    pub fn get_terminal_value_node(&self, value: Value) -> Option<Node> {
        let mut node = *self.terminal_values.get(&value)?;
        Some(self.propagate_alias(node))
    }
    pub fn get_member_value_node(&self, value: Value) -> Option<Node> {
        let mut node = *self.values.get(&value)?;
        Some(self.propagate_alias(node))
    }

    pub fn get_member_or_terminal(&self, value: Value) -> Option<Node> {
        self.get_member_value_node(value)
            .or_else(|| self.get_terminal_value_node(value))
    }

    //pub fn node_is_value(&self, node: Node) -> bool {
    //    self.nodes[node].is_value()
    //}

    //pub fn mark_root(&mut self, value: Value) {
    //    // TODO change to take node?
    //    let node = self.get_value_node(value).unwrap();
    //    self.entry_edges.insert(value, node);
    //}
    pub fn mark_root(&mut self, value: Value, node: Node) {
        if let Some(old) = self.entry_edges.get(&value) {
            assert!(node == *old);
        }
        self.entry_edges.insert(value, node);
    }

    pub fn get_root(&self, value: Value) -> Option<Node> {
        self.entry_edges.get(&value).cloned()
    }
    pub fn get_uniform(&self, value: Value) -> Option<Node> {
        self.uniform_mappings.get(&value).cloned()
    }

    pub fn iter_roots<'a>(&'a self) -> impl Iterator<Item = (Value, Node)> + 'a {
        self.entry_edges.iter().map(|(k, v)| (*k, *v))
    }
    pub fn iter_uniform_mappings<'a>(&'a self) -> impl Iterator<Item = (Value, Node)> + 'a {
        self.uniform_mappings.iter().map(|(k, v)| (*k, *v))
    }

    pub fn insert_scoped(&mut self, value: Value) -> Node {
        //if let Some(node) = self.terminal_values.get(&value) {
        //    assert!(self.active_nodes.contains(&node));
        //    // TODO assert same
        //    return *node;
        //}
        let node = self.nodes.push(NodeKind::Scope(value));
        self.active_nodes.insert(node);
        self.terminal_values.insert(value, node);
        node
    }

    pub fn insert_chain_entry_arg(&mut self, chain: Chain, arg_index: usize, arg: Value) -> Node {
        //if let Some(node) = self.terminal_values.get(&arg) {
        //    assert!(self.active_nodes.contains(&node));
        //    // TODO assert same
        //    return *node;
        //}
        let entry_arg = EntryArg {
            chain,
            arg_index,
            arg,
        };
        let node = self.nodes.push(NodeKind::EntryArg(entry_arg));
        self.active_nodes.insert(node);
        self.terminal_values.insert(arg, node);

        let slot = &mut self.chains[chain].args[arg_index];
        assert!(slot.is_none());
        *slot = Some(node);

        node
    }

    pub fn insert_phi(&mut self, tier: usize, value: Value) -> Node {
        if let Some(node) = self.values.get(&value) {
            assert!(self.active_nodes.contains(node));
            assert!(self.nodes[*node].is_phi());
            return *node;
        }
        let scope_node = self.insert_scoped(value);
        let node = self.nodes.push(NodeKind::Phi(Phi {
            value: Some((value, scope_node)),
            tier: Some(tier),
            entries: BTreeMap::new(),
        }));
        self.active_nodes.insert(node);
        self.values.insert(value, node);
        node
    }

    pub fn phi_add_entry(&mut self, node: Node, chain: Chain, dep: Node) {
        match &mut self.nodes[node] {
            NodeKind::Phi(phi) => {
                phi.entries.insert(chain, dep);
            },
            _ => panic!(),
        }
    }

    pub fn insert_prim(&mut self, value: Value) -> Node {
        if let Some(node) = self.values.get(&value) {
            assert!(self.active_nodes.contains(node));
            assert!(self.nodes[*node].is_prim());
            return *node;
        }
        let node = self.nodes.push(NodeKind::Prim(Prim {
            prim: value,
            dependencies: BTreeMap::new(),
        }));
        self.active_nodes.insert(node);
        self.values.insert(value, node);
        node
    }

    pub fn insert_block_capture(&mut self, value: Value, block: Block) -> Node {
        if let Some(node) = self.values.get(&value) {
            assert!(self.active_nodes.contains(node));
            assert!(self.nodes[*node].is_block_capture());
            return *node;
        }
        let node = self.nodes.push(NodeKind::BlockCapture(BlockCapture {
            value,
            capture: block,
            dependencies: BTreeMap::new(),
        }));
        self.active_nodes.insert(node);
        self.values.insert(value, node);
        node
    }

    pub fn add_dep(&mut self, node: Node, dep: Node, value: Value) {
        match &mut self.nodes[node] {
            NodeKind::Prim(prim) => {
                prim.dependencies.insert(value, dep);
            },
            NodeKind::BlockCapture(cap) => {
                cap.dependencies.insert(value, dep);
            },
            _ => panic!(),
        }
    }

    pub fn gen_dot(&self, out: &std::path::Path) {
        let mut g = GraphPrinter::new();

        for (node, node_data) in self.nodes.iter() {
            if !self.active_nodes.contains(&node) { continue; }

            g.node(node, &format!("{}: {:#?}", node, node_data));

            match node_data {
                NodeKind::Scope(_) => {},
                NodeKind::EntryArg(_) => {},
                NodeKind::Phi(phi) => {
                    for (block, from_node) in phi.entries.iter() {
                        g.edge(node, *from_node, &format!("{:#?}", (block, from_node)));
                    }
                },
                NodeKind::Prim(prim) => {
                    for (dep_value, dep_node) in prim.dependencies.iter() {
                        g.edge(node, *dep_node, &format!("{:#?}", (dep_value, dep_node)));
                    }
                },
                NodeKind::BlockCapture(cap) => {
                    for (dep_value, dep_node) in cap.dependencies.iter() {
                        g.edge(node, *dep_node, &format!("{:#?}", (dep_value, dep_node)));
                    }
                },
            }
        }

        for (val, node) in self.entry_edges.iter() {
            let nid = PrefixedNid("entry_", DisplayNid(val));
            g.node(nid, &format!("root dep: {}", val));
            g.edge(nid, DisplayNid(node), "");
        }

        let mut added_blocks = BTreeSet::new();
        for (chain, chain_data) in self.chains.iter() {
            for block in chain_data.blocks.iter() {
                if !added_blocks.contains(block) {
                    added_blocks.insert(block);
                    g.node(DisplayNid(*block), &format!("{}", block));
                }
            }
            for (from, to) in chain_data.blocks.iter().zip(chain_data.blocks.iter().skip(1)) {
                g.edge(DisplayNid(from), DisplayNid(to), "");
            }
            g.edge(DisplayNid(chain), DisplayNid(chain_data.blocks[0]), &format!("{:?}", chain_data.args));
        }

        g.finish_run_dot(out);
    }

    pub fn chain_count(&self) -> usize {
        self.chains.len()
    }

    pub fn entry_chain(&mut self, arity: usize, blocks: Vec<Block>) -> Chain {
        self.chains.push(ChainData {
            args: vec![None; arity],
            blocks,
        })
    }

    pub fn get_chain_entry_block(&self, chain: Chain) -> Block {
        self.chains[chain].blocks[0]
    }

    pub fn node(&self, node: Node) -> &NodeKind {
        &self.nodes[node]
    }

    pub fn follow_chain(&self, mut node: Node, chain: Chain) -> Node {
        //println!("Follow {} {}", node, chain);
        loop {
            match &self.nodes[node] {
                NodeKind::Phi(phi) => {
                    node = phi.entries[&chain];
                },
                _ => break,
            }
        }
        node
    }
    pub fn follow_chain_maybe(&self, mut node: Node, chain: Chain) -> Option<Node> {
        //println!("Follow {} {}", node, chain);
        loop {
            match &self.nodes[node] {
                NodeKind::Phi(phi) => {
                    if let Some(next) = phi.entries.get(&chain) {
                        node = *next;
                    } else {
                        return None;
                    }
                },
                _ => break,
            }
        }
        Some(node)
    }

    pub fn dfs(&self, start: Node, order: &mut Vec<Node>) {
        order.clear();
        let mut walker = Walker::with(self.entry_edges.values().cloned().collect());
        while let Some(node) = walker.next() {
            let node_data = &self.nodes[node];
            for dep in node_data.dependencies() {
                walker.put(dep);
            }
            order.push(node);
        }
    }

    pub fn process(&mut self) {
        self.expand_phis();
        self.remove_uniform();
        self.reduce_phis2();

        //chain_graph.decycle();
        //chain_graph.expand_phis();
        //chain_graph.remove_uniform();
        //chain_graph.reduce_phis();
    }

}

/// Private API
impl ChainGraph {

    fn propagate_graph_aliases(&mut self) {
        let mut tmp = Vec::new();

        for node in self.entry_edges.values_mut() {
            if let Some(to) = self.aliases.get(&*node) {
                *node = *to;
            }
        }

        for node in self.nodes.values_mut() {
            match node {
                NodeKind::Scope(_) => (),
                NodeKind::EntryArg(_) => (),
                NodeKind::Phi(phi) => {
                    for entry_node in phi.entries.values_mut() {
                        if let Some(to) = self.aliases.get(&*entry_node) {
                            *entry_node = *to;
                        }
                    }
                },
                NodeKind::Prim(prim) => {
                    tmp.clear();
                    tmp.extend(prim.dependencies.iter().map(|(k, v)| (*k, *v)));
                    prim.dependencies.clear();

                    for (dep_value, dep_node) in tmp.iter() {
                        if let Some(to) = self.aliases.get(dep_node) {
                            prim.dependencies.insert(*dep_value, *to);
                        } else {
                            prim.dependencies.insert(*dep_value, *dep_node);
                        }
                    }
                },
                NodeKind::BlockCapture(bc) => {
                    tmp.clear();
                    tmp.extend(bc.dependencies.iter().map(|(k, v)| (*k, *v)));
                    bc.dependencies.clear();

                    for (dep_value, dep_node) in tmp.iter() {
                        if let Some(to) = self.aliases.get(dep_node) {
                            bc.dependencies.insert(*dep_value, *to);
                        } else {
                            bc.dependencies.insert(*dep_value, *dep_node);
                        }
                    }
                },
            }
        }
    }

    fn insert_anon_phi(&mut self) -> Node {
        let node = self.nodes.push(NodeKind::Phi(Phi {
            value: None,
            tier: None,
            entries: BTreeMap::new(),
        }));
        self.active_nodes.insert(node);
        node
    }

}

impl ChainGraph {

    pub fn remove_uniform(&mut self) {
        'outer: for (entry_value, entry_node) in self.entry_edges.iter() {
            let mut terminal = None;
            for chain in self.chains.keys() {
                let end = self.follow_chain(*entry_node, chain);
                let end_kind = &self.nodes[end];

                if !end_kind.is_terminal() {
                    continue 'outer;
                }

                if let Some(prev) = terminal {
                    let prev_value = self.nodes[prev].terminal_value();
                    let curr_value = self.nodes[end].terminal_value();

                    if prev_value != curr_value {
                        continue 'outer;
                    }

                    if self.nodes[end].is_entry_arg() {
                        terminal = Some(end);
                    }
                } else {
                    terminal = Some(end);
                }

            }
            self.uniform_mappings.insert(*entry_value, terminal.unwrap());
        }

        for entry in self.uniform_mappings.keys() {
            self.entry_edges.remove(entry);
        }
    }

    pub fn expand_phis(&mut self) {
        for entry_node in self.entry_edges.values() {
            match &mut self.nodes[*entry_node] {
                NodeKind::Phi(phi) => {
                    let (_value, scoped_node) = phi.value.unwrap();

                    for chain in self.chains.keys() {
                        if !phi.entries.contains_key(&chain) {
                            phi.entries.insert(chain, scoped_node);
                        }
                    }
                },
                _ => (),
            }
        }
    }

    pub fn reduce_phis2(&mut self) {

        fn collect_relevant_phis(
            graph: &ChainGraph,
            relevant: &mut BTreeSet<Node>,
            node: Node,
            parent_relevant: bool,
        ) {
            match &graph.nodes[node] {
                NodeKind::Phi(phi) => {
                    if parent_relevant {
                        relevant.insert(node);
                    }

                    for dep_node in phi.entries.values() {
                        collect_relevant_phis(
                            graph, relevant, *dep_node, false);
                    }
                }
                kind => {
                    for dep_node in kind.dependencies() {
                        collect_relevant_phis(
                            graph, relevant, dep_node, true);
                    }
                }
            }
        }

        let mut relevant = BTreeSet::new();
        for entry in self.entry_edges.values() {
            collect_relevant_phis(self, &mut relevant, *entry, true);
        }

        for phi in relevant.iter() {
            let new = self.insert_anon_phi();

            let mut a_node = None;
            let mut is_mono = true;

            for chain in self.chains.keys() {
                if let Some(end) = self.follow_chain_maybe(*phi, chain) {
                    if let Some(prev) = a_node {
                        if end != prev {
                            is_mono = false;
                        }
                    } else {
                        a_node = Some(end);
                    }

                    self.phi_add_entry(new, chain, end);
                }
            }

            if is_mono {
                self.aliases.insert(*phi, a_node.unwrap());
            } else {
                self.aliases.insert(*phi, new);
            }
        }

        self.propagate_graph_aliases();
    }

}

/// Graph decycle
//impl ChainGraph {
//
//    pub fn decycle(&mut self) {
//
//        #[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
//        enum EdgeRef {
//            Chain(Chain),
//            Value(Value),
//        }
//        impl EdgeRef {
//            fn chain(&self) -> Chain {
//                match self {
//                    EdgeRef::Chain(chain) => *chain,
//                    _ => unreachable!(),
//                }
//            }
//            fn value(&self) -> Value {
//                match self {
//                    EdgeRef::Value(value) => *value,
//                    _ => unreachable!(),
//                }
//            }
//        }
//
//        let mut updates = BTreeMap::new();
//        let mut new_value_nodes = BTreeSet::new();
//
//        fn rec(
//            cg: &ChainGraph,
//            updates: &mut BTreeMap<(Node, EdgeRef), Value>,
//            new_value_nodes: &mut BTreeSet<Value>,
//            last_tier: Option<usize>,
//            last_edge: Option<(Node, EdgeRef)>,
//            node: Node,
//        ) {
//            match &cg.nodes[node] {
//                NodeKind::Scope(_value) => (),
//                NodeKind::EntryArg(_entry_arg) => (),
//                NodeKind::Phi(phi) => {
//                    let tier = phi.tier.unwrap();
//
//                    if let Some(last_tier) = last_tier {
//                        println!("{} <= {}", tier, last_tier);
//                    }
//
//                    match last_tier {
//                        Some(last_tier) if tier <= last_tier => {
//                            println!("!!!! {} <= {}", tier, last_tier);
//
//                            let curr_value = phi.value.unwrap();
//                            new_value_nodes.insert(curr_value);
//                            updates.insert(last_edge.unwrap(), curr_value);
//                        },
//                        _ => {
//                            for (from_block, value_node) in phi.entries.iter() {
//                                rec(
//                                    cg,
//                                    updates,
//                                    new_value_nodes,
//                                    Some(tier),
//                                    Some((node, EdgeRef::Chain(*from_block))),
//                                    *value_node,
//                                );
//                            }
//                        },
//                    }
//
//                },
//                NodeKind::Prim(prim) => {
//                    for (from_value, value_node) in prim.dependencies.iter() {
//                        rec(
//                            cg,
//                            updates,
//                            new_value_nodes,
//                            last_tier,
//                            Some((node, EdgeRef::Value(*from_value))),
//                            *value_node,
//                        );
//                    }
//                },
//                NodeKind::BlockCapture(bc) => {
//                    for (from_value, value_node) in bc.dependencies.iter() {
//                        rec(
//                            cg,
//                            updates,
//                            new_value_nodes,
//                            last_tier,
//                            Some((node, EdgeRef::Value(*from_value))),
//                            *value_node,
//                        );
//                    }
//                },
//            }
//        }
//
//        for entry_node in self.entry_edges.values() {
//            rec(self, &mut updates, &mut new_value_nodes, None, None, *entry_node);
//        }
//
//        println!("UPDATES: {:?}", updates);
//
//        let mut new_values = BTreeMap::new();
//
//        for value in new_value_nodes.iter() {
//            if let Some(existing) = self.get_terminal_value_node(*value) {
//                new_values.insert(*value, existing);
//            } else {
//                let node = self.nodes.push(NodeKind::Scope(*value));
//                self.active_nodes.insert(node);
//                self.terminal_values.insert(*value, node);
//                new_values.insert(*value, node);
//            }
//        }
//
//        for ((node, edge), value) in updates.iter() {
//            let new_target = new_values[value];
//
//            let node = &mut self.nodes[*node];
//            match node {
//                NodeKind::Scope(_) => unreachable!(),
//                NodeKind::EntryArg(_) => unreachable!(),
//                NodeKind::Phi(phi) => {
//                    let chain = edge.chain();
//                    phi.entries.insert(chain, new_target);
//                }
//                NodeKind::Prim(prim) => {
//                    let value = edge.value();
//                    prim.dependencies.insert(value, new_target);
//                }
//                NodeKind::BlockCapture(bc) => {
//                    let value = edge.value();
//                    bc.dependencies.insert(value, new_target);
//                }
//            }
//        }
//
//    }
//
//}

/// Graph reduction
impl ChainGraph {

    /// This will reduce complex chains of phis into either:
    /// * A single phi block. This is done when selection between several
    ///   incoming paths is required.
    /// * No phi blocks. This is done when there is only one value selected
    ///   reducing the phis.
    pub fn reduce_phis(&mut self) {

        #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
        pub struct Group(u32);
        entity_impl!(Group, "group");

        let mut node_pool = EntitySetPool::new();

        let mut active_phi_groups: BTreeSet<Group> = BTreeSet::new();
        let mut phi_groups: PrimaryMap<Group, EntitySet<Node>> = PrimaryMap::new();
        let mut phi_groups_back: BTreeMap<Node, Group> = BTreeMap::new();

        // Walk the graph while identifying all phi node groups.
        let mut walker = Walker::with(self.entry_edges.values().cloned().collect());
        while let Some(node) = walker.next() {
            let node_data = &self.nodes[node];
            for dep in node_data.dependencies() {
                walker.put(dep);
            }
            if node_data.is_phi() {
                let group = if let Some(group) = phi_groups_back.get(&node) {
                    // The node is part of an already identified group.
                    *group
                } else {
                    // We create a new phi group for this node.
                    let mut set = EntitySet::new();
                    set.insert(node, &mut node_pool);

                    let group = phi_groups.push(set);
                    active_phi_groups.insert(group);

                    phi_groups_back.insert(node, group);

                    group
                };

                for dep in node_data.dependencies() {
                    if self.nodes[dep].is_phi() {
                        if let Some(other_group) = phi_groups_back.get(&dep).cloned() {
                            // We can hit a node that's already present in a
                            // group if the graph joins from two different root
                            // nodes. In this case we simply merge the two.

                            // We disregard `group`.
                            active_phi_groups.remove(&group);

                            let s_group = phi_groups[group].clone();

                            // Union into `other_group`
                            phi_groups[other_group].union(
                                &s_group, &mut node_pool);

                            // Update back mappings for all entries in `group`
                            for s_node in s_group.iter(&node_pool) {
                                phi_groups_back.insert(s_node, other_group);
                            }
                        } else {
                            // Normal case, node is not assigned to a group.
                            // Insert it into the current group, create a back
                            // mapping.
                            phi_groups[group].insert(dep, &mut node_pool);
                            phi_groups_back.insert(dep, group);
                        }
                    }
                }
            }
        }

        // These are used to update graph edges
        let mut final_entries: Vec<(Chain, Node)> = Vec::new();

        for group in active_phi_groups.iter() {
            final_entries.clear();

            let group_set = &phi_groups[*group];
            for node in group_set.iter(&node_pool) {

                match &self.nodes[node] {
                    NodeKind::Phi(phi) => {
                        // For each entry in the phi..
                        for (block, block_node) in phi.entries.iter() {
                            // if the entry is not to another node in the group.
                            if !self.nodes[*block_node].is_phi() {
                                // add the entry to the new phi
                                final_entries.push((*block, *block_node));
                            }
                        }
                    },
                    _ => unreachable!(),
                }
            }

            let new_target;

            trace!("FINAL ENTRIES: {:?}", final_entries);

            // If there is only one target node, we can completely omit the phi.
            assert!(final_entries.len() > 0);
            let first_node = final_entries[0].1;
            if final_entries.iter().all(|(_b, n)| *n == first_node) {

                new_target = first_node;

            } else {

                // The new phi node that encompasses the entire group
                let new_phi = self.insert_anon_phi();

                // Add entries to new phi
                for (chain, from_node) in final_entries.iter() {

                    // If the from node is not a phi itself, then it's always
                    // from outside the group.
                    if !self.nodes[*from_node].is_phi() {

                        //self.phi_add_entry(new_phi, *entry_block, *block_node);
                        match &mut self.nodes[new_phi] {
                            NodeKind::Phi(phi) => {
                                // Sanity check, chain will never occur more
                                // than once.
                                assert!(!phi.entries.contains_key(chain));
                                phi.entries.insert(*chain, *from_node);
                            },
                            _ => panic!(),
                        }

                    }
                }

                new_target = new_phi;

            }

            for node in group_set.iter(&node_pool) {
                // Update all nodes in the graph so they reference the new phi
                // instead of entries in the old group
                self.aliases.insert(node, new_target);
                self.active_nodes.remove(&node);
            }

        }


        // Propagate all aliases, this will update the graph with our changes.
        self.propagate_graph_aliases();


    }

}

#[derive(Debug)]
pub enum NodeKind {
    /// External value
    Scope(Value),
    /// Value on entry point of a chain
    EntryArg(EntryArg),
    /// Phi within chain
    Phi(Phi),
    /// Prim with dependencies
    Prim(Prim),
    /// Block capture with dependencies
    BlockCapture(BlockCapture),
}
impl NodeKind {
    pub fn is_phi(&self) -> bool {
        match self {
            NodeKind::Phi(_) => true,
            _ => false,
        }
    }
    pub fn is_prim(&self) -> bool {
        match self {
            NodeKind::Prim(_) => true,
            _ => false,
        }
    }
    pub fn is_block_capture(&self) -> bool {
        match self {
            NodeKind::BlockCapture(_) => true,
            _ => false,
        }
    }
    pub fn is_scope(&self) -> bool {
        match self {
            NodeKind::Scope(_) => true,
            _ => false,
        }
    }
    pub fn is_entry_arg(&self) -> bool {
        match self {
            NodeKind::EntryArg(_) => true,
            _ => false,
        }
    }

    pub fn is_terminal(&self) -> bool {
        match self {
            NodeKind::Scope(_) => true,
            NodeKind::EntryArg(_) => true,
            _ => false,
        }
    }
    pub fn terminal_value(&self) -> Value {
        match self {
            NodeKind::Scope(value) => *value,
            NodeKind::EntryArg(entry_arg) => entry_arg.arg,
            _ => unreachable!(),
        }
    }

    pub fn dependencies<'a>(&'a self) -> impl Iterator<Item = Node> + 'a {

        enum EIter<T, A, B, C>
        where
            A: Iterator<Item = T>,
            B: Iterator<Item = T>,
            C: Iterator<Item = T>,
        {
            A(A),
            B(B),
            C(C),
            None,
        }
        impl<T, A, B, C> Iterator for EIter<T, A, B, C>
        where
            A: Iterator<Item = T>,
            B: Iterator<Item = T>,
            C: Iterator<Item = T>,
        {
            type Item = T;
            fn next(&mut self) -> Option<T> {
                match self {
                    EIter::A(a) => a.next(),
                    EIter::B(b) => b.next(),
                    EIter::C(c) => c.next(),
                    EIter::None => None,
                }
            }
        }

        match self {
            NodeKind::Phi(i) => EIter::A(i.entries.values().cloned()),
            NodeKind::Prim(i) => EIter::B(i.dependencies.values().cloned()),
            NodeKind::BlockCapture(i) => EIter::C(i.dependencies.values().cloned()),
            NodeKind::Scope(_) => EIter::None,
            NodeKind::EntryArg(_) => EIter::None,
        }
    }

}

/// A value on the entry point on a chain.
#[derive(Debug)]
pub struct EntryArg {
    pub chain: Chain,
    pub arg_index: usize,
    pub arg: Value,
}

/// Selects a value depending on the block it passes through.
#[derive(Debug)]
pub struct Phi {
    pub value: Option<(Value, Node)>,
    pub tier: Option<usize>,
    pub entries: BTreeMap<Chain, Node>,
}

/// Composition using primop
#[derive(Debug)]
pub struct Prim {
    /// The value referencing the original PrimOp.
    /// Used when synthesizing the new CFG.
    pub prim: Value,
    /// Mapping from used value => node
    pub dependencies: BTreeMap<Value, Node>,
}

/// Composition using closure capture
#[derive(Debug)]
pub struct BlockCapture {
    /// Mainly for debugging.
    pub value: Value,
    /// The block being captured.
    /// Used when synthesizing the new CFG.
    pub capture: Block,
    /// Mapping from used value => node
    pub dependencies: BTreeMap<Value, Node>,
}
