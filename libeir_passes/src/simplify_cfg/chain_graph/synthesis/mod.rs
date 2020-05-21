#![allow(dead_code)]

use std::collections::BTreeMap;

use libeir_ir::{Function, Value};
use cranelift_entity::{ListPool, EntityList, PrimaryMap, SecondaryMap, entity_impl};
use libeir_util_datastructures::pooled_entity_set::{EntitySet, EntitySetPool};
use libeir_util_datastructures::aux_traits::{AuxDebug, AuxImpl};
use super::{ChainGraph, Node, Chain};

pub mod simple;
pub mod single;
pub mod terminating_target;
pub mod compound;

pub struct Synthesis {
    pub segments: PrimaryMap<Segment, SegmentData>,
    pub order: Vec<Segment>,

    pub instances: PrimaryMap<Instance, InstanceKind>,

    pub substitutions: BTreeMap<Chain, Value>,

    pub segments_back: Option<SecondaryMap<Segment, EntitySet<Segment>>>,

    pub chain_set_pool: EntitySetPool<Chain>,
    pub node_pool: ListPool<Node>,
    pub instance_pool: ListPool<Instance>,
    pub segment_set_pool: Option<EntitySetPool<Segment>>,
}

impl std::fmt::Debug for Synthesis {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let mut b = f.debug_struct("Synthesis");
        b.field("segments", &AuxImpl(&self.segments, self));
        b.field("order", &self.order);
        b.field("instances", &self.instances);
        b.field("substitutions", &self.substitutions);
        match &self.segments_back {
            None => {
                let none: Option<()> = None;
                b.field("segments_back", &none)
            },
            Some(inner) =>
                b.field("segments_back", &AuxImpl(
                    inner,
                    self.segment_set_pool.as_ref().unwrap()
                )),
        };
        b.finish()
    }
}

impl Synthesis {

    pub fn new() -> Self {
        Synthesis {
            segments: PrimaryMap::new(),
            order: Vec::new(),

            instances: PrimaryMap::new(),

            substitutions: BTreeMap::new(),

            segments_back: None,

            chain_set_pool: EntitySetPool::new(),
            node_pool: ListPool::new(),
            instance_pool: ListPool::new(),
            segment_set_pool: None,
        }
    }

    fn create_segment(&mut self, head: SegmentHeadKind) -> Segment {
        self.segments.push(SegmentData {
            head,
            in_args: EntityList::new(),
            externals: EntityList::new(),
            instances: EntityList::new(),
            body: SegmentBodyKind::None,

            chains: EntitySet::new(),
            node_ord: EntityList::new(),
        })
    }

    pub fn create_entry_segment(&mut self, chain: Chain, graph: &ChainGraph) -> Segment {
        let segment = self.create_segment(SegmentHeadKind::Entry {
            chain,
        });

        let mut in_args = EntityList::new();
        for arg in graph.chains[chain].args.iter() {
            let instance = match arg {
                Some(arg_node) => self.instances.push(InstanceKind::Arg {
                    node: *arg_node,
                    by: segment,
                }),
                None => self.instances.push(InstanceKind::NotRelevant),
            };
            in_args.push(instance, &mut self.instance_pool);
        }

        self.segments[segment].in_args = in_args;

        segment
    }

    pub fn create_intermediate_segment(&mut self) -> Segment {
        self.create_segment(SegmentHeadKind::Intermediate)
    }

    pub fn segment<'a>(&'a mut self, segment: Segment) -> SegmentBuilder<'a> {
        SegmentBuilder {
            synthesis: self,
            segment,
            out_args: EntityList::new(),
        }
    }

    pub fn visit_segment(&mut self, segment: Segment) {
        self.order.push(segment);
    }

    pub fn substitute(&mut self, chain: Chain, value: Value) {
        if let Some(prev_val) = self.substitutions.get(&chain) {
            assert!(*prev_val == value);
        } else {
            self.substitutions.insert(chain, value);
        }
    }

    /// This will perform necessary postprocessing of the synthesis.
    /// Does things like populate chains to segments, generate node order.
    pub fn postprocess(&mut self, graph: &ChainGraph) {
        let mut pool = EntitySetPool::new();
        let mut back: SecondaryMap<Segment, EntitySet<Segment>> = SecondaryMap::new();

        let mut walker = crate::util::Walker::new();

        for segment_id in self.order.iter() {
            let segment = &self.segments[*segment_id];
            match segment.body {
                SegmentBodyKind::None => panic!(),
                SegmentBodyKind::Terminal { .. } => {
                    walker.put(*segment_id);
                },
                SegmentBodyKind::ToIntermediate { to, .. } => {
                    back[to].insert(*segment_id, &mut pool);
                }
            }
        }

        let mut segment_order = Vec::new();
        while let Some(segment_id) = walker.next() {
            let segment = &self.segments[segment_id];
            segment_order.push(segment_id);
            for pred in back[segment_id].iter(&pool) {
                walker.put(pred);
            }
        }

        for segment_id in segment_order.iter().rev() {
            let segment = &self.segments[*segment_id];
            let mut chains = segment.chains.clone();

            match &segment.head {
                SegmentHeadKind::Entry { chain } => {
                    chains.insert(*chain, &mut self.chain_set_pool);
                },
                _ => (),
            }

            for pred in back[*segment_id].iter(&pool) {
                chains.union(
                    &self.segments[pred].chains,
                    &mut self.chain_set_pool
                );
            }

            self.segments[*segment_id].chains = chains;
        }

        self.segment_set_pool = Some(pool);
        self.segments_back = Some(back);
    }

}

pub struct SegmentBuilder<'a> {
    synthesis: &'a mut Synthesis,
    segment: Segment,
    out_args: EntityList<Instance>,
}
impl<'a> SegmentBuilder<'a> {

    fn create_arg_instance(&mut self, node: Node) -> Instance {
        self.synthesis.instances.push(InstanceKind::Arg {
            node,
            by: self.segment,
        })
    }

    fn create_creation_instance(&mut self, node: Node) -> Instance {
        self.synthesis.instances.push(InstanceKind::BaseCreation {
            node,
            by: self.segment,
        })
    }

    pub fn push_in_arg(&mut self, node: Node) -> Instance {
        let instance = self.create_arg_instance(node);

        let seg = &mut self.synthesis.segments[self.segment];
        assert!(seg.head.is_intermediate());
        assert!(seg.body.is_none());

        seg.in_args.push(instance, &mut self.synthesis.instance_pool);

        instance
    }

    pub fn push_external(&mut self, instance: Instance) {
        let seg = &mut self.synthesis.segments[self.segment];
        assert!(seg.body.is_none());

        seg.externals.push(instance, &mut self.synthesis.instance_pool);
    }

    pub fn push_instance(&mut self, node: Node) -> Instance {
        let instance = self.create_creation_instance(node);

        let seg = &mut self.synthesis.segments[self.segment];
        assert!(seg.body.is_none());

        seg.instances.push(instance, &mut self.synthesis.instance_pool);

        instance
    }

    pub fn push_out_arg(&mut self, instance: Instance) {
        // TODO validate instance in scope?
        self.out_args.push(instance, &mut self.synthesis.instance_pool);
    }

    pub fn finish_to(&mut self, segment: Segment) {
        let seg = &mut self.synthesis.segments[self.segment];
        assert!(seg.body.is_none());
        seg.body = SegmentBodyKind::ToIntermediate {
            to: segment,
            out_args: self.out_args.clone(),
        };
    }

    pub fn finish_target(&mut self, single: bool) {
        let seg = &mut self.synthesis.segments[self.segment];
        assert!(seg.body.is_none());
        seg.body = SegmentBodyKind::Terminal {
            single,
        };
    }

}

/// An instance of a node in the syntheized CFG.
/// Can only be created once, in a single segment.
/// This is a node that is instantiated in a set of chains.
/// This is used as an unique id for a node instantiation when rewriting the
/// EIR.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Instance(u32);
entity_impl!(Instance, "instance");

#[derive(Debug)]
pub enum InstanceKind {
    BaseCreation {
        /// The node this is an instance of.
        node: Node,
        /// The segment that created this instance.
        by: Segment,
    },
    Arg {
        /// The node this is an instance of.
        node: Node,
        /// The segment that created this instance.
        by: Segment,
    },
    NotRelevant,
}
impl InstanceKind {
    pub fn is_relevant(&self) -> bool {
        match self {
            InstanceKind::NotRelevant => false,
            _ => true,
        }
    }
    pub fn node(&self) -> Node {
        match self {
            InstanceKind::BaseCreation { node, .. } => *node,
            InstanceKind::Arg { node, .. } => *node,
            InstanceKind::NotRelevant => unreachable!(),
        }
    }
}

/// Synthesized segment, mostly equivalent to a block in the resulting EIR.
#[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub struct Segment(u32);
entity_impl!(Segment, "segment");

/// This will:
/// 1. Select a block according to the `head`
/// 2. Insert `in_args` and `externals` into the local scope
/// 3. Map all `instances` to the local scope, insert them into the local scope
/// 4. Generate the body of the selected block according to `body`
#[derive(Debug)]
pub struct SegmentData {
    /// The head of the segment, how the block is created, how control flow
    /// enters this segment.
    pub head: SegmentHeadKind,
    /// The instances that are created by arguments on the head.
    /// * If the head is a chain entry, the count must be equal to the entry
    ///   argument count.
    /// * If the head is an intermediate block, this length will specify the
    ///   argument count.
    pub in_args: EntityList<Instance>,
    /// The externals of this segment, instances that are asserted to be in
    /// scope, but does not enter the segment through the head.
    pub externals: EntityList<Instance>,
    /// The instances created by this segment.
    pub instances: EntityList<Instance>,
    /// The body of the segment, how control flow leaves this segment.
    pub body: SegmentBodyKind,

    /// All chains that pass through this segment.
    /// This should not be generated by the strategy, it will be populated
    /// afterwards.
    pub chains: EntitySet<Chain>,
    /// A valid lowering order for nodes in the segment.
    /// This should not be generated by the strategy, it will be populated
    /// afterwards.
    pub node_ord: EntityList<Instance>,
}

impl AuxDebug<Synthesis> for SegmentData {
    fn aux_fmt(&self, f: &mut std::fmt::Formatter<'_>, aux: &Synthesis) -> std::fmt::Result {
        let mut b = f.debug_struct("SegmentData");
        b.field("head", &self.head);
        b.field("in_args", &AuxImpl(&self.in_args, &aux.instance_pool));
        b.field("externals", &AuxImpl(&self.externals, &aux.instance_pool));
        b.field("instances", &AuxImpl(&self.instances, &aux.instance_pool));
        b.field("body", &AuxImpl(&self.body, aux));
        b.field("chains", &AuxImpl(&self.chains, &aux.chain_set_pool));
        b.field("node_ord", &AuxImpl(&self.node_ord, &aux.instance_pool));
        b.finish()
    }
}

#[derive(Debug)]
pub enum SegmentHeadKind {
    /// The entry point of a chain.
    /// There must be exactly one of these for each chain.
    /// The arity of this block is always the same as on the original entry
    /// block.
    ///
    /// This will:
    /// 1. Create a new block
    /// 2. Add arguments matching arity of orignal entry
    /// 3. Insert mapping "old entry => this (new entry)"
    Entry {
        chain: Chain,
    },

    /// An intermediate block in the synthesized CFG.
    /// This can be reached from any number of chains.
    ///
    /// This will:
    /// 1. Create a new block
    /// 2. Create `in_args` on block
    Intermediate,
}

impl SegmentHeadKind {

    pub fn is_intermediate(&self) -> bool {
        match self {
            SegmentHeadKind::Intermediate => true,
            _ => false,
        }
    }

}

#[derive(Debug)]
pub enum SegmentBodyKind {
    /// Not populated
    None,
   
    /// This will call an intermediate block in the synthesized CFG.
    ///
    /// This will:
    /// 1. Create a control flow call within the selected block with `out_args`
    ///    as arguments.
    ToIntermediate {
        to: Segment,
        out_args: EntityList<Instance>,
    },

    /// This will specialize the target block into the selected block.
    ///
    /// Specialization will occur with the full local scope. All root value
    /// nodes must be in the local scope for this to be valid.
    ///
    /// Note: Specialization will ONLY happen on the operation within the target
    /// block. This needs to be taken into account by the synthesis strategy, as
    /// incorrect IR will be generated if any successor blocks attempt to access
    /// instances that are not
    Terminal {
        /// There can only ever exist a single segment with `single: true`.
        ///
        /// * If this is true, all result mappings from this will be applied
        ///   globally. This is the general and safe mode. This usually means
        ///   there will only be a single target segment.
        ///
        ///   A notable exception to this is having one `single: true` segment
        ///   and one or more `single: false` segments. This can be valid in
        ///   very specific cases.
        ///  
        /// * If this is false, no result mappings are applied globally, only
        ///   when specializing the target operation.
        ///
        ///   This is a useful mode when no values from the chain are accessed
        ///   in successors of the target block. A trivial example of this is
        ///   when a return call is performed in the target block.
        single: bool,
    },
}

impl AuxDebug<Synthesis> for SegmentBodyKind {
    fn aux_fmt(&self, f: &mut std::fmt::Formatter<'_>, aux: &Synthesis) -> std::fmt::Result {
        match self {
            SegmentBodyKind::None => f.debug_struct("None").finish(),
            SegmentBodyKind::ToIntermediate { to, out_args } => {
                let mut b = f.debug_struct("ToIntermediate");
                b.field("to", to);
                b.field("out_args", &AuxImpl(out_args, &aux.instance_pool));
                b.finish()
            },
            SegmentBodyKind::Terminal { single } => {
                let mut b = f.debug_struct("Terminal");
                b.field("single", single);
                b.finish()
            }
        }
    }
}

impl SegmentBodyKind {
    pub fn is_none(&self) -> bool {
        match self {
            SegmentBodyKind::None => true,
            _ => false,
        }
    }
}

pub trait SynthesisStrategy {
    /// If the strategy is compatible with the given graph, returns the
    /// synthesized CFG.
    fn try_run(&self, graph: &ChainGraph, fun: &Function) -> Option<Synthesis>;
}
