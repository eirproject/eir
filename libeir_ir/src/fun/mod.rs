use std::collections::{ HashSet };

use ::cranelift_entity::{ PrimaryMap, ListPool, EntityList,
                          entity_impl };

use libeir_util::pooled_entity_set::{ EntitySetPool, PooledEntitySet };

use libeir_diagnostics::{ ByteSpan, DUMMY_SPAN };

use crate::{ FunctionIdent, ConstantTerm };
use crate::op::OpKind;
use crate::pattern::{ PatternContainer, PatternClause };

mod builder;
pub use builder::{ FunctionBuilder, CaseBuilder };

//mod validate;

//mod graph;
//pub use graph::{ FunctionCfg, CfgNode, CfgEdge };
//pub use petgraph::Direction;

//mod layout;
//pub use layout::Layout;

//pub mod live;

/// Block/continuation
#[derive(Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Block(u32);
entity_impl!(Block, "block");

/// Either a SSA variable, abstraction or a constant
#[derive(Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Value(u32);
entity_impl!(Value, "value");

/// Reference to other function
#[derive(Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct FunRef(u32);
entity_impl!(FunRef, "fun_ref");

#[derive(Debug)]
pub struct BlockData {
    arguments: EntityList<Value>,

    op: Option<OpKind>,
    reads: EntityList<Value>,

    // These will contain all the connected blocks, regardless
    // of whether they are actually alive or not.
    predecessors: PooledEntitySet<Block>,

    span: ByteSpan,
}

#[derive(Debug)]
pub struct ValueData {
    kind: ValueType,
    usages: PooledEntitySet<Block>,

    span: ByteSpan,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ValueType {
    Arg(Block),
    Block(Block),
    Constant(ConstantTerm),
    Alias(Value),
}

#[derive(Debug)]
pub struct WriteToken(Value);

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum Dialect {
    /// Allows all operations, including high level pattern matching construct.
    High,
    /// High minus pattern matching construct.
    Normal,
    /// Continuation passing style.
    /// Normal minus returning calls. Only tail calls allowed.
    CPS,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum AttributeKey {
    Continuation,
}
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AttributeValue {
    None,
}

#[derive(Debug)]
pub struct Function {

    // Meta
    ident: FunctionIdent,
    span: ByteSpan,

    blocks: PrimaryMap<Block, BlockData>,
    values: PrimaryMap<Value, ValueData>,
    fun_refs: PrimaryMap<FunRef, FunctionIdent>,

    entry_block: Option<Block>,

    value_pool: ListPool<Value>,
    clause_pool: ListPool<PatternClause>,
    block_set_pool: EntitySetPool,

    pattern_container: PatternContainer,

    // Auxiliary information
    pub constant_values: HashSet<Value>, // Use EntitySet?

}

/// Values
impl Function {

    pub fn iter_constants<'a>(&'a self) -> std::collections::hash_set::Iter<'a, Value> {
        self.constant_values.iter()
    }

    pub fn value<'a>(&'a self, value: Value) -> &'a ValueType {
        &self.values[value].kind
    }
    pub fn value_is_constant(&self, value: Value) -> bool {
        self.constant_values.contains(&value)
    }
    pub fn value_constant<'a>(&'a self, value: Value) -> &'a ConstantTerm {
        if let ValueType::Constant(con) = &self.values[value].kind {
            con
        } else {
            panic!()
        }
    }

}

/// Blocks
impl Function {

    pub fn block_insert(&mut self) -> Block {
        self.blocks.push(BlockData {
            arguments: EntityList::new(),

            op: None,
            reads: EntityList::new(),

            predecessors: PooledEntitySet::new(),

            span: DUMMY_SPAN,
        })
    }

    pub fn block_arg_insert(&mut self, block: Block) -> Value {
        let val = self.values.push(ValueData {
            kind: ValueType::Arg(block),
            usages: PooledEntitySet::new(),

            span: DUMMY_SPAN,
        });
        self.blocks[block].arguments.push(val, &mut self.value_pool);
        val
    }

    pub fn block_set_entry(&mut self, block: Block) {
        self.entry_block = Some(block);
    }

    fn block_mut<'a>(&'a mut self, block: Block) -> &'a mut BlockData {
        self.blocks.get_mut(block).unwrap()
    }

    pub fn block_arg_n(&self, block: Block, num: usize) -> Option<Value> {
        self.blocks[block].arguments.get(num, &self.value_pool)
    }

}

/// Graph
impl Function {

    // fn block_remove_successors(&mut self, block: Block) {
    //     
    // }

}

/// Patterns
impl Function {

    pub fn pattern_container<'a>(&'a self) -> &'a PatternContainer {
        &self.pattern_container
    }

    pub fn pattern_container_mut<'a>(&'a mut self) -> &'a mut PatternContainer {
        &mut self.pattern_container
    }

}

impl Function {

    pub fn new(ident: FunctionIdent) -> Self {
        Function {
            ident: ident,
            span: DUMMY_SPAN,

            blocks: PrimaryMap::new(),
            values: PrimaryMap::new(),
            fun_refs: PrimaryMap::new(),

            entry_block: None,

            value_pool: ListPool::new(),
            clause_pool: ListPool::new(),
            block_set_pool: EntitySetPool::new(),

            pattern_container: PatternContainer::new(),

            constant_values: HashSet::new(),
        }
    }

    pub fn ident(&self) -> &FunctionIdent {
        &self.ident
    }


    pub fn entry_arg_num(&self) -> usize {
        self.block_args(self.block_entry()).len()
    }

    pub fn block_entry(&self) -> Block {
        self.entry_block.unwrap()
    }
    pub fn block_args<'a>(&'a self, block: Block) -> &'a [Value] {
        self.blocks[block].arguments.as_slice(&self.value_pool)
    }


    //pub fn used_values(&self, set: &mut HashSet<Value>) {
    //    set.clear();
    //    for block in self.iter_block() {
    //        for arg in self.block_args(block) {
    //            set.insert(*arg);
    //        }
    //        for op in self.iter_op(block) {
    //            for read in self.op_reads(op) {
    //                set.insert(*read);
    //            }
    //            for write in self.op_writes(op) {
    //                set.insert(*write);
    //            }
    //            //for branch in self.op_branches(op) {
    //            //    for val in self.block_call_args(*branch) {
    //            //        set.insert(*val);
    //            //    }
    //            //}
    //        }
    //    }
    //}

    //pub fn gen_cfg(&self) -> FunctionCfg {
    //    let mut graph = Graph::new();

    //    let mut blocks = HashMap::new();
    //    let mut ops = HashMap::new();

    //    for block in self.iter_block() {
    //        let idx = graph.add_node(CfgNode::Block(block));
    //        blocks.insert(block, idx);

    //        let mut prev = idx;

    //        for op in self.iter_op(block) {
    //            if self.op_branches(op).len() > 0 || self.op_kind(op).is_block_terminator() {
    //                let op_node = graph.add_node(CfgNode::Op(op));
    //                ops.insert(op, op_node);
    //                graph.add_edge(prev, op_node, CfgEdge::Flow);
    //                prev = op_node;
    //            }
    //        }
    //    }

    //    for block in self.iter_block() {
    //        for op in self.iter_op(block) {
    //            for branch in self.op_branches(op) {
    //                let target = self.block_call_target(*branch);
    //                graph.add_edge(
    //                    ops[&op], blocks[&target], CfgEdge::Call(*branch)
    //                );
    //            }
    //        }
    //    }

    //    FunctionCfg {
    //        graph: graph,
    //        ops: ops,
    //        blocks: blocks,
    //    }
    //}

    //pub fn live_values(&self) -> self::live::LiveValues {
    //    self::live::calculate_live_values(self)
    //}

    //pub fn get_all_static_calls(&self) -> Vec<FunctionIdent> {
    //    let mut res = Vec::new();
    //    for block in self.iter_block() {
    //        for op in self.iter_op(block) {
    //            let kind = self.op_kind(op);
    //            match kind {
    //                OpKind::CaptureNamedFunction(ident) => {
    //                    res.push(ident.clone());
    //                },
    //                OpKind::Call { arity, .. } => {
    //                    let reads = self.op_reads(op);
    //                    match (self.value(reads[0]), self.value(reads[1])) {
    //                        (
    //                            ValueType::Constant(ConstantTerm::Atomic(
    //                                AtomicTerm::Atom(module))),
    //                            ValueType::Constant(ConstantTerm::Atomic(
    //                                AtomicTerm::Atom(name))),
    //                        ) => {
    //                            res.push(FunctionIdent {
    //                                module: module.clone(),
    //                                name: name.clone(),
    //                                lambda: None,
    //                                arity: *arity,
    //                            });
    //                        },
    //                        _ => (),
    //                    }
    //                },
    //                OpKind::BindClosure { ident } => {
    //                    res.push(ident.clone());
    //                },
    //                _ => (),
    //            }
    //        }
    //    }
    //    res
    //}

    //pub fn to_text(&self) -> String {
    //    use crate::text::{ ToEirText, ToEirTextContext };

    //    let mut ctx = ToEirTextContext::new();

    //    let mut out = Vec::new();
    //    self.to_eir_text(&mut ctx, 0, &mut out).unwrap();
    //    String::from_utf8(out).unwrap()
    //}

    //pub fn to_text_annotated_live_values(&self) -> String {
    //    use crate::text::{ ToEirText, ToEirTextContext, EirLiveValuesAnnotator };

    //    let mut ctx = ToEirTextContext::new();
    //    ctx.add_annotator(EirLiveValuesAnnotator::new());

    //    let mut out = Vec::new();
    //    self.to_eir_text(&mut ctx, 0, &mut out).unwrap();
    //    String::from_utf8(out).unwrap()
    //}

}

