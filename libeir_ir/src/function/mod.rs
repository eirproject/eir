use std::collections::{ HashSet };
use std::hash::{ Hash, Hasher };
use std::cmp::Eq;

use cranelift_entity::{ EntityRef, PrimaryMap, ListPool, EntityList, entity_impl };

use libeir_util_datastructures::pooled_entity_set::{ EntitySetPool, EntitySet,
                                                     BoundEntitySet };
use libeir_util_datastructures::aux_hash_map::{ AuxHash, AuxEq };
use libeir_util_datastructures::dedup_aux_primary_map::DedupAuxPrimaryMap;

use libeir_diagnostics::{ ByteSpan, DUMMY_SPAN };

use crate::{ FunctionIdent };
use crate::constant::{ ConstantContainer, Const, ConstKind };
use crate::pattern::{ PatternContainer, PatternClause };

pub mod builder;

mod pool_container;
use pool_container::PoolContainer;

mod op;
pub use op::{ OpKind, MatchKind, BasicType, MapPutUpdate, CallKind };

mod primop;
pub use primop::{ PrimOpKind, BinOp, LogicOp };

mod value;
use value::ValueMap;
pub use value::{ Value, ValueKind };

mod location;
pub use location::{Location, LocationContainer};

mod format;
pub use format::{ContainerDebug, ContainerDebugAdapter};

//mod serialize;

/// Block/continuation
#[derive(Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Block(u32);
entity_impl!(Block, "block");

#[derive(Copy, Clone, Hash, PartialEq, Eq)]
pub struct Argument(u32);
entity_impl!(Argument, "argument");

/// Reference to other function
#[derive(Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct FunRef(u32);
entity_impl!(FunRef, "fun_ref");

#[derive(Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct PrimOp(u32);
entity_impl!(PrimOp, "prim_op");

#[derive(Debug, Clone)]
pub struct BlockData {
    pub(crate) arguments: EntityList<Value>,

    pub(crate) op: Option<OpKind>,
    pub(crate) reads: EntityList<Value>,

    pub(crate) location: Location,

    // Auxilary data for graph implementation

    // These will contain all the connected blocks, regardless
    // of whether they are actually alive or not.
    pub(crate) predecessors: EntitySet<Block>,
    pub(crate) successors: EntitySet<Block>,
}

#[derive(Debug, Clone)]
pub struct PrimOpData {
    op: PrimOpKind,
    reads: EntityList<Value>,
}
impl AuxHash<PoolContainer> for PrimOpData {
    fn aux_hash<H: Hasher>(&self, state: &mut H, container: &PoolContainer) {
        self.op.hash(state);
        self.reads.as_slice(&container.value).hash(state);
    }
}
impl AuxEq<PoolContainer> for PrimOpData {
    fn aux_eq(&self, rhs: &PrimOpData, container: &PoolContainer) -> bool {
        if self.op != rhs.op { return false; }
        self.reads.as_slice(&container.value)
            == rhs.reads.as_slice(&container.value)
    }
}

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

#[derive(Debug, Clone)]
pub struct Function {

    // Meta
    ident: FunctionIdent,
    entry_block: Option<Block>,
    span: ByteSpan,

    pub(crate) blocks: PrimaryMap<Block, BlockData>,
    pub(crate) values: ValueMap,
    pub(crate) primops: DedupAuxPrimaryMap<PrimOp, PrimOpData, PoolContainer>,

    pub pool: PoolContainer,

    pattern_container: PatternContainer,
    constant_container: ConstantContainer,

    // Auxiliary information
    pub constant_values: HashSet<Value>,
    pub locations: LocationContainer,
}

impl Function {

    pub fn pat(&self) -> &PatternContainer {
        &self.pattern_container
    }

    pub fn cons(&self) -> &ConstantContainer {
        &self.constant_container
    }

}

/// Values
impl Function {

    pub fn iter_constants(&self) -> std::collections::hash_set::Iter<'_, Value> {
        self.constant_values.iter()
    }

    pub fn const_kind(&self, constant: Const) -> &ConstKind {
        self.constant_container.const_kind(constant)
    }

    pub fn value_kind(&self, value: Value) -> ValueKind {
        self.values[value].kind
    }

    pub fn value_is_constant(&self, value: Value) -> bool {
        self.constant_values.contains(&value)
    }

    pub fn value_list_length(&self, value: Value) -> usize {
        match self.value_kind(value) {
            ValueKind::PrimOp(prim) => {
                if let PrimOpKind::ValueList = self.primop_kind(prim) {
                    return self.primop_reads(prim).len();
                }
            }
            _ => (),
        }
        1
    }

    pub fn value_list_get_n(&self, value: Value, n: usize) -> Option<Value> {
        match self.value_kind(value) {
            ValueKind::PrimOp(prim) => {
                if let PrimOpKind::ValueList = self.primop_kind(prim) {
                    let reads = self.primop_reads(prim);
                    return reads.get(n).cloned();
                }
            }
            _ => (),
        }

        if n == 0 {
            Some(value)
        } else {
            None
        }
    }

    /// If the value is a variable, get its definition block and argument position
    pub fn value_argument(&self, value: Value) -> Option<(Block, usize)> {
        if let ValueKind::Argument(block, arg) = self.values[value].kind {
            Some((block, arg))
        } else {
            None
        }
    }

    pub fn value_block(&self, value: Value) -> Option<Block> {
        if let ValueKind::Block(block) = self.values[value].kind {
            Some(block)
        } else {
            None
        }
    }

    pub fn value_const(&self, value: Value) -> Option<Const> {
        if let ValueKind::Const(con) = &self.values[value].kind {
            Some(*con)
        } else {
            None
        }
    }

    pub fn value_primop(&self, value: Value) -> Option<PrimOp> {
        if let ValueKind::PrimOp(prim) = &self.values[value].kind {
            Some(*prim)
        } else {
            None
        }
    }

    pub fn value_usages(&self, value: Value) -> BoundEntitySet<Block> {
        self.values[value].usages.bind(&self.pool.block_set)
    }

    /// Walks all nested values contained within
    /// the tree of potential PrimOps.
    pub fn value_walk_nested_values<F, R>(&self, value: Value,
                                          visit: &mut F) -> Result<(), R>
    where
        F: FnMut(Value) -> Result<(), R>,
    {
        visit(value)?;
        if let ValueKind::PrimOp(primop) = self.values[value].kind {
            self.primop_walk_nested_values(primop, visit)?;
        }
        Ok(())
    }
    pub fn value_walk_nested_values_mut<F, R>(&mut self, value: Value,
                                              visit: &mut F) -> Result<(), R>
    where
        F: FnMut(&mut Function, Value) -> Result<(), R>,
    {
        visit(self, value)?;
        if let ValueKind::PrimOp(primop) = self.values[value].kind {
            self.primop_walk_nested_values_mut(primop, visit)?;
        }
        Ok(())
    }

}

/// PrimOps
impl Function {

    pub fn primop_kind(&self, primop: PrimOp) -> &PrimOpKind {
        &self.primops[primop].op
    }
    pub fn primop_reads(&self, primop: PrimOp) -> &[Value] {
        &self.primops[primop].reads.as_slice(&self.pool.value)
    }

    pub fn primop_walk_nested_values<F, R>(&self, primop: PrimOp,
                                           visit: &mut F) -> Result<(), R>
    where
        F: FnMut(Value) -> Result<(), R>,
    {
        let data = &self.primops[primop];
        for read in data.reads.as_slice(&self.pool.value) {
            self.value_walk_nested_values(*read, visit)?;
        }
        Ok(())
    }

    pub fn primop_walk_nested_values_mut<F, R>(&mut self, primop: PrimOp,
                                               visit: &mut F) -> Result<(), R>
    where
        F: FnMut(&mut Function, Value) -> Result<(), R>,
    {
        let len = self.primops[primop].reads.as_slice(&self.pool.value).len();
        for n in 0..len {
            let read = self.primops[primop].reads.as_slice(&self.pool.value)[n];
            self.value_walk_nested_values_mut(read, visit)?;
        }
        Ok(())
    }

}

/// Blocks
impl Function {

    fn block_insert(&mut self) -> Block {
        let block = self.blocks.push(BlockData {
            arguments: EntityList::new(),

            op: None,
            reads: EntityList::new(),

            predecessors: EntitySet::new(),
            successors: EntitySet::new(),

            location: self.locations.location_empty(),
        });
        self.values.push(ValueKind::Block(block));
        block
    }

    fn block_arg_insert(&mut self, block: Block) -> Value {
        let arg_num = self.blocks[block].arguments.len(&self.pool.value);
        let val = self.values.push(ValueKind::Argument(block, arg_num));
        self.blocks[block].arguments.push(val, &mut self.pool.value);
        val
    }

    pub fn block_arg_n(&self, block: Block, num: usize) -> Option<Value> {
        self.blocks[block].arguments.get(num, &self.pool.value)
    }

    pub fn block_kind(&self, block: Block) -> Option<&OpKind> {
        self.blocks[block].op.as_ref()
    }

    pub fn block_location(&self, block: Block) -> Location {
        self.blocks[block].location
    }

    pub fn block_entry(&self) -> Block {
        self.entry_block.expect("Entry block not set on function")
    }
    pub fn block_args<B>(&self, block: B) -> &[Value] where B: Into<Block> {
        let block: Block = block.into();
        self.blocks[block].arguments.as_slice(&self.pool.value)
    }

    pub fn block_reads(&self, block: Block) -> &[Value] {
        self.blocks[block].reads.as_slice(&self.pool.value)
    }

    pub fn block_value(&self, block: Block) -> Value {
        self.values.get(ValueKind::Block(block)).unwrap()
    }

    pub fn block_walk_nested_values<F, R>(&self, block: Block,
                                          visit: &mut F) -> Result<(), R>
    where
        F: FnMut(Value) -> Result<(), R>,
    {
        let reads_len = self.blocks[block].reads.as_slice(&self.pool.value).len();
        for n in 0..reads_len {
            let read = self.blocks[block].reads.get(n, &self.pool.value).unwrap();
            self.value_walk_nested_values(read, visit)?;
        }
        Ok(())
    }
    pub fn block_walk_nested_values_mut<F, R>(&mut self, block: Block,
                                              visit: &mut F) -> Result<(), R>
    where
        F: FnMut(&mut Function, Value) -> Result<(), R>,
    {
        let reads_len = self.blocks[block].reads.as_slice(&self.pool.value).len();
        for n in 0..reads_len {
            let read = self.blocks[block].reads.get(n, &self.pool.value).unwrap();
            self.value_walk_nested_values_mut(read, visit)?;
        }
        Ok(())
    }

    pub fn block_op_eq(&self, lb: Block, r_fun: &Function, rb: Block) -> bool {
        match (self.block_kind(lb).unwrap(), r_fun.block_kind(rb).unwrap()) {
            (OpKind::Case { .. }, _) => unimplemented!(),
            (_, OpKind::Case { .. }) => unimplemented!(),
            (OpKind::Call(l), OpKind::Call(r)) => l == r,
            (OpKind::IfBool, OpKind::IfBool) => true,
            (OpKind::Intrinsic(sym1), OpKind::Intrinsic(sym2)) if sym1 == sym2 => true,
            (OpKind::TraceCaptureRaw, OpKind::TraceCaptureRaw) => true,
            (OpKind::TraceConstruct, OpKind::TraceConstruct) => true,
            (OpKind::MapPut { action: a1 }, OpKind::MapPut { action: a2 }) if a1 == a2 => true,
            (OpKind::UnpackValueList(n1), OpKind::UnpackValueList(n2)) if n1 == n2 => true,
            (OpKind::BinaryPush { specifier: s1 }, OpKind::BinaryPush { specifier: s2 }) if s1 == s2 => true,
            (OpKind::Match { branches: b1 }, OpKind::Match { branches: b2 }) if b1 == b2 => true,
            (OpKind::Unreachable, OpKind::Unreachable) => true,
            _ => false,
        }
    }

    // Iterates through ALL blocks in the function container
    pub fn block_iter(&self) -> impl Iterator<Item = Block> {
        self.blocks.keys()
    }

}

/// Graph
impl Function {

    /// Validates graph invariants for the block.
    /// Relatively inexpensive, for debug assertions.
    pub(crate) fn graph_validate_block(&self, block: Block) {
        let block_data = &self.blocks[block];

        let mut successors_set = HashSet::new();
        self.block_walk_nested_values::<_, ()>(block, &mut |val| {
            if let ValueKind::Block(succ_block) = self.value_kind(val) {
                assert!(block_data.successors.contains(
                    succ_block, &self.pool.block_set));
                assert!(self.blocks[succ_block].predecessors.contains(
                    block, &self.pool.block_set));
                successors_set.insert(succ_block);
            }
            Ok(())
        }).unwrap();

        assert!(block_data.successors.size(&self.pool.block_set)
                == successors_set.len());
    }

    /// Validates graph invariants globally, for the whole
    /// function.
    /// Relatively expensive. Should only be used in tests.
    pub fn graph_validate_global(&self) {
        for block in self.blocks.keys() {
            self.graph_validate_block(block);
        }
    }

}

/// Patterns
impl Function {

    pub fn pattern_container(&self) -> &PatternContainer {
        &self.pattern_container
    }

    pub fn pattern_container_mut(&mut self) -> &mut PatternContainer {
        &mut self.pattern_container
    }

}

pub trait GeneralSet<V> {
    fn contains(&self, key: &V, fun: &Function) -> bool;
    fn insert(&mut self, key: V, fun: &mut Function) -> bool;
}
impl<V> GeneralSet<V> for HashSet<V> where V: Hash + Eq {
    fn contains(&self, key: &V, _fun: &Function) -> bool {
        HashSet::contains(self, key)
    }
    fn insert(&mut self, key: V, _fun: &mut Function) -> bool {
        HashSet::insert(self, key)
    }
}
impl<V> GeneralSet<V> for EntitySet<V> where V: EntityRef + SetPoolProvider {
    fn contains(&self, key: &V, fun: &Function) -> bool {
        EntitySet::contains(self, *key, V::pool(fun))
    }
    fn insert(&mut self, key: V, fun: &mut Function) -> bool {
        EntitySet::insert(self, key, V::pool_mut(fun))
    }
}

pub trait SetPoolProvider: Sized {
    fn pool(fun: &Function) -> &EntitySetPool<Self>;
    fn pool_mut(fun: &mut Function) -> &mut EntitySetPool<Self>;
}
impl SetPoolProvider for Block {
    fn pool(fun: &Function) -> &EntitySetPool<Block> {
        &fun.pool.block_set
    }
    fn pool_mut(fun: &mut Function) -> &mut EntitySetPool<Block> {
        &mut fun.pool.block_set
    }
}

impl Function {

    pub fn new(ident: FunctionIdent) -> Self {
        Function {
            ident,
            span: DUMMY_SPAN,

            blocks: PrimaryMap::new(),
            values: ValueMap::new(),
            primops: DedupAuxPrimaryMap::new(),

            entry_block: None,

            pool: PoolContainer {
                value: ListPool::new(),
                clause: ListPool::new(),
                block_set: EntitySetPool::new(),
            },

            pattern_container: PatternContainer::new(),
            constant_container: ConstantContainer::new(),

            constant_values: HashSet::new(),

            locations: LocationContainer::new(),
        }
    }

    pub fn ident(&self) -> &FunctionIdent {
        &self.ident
    }

    pub fn entry_arg_num(&self) -> usize {
        self.block_args(self.block_entry()).len()
    }

    pub fn to_text(&self) -> String {
        use crate::text::{ ToEirText, ToEirTextContext };

        let mut ctx = ToEirTextContext::new();

        let mut out = Vec::new();
        self.to_eir_text(&mut ctx, 0, &mut out).unwrap();
        String::from_utf8(out).unwrap()
    }

}

