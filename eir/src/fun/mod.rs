use crate::{ FunctionIdent, ConstantTerm, AtomicTerm, LambdaEnvIdx };
use crate::Clause;
use crate::op::OpKind;
use ::cranelift_entity::{ PrimaryMap, SecondaryMap, ListPool, EntityList,
                          EntitySet, entity_impl };
use ::cranelift_entity::packed_option::PackedOption;
use std::collections::{ HashMap, HashSet };

use petgraph::graph::{ Graph, NodeIndex };

pub mod builder;
pub use builder::FunctionBuilder;

mod validate;

mod graph;
pub use graph::{ FunctionCfg, CfgNode, CfgEdge };

/// Basic block in function
#[derive(Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Ebb(u32);
entity_impl!(Ebb, "ebb");

/// OP in EBB
#[derive(Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Op(u32);
entity_impl!(Op, "op");

/// Either a SSA variable or a constant
#[derive(Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Value(u32);
entity_impl!(Value, "value");

/// Call from OP to other EBB
#[derive(Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct EbbCall(u32);
entity_impl!(EbbCall, "ebb_call");

/// Reference to other function
#[derive(Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct FunRef(u32);
entity_impl!(FunRef, "fun_ref");

#[derive(Clone, Debug, Default)]
pub struct EbbNode {
    prev: Option<Ebb>,
    next: Option<Ebb>,
    first_op: Option<Op>,
    last_op: Option<Op>,
}

#[derive(Clone, Debug, Default)]
pub struct OpNode {
    ebb: Option<Ebb>,
    prev: Option<Op>,
    next: Option<Op>,
}

#[derive(Debug)]
pub struct Layout {
    ebbs: SecondaryMap<Ebb, EbbNode>,
    ops: SecondaryMap<Op, OpNode>,
    first_ebb: Option<Ebb>,
    last_ebb: Option<Ebb>,
}
impl Layout {

    pub fn new() -> Self {
        Layout {
            ebbs: SecondaryMap::new(),
            ops: SecondaryMap::new(),
            first_ebb: None,
            last_ebb: None,
        }
    }

    pub fn insert_ebb_first(&mut self, ebb: Ebb) {
        assert!(self.first_ebb.is_none());
        assert!(self.last_ebb.is_none());
        self.first_ebb = Some(ebb);
        self.last_ebb = Some(ebb);
    }

    pub fn remove_ebb(&mut self, ebb: Ebb) {
        let next = self.ebbs[ebb].next;
        let prev = self.ebbs[ebb].prev;

        if let Some(next) = next {
            assert!(self.ebbs[next].prev == Some(ebb));
            self.ebbs[next].prev = prev;
        } else {
            assert!(self.last_ebb == Some(ebb));
            self.last_ebb = prev;
        }

        if let Some(prev) = prev {
            assert!(self.ebbs[prev].next == Some(ebb));
            self.ebbs[prev].next = next;
        } else {
            assert!(self.first_ebb == Some(ebb));
            self.first_ebb = next;
        }
    }

    pub fn insert_ebb_after(&mut self, prev: Ebb, ebb: Ebb) {
        // TODO: Validate not inserted
        let next = self.ebbs[prev].next;
        self.ebbs[prev].next = Some(ebb);
        self.ebbs[ebb].prev = Some(prev);
        self.ebbs[ebb].next = next;
        if let Some(next) = next {
            self.ebbs[next].prev = Some(ebb);
        }
    }

    pub fn insert_op_after(&mut self, ebb: Ebb, prev_op: Option<Op>, op: Op) {
        assert!(self.ops[op].ebb == None);
        self.ops[op].ebb = Some(ebb);

        if let Some(prev_op) = prev_op {
            // If a previous operation is selected,

            let next = self.ops[prev_op].next;
            // Update previous and next of current
            self.ops[op].prev = Some(prev_op);
            self.ops[op].next = next;

            // Set the next of previous to current
            self.ops[prev_op].next = Some(op);

            // If there is a next operation,
            // set it's previous to current
            // else,
            // set the last Ebb Op to current
            if let Some(next) = next {
                assert!(self.ops[next].prev == Some(prev_op));
                self.ops[next].prev = Some(op);
            } else {
                assert!(self.ebbs[ebb].last_op == Some(prev_op));
                self.ebbs[ebb].last_op = Some(op);
            }
        } else {
            // No previous operation selected, insert at beginning

            // Get the Op that is at block start
            let next = self.ebbs[ebb].first_op;

            // Set the next of the current Op to that
            self.ops[op].next = next;

            // Set the first Op of the Ebb to the current Op
            self.ebbs[ebb].first_op = Some(op);

            if let Some(next) = next {
                // If there was an Op after this one, set its previous to current
                assert!(self.ops[next].prev == None);
                self.ops[next].prev = Some(op);
            } else {
                // If there was no Op after this one, set the last Op to current
                assert!(self.ebbs[ebb].last_op == None);
                self.ebbs[ebb].last_op = Some(op);
            }

        }

    }

}

#[derive(Debug)]
pub struct OpData {
    kind: OpKind,
    reads: EntityList<Value>,
    writes: EntityList<Value>,
    ebb_calls: EntityList<EbbCall>,
}

#[derive(Debug)]
pub struct EbbData {
    arguments: EntityList<Value>,
    finished: bool,
}

#[derive(Debug, PartialEq, Eq)]
pub enum ValueType {
    Variable,
    Constant(ConstantTerm),
}

#[derive(Debug)]
pub struct EbbCallData {
    block: Ebb,
    values: EntityList<Value>,
}

pub struct EbbIter<'a> {
    fun: &'a Function,
    next: Option<Ebb>,
}
impl<'a> Iterator for EbbIter<'a> {
    type Item = Ebb;
    fn next(&mut self) -> Option<Self::Item> {
        let next = self.next;
        match self.next {
            Some(n) => self.next = self.fun.layout.ebbs[n].next,
            None => (),
        }
        next
    }
}

pub struct OpIter<'a> {
    fun: &'a Function,
    next: Option<Op>,
}
impl<'a> Iterator for OpIter<'a> {
    type Item = Op;
    fn next(&mut self) -> Option<Self::Item> {
        let next = self.next;
        match self.next {
            Some(n) => self.next = self.fun.layout.ops[n].next,
            None => (),
        }
        next
    }
}

#[derive(Debug)]
pub struct Function {

    ident: FunctionIdent,

    layout: Layout,

    ops: PrimaryMap<Op, OpData>,
    ebbs: PrimaryMap<Ebb, EbbData>,
    values: PrimaryMap<Value, ValueType>,
    ebb_calls: PrimaryMap<EbbCall, EbbCallData>,
    fun_refs: PrimaryMap<FunRef, FunctionIdent>,

    ebb_call_pool: ListPool<EbbCall>,
    value_pool: ListPool<Value>,

    // Auxiliary information
    constant_values: HashSet<Value>, // Use EntitySet?

}

impl Function {

    pub fn new(ident: FunctionIdent) -> Self {
        Function {
            ident: ident,
            layout: Layout::new(),

            ops: PrimaryMap::new(),
            ebbs: PrimaryMap::new(),
            values: PrimaryMap::new(),
            ebb_calls: PrimaryMap::new(),
            fun_refs: PrimaryMap::new(),

            ebb_call_pool: ListPool::new(),
            value_pool: ListPool::new(),

            constant_values: HashSet::new(),
        }
    }

    pub fn ebb_remove(&mut self, ebb: Ebb) {
        self.layout.remove_ebb(ebb)
    }

    pub fn new_variable(&mut self) -> Value {
        self.values.push(ValueType::Variable)
    }

    pub fn ident(&self) -> &FunctionIdent {
        &self.ident
    }

    pub fn iter_ebb<'a>(&'a self) -> EbbIter<'a> {
        EbbIter {
            fun: self,
            next: self.layout.first_ebb,
        }
    }
    pub fn iter_op<'a>(&'a self, ebb: Ebb) -> OpIter<'a> {
        OpIter {
            fun: self,
            next: self.layout.ebbs[ebb].first_op,
        }
    }

    pub fn iter_constants<'a>(&'a self) -> std::collections::hash_set::Iter<'a, Value> {
        self.constant_values.iter()
    }

    pub fn ebb_entry(&self) -> Ebb {
        self.layout.first_ebb.unwrap()
    }

    pub fn ebb_args<'a>(&'a self, ebb: Ebb) -> &'a [Value] {
        self.ebbs[ebb].arguments.as_slice(&self.value_pool)
    }

    pub fn ebb_call_target<'a>(&'a self, ebb: EbbCall) -> Ebb {
        self.ebb_calls[ebb].block
    }
    pub fn ebb_call_args<'a>(&'a self, ebb: EbbCall) -> &'a [Value] {
        self.ebb_calls[ebb].values.as_slice(&self.value_pool)
    }

    pub fn op_kind<'a>(&'a self, op: Op) -> &'a OpKind {
        &self.ops[op].kind
    }
    pub fn op_writes<'a>(&'a self, op: Op) -> &[Value] {
        self.ops[op].writes.as_slice(&self.value_pool)
    }
    pub fn op_reads<'a>(&'a self, op: Op) -> &[Value] {
        self.ops[op].reads.as_slice(&self.value_pool)
    }
    pub fn op_branches<'a>(&'a self, op: Op) -> &[EbbCall] {
        self.ops[op].ebb_calls.as_slice(&self.ebb_call_pool)
    }

    pub fn value<'a>(&'a self, value: Value) -> &'a ValueType {
        &self.values[value]
    }

    pub fn used_values(&self, set: &mut HashSet<Value>) {
        set.clear();
        for ebb in self.iter_ebb() {
            for arg in self.ebb_args(ebb) {
                set.insert(*arg);
            }
            for op in self.iter_op(ebb) {
                for read in self.op_reads(op) {
                    set.insert(*read);
                }
                for write in self.op_writes(op) {
                    set.insert(*write);
                }
            }
        }
    }

    pub fn gen_cfg(&self) -> FunctionCfg {
        let mut graph = Graph::new();

        let mut blocks = HashMap::new();
        let mut ops = HashMap::new();

        for ebb in self.iter_ebb() {
            let idx = graph.add_node(CfgNode::Ebb(ebb));
            blocks.insert(ebb, idx);

            let mut prev = idx;

            for op in self.iter_op(ebb) {
                if self.op_branches(op).len() > 0 {
                    let op_node = graph.add_node(CfgNode::Op(op));
                    ops.insert(op, op_node);
                    graph.add_edge(prev, op_node, CfgEdge::Flow);
                    prev = op_node;
                }
            }
        }

        for ebb in self.iter_ebb() {
            for op in self.iter_op(ebb) {
                for branch in self.op_branches(op) {
                    let target = self.ebb_call_target(*branch);
                    graph.add_edge(
                        ops[&op], blocks[&target], CfgEdge::Call(*branch)
                    );
                }
            }
        }

        FunctionCfg {
            graph: graph,
            ops: ops,
            ebbs: blocks,
        }
    }

    pub fn to_text(&self) -> String {
        use crate::text::ToEirText;

        let mut out = Vec::new();
        self.to_eir_text(0, &mut out).unwrap();
        String::from_utf8(out).unwrap()
    }

}

