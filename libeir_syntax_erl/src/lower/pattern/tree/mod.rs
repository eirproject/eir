//! Small IR used to do linting and transformations on patterns
//! before they are lowered to Eir.

use std::collections::{ HashMap, BTreeSet };

use either::Either;

use cranelift_entity::{ PrimaryMap, SecondaryMap, EntityList, ListPool,
                        entity_impl };


use libeir_ir::{
    FunctionBuilder,
    Value as IrValue,
    Const,
    BinaryEntrySpecifier,
    PrimOp,
};

use libeir_diagnostics::{ ByteSpan };
use libeir_intern::Ident;

use crate::lower::LowerCtx;

mod from_expr;

mod merge;
use self::merge::merge_tree_nodes;

mod promote_values;
use self::promote_values::promote_values;

mod lower;

#[derive(Debug)]
pub(crate) struct Tree {
    roots: Vec<TreeNode>,
    pub unmatchable: bool,

    nodes: PrimaryMap<TreeNode,TreeNodeKind>,
    node_pool: ListPool<TreeNode>,

    binds: SecondaryMap<TreeNode, Vec<Ident>>,

    constraints: SecondaryMap<TreeNode, BTreeSet<ConstraintKind>>,
    resolved_binds: Option<HashMap<Ident, TreeNode>>,
}
impl Tree {

    pub fn new() -> Self {
        Tree {
            roots: Vec::new(),
            unmatchable: false,

            nodes: PrimaryMap::new(),
            node_pool: ListPool::new(),

            binds: SecondaryMap::new(),

            constraints: SecondaryMap::new(),
            resolved_binds: None,
        }
    }

    pub fn process(
        &mut self,
        ctx: &mut LowerCtx,
        b: &mut FunctionBuilder,
        shadow: bool,
    ) {
        merge_tree_nodes(ctx, b, self);
        promote_values(ctx, b, self, shadow);
    }

    pub fn node_span(&self, node: TreeNode) -> Option<ByteSpan> {
        match &self.nodes[node] {
            TreeNodeKind::Atomic(span, _) => Some(*span),
            TreeNodeKind::Value(span, _) => Some(*span),
            TreeNodeKind::Wildcard => None,
            TreeNodeKind::Tuple { span, .. } => Some(*span),
            TreeNodeKind::Cons { span, .. } => Some(*span),
            TreeNodeKind::Binary { span, .. } => Some(*span),
            TreeNodeKind::Map { span, .. } => Some(*span),
            TreeNodeKind::And { left, right } => {
                self.node_span(*left)
                    .or_else(|| self.node_span(*right))
            }
        }
    }

    fn rename(&mut self, from: TreeNode, to: TreeNode) {
        if from == to { return; }
        let len = self.binds[from].len();
        for n in 0..len {
            let ident = self.binds[from][n];
            self.binds[to].push(ident);
        }
    }

}

#[derive(Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) struct TreeNode(u32);
entity_impl!(TreeNode, "tree_node");

#[derive(Debug, Clone)]
pub(crate) enum TreeNodeKind {
    /// This must be an atomic value.
    /// Compound types are not allowed.
    Atomic(ByteSpan, Const),

    // Promoted to in a late pass.
    Value(ByteSpan, Either<IrValue, TreeNode>),

    Wildcard,
    Tuple {
        span: ByteSpan,
        elems: EntityList<TreeNode>
    },
    Cons {
        span: ByteSpan,
        head: TreeNode,
        tail: TreeNode,
    },
    Binary {
        span: ByteSpan,
        specifier: BinaryEntrySpecifier,
        size: Option<Either<Ident, IrValue>>,
        size_resolved: Option<Either<TreeNode, IrValue>>,
        value: TreeNode,
        tail: TreeNode,
    },
    Map {
        span: ByteSpan,
        entries: Vec<(IrValue, TreeNode)>,
    },

    And {
        left: TreeNode,
        right: TreeNode,
    },
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub(crate) enum ConstraintKind {
    Value(IrValue),
    Const(Const),
    PrimOp(PrimOp),
    Node(TreeNode),
}
impl ConstraintKind {
    fn constant(&self) -> Option<Const> {
        if let ConstraintKind::Const(cons) = self {
            Some(*cons)
        } else {
            None
        }
    }
    fn primop(&self) -> Option<PrimOp> {
        if let ConstraintKind::PrimOp(prim) = self {
            Some(*prim)
        } else {
            None
        }
    }
    fn value(&self) -> Option<IrValue> {
        if let ConstraintKind::Value(val) = self {
            Some(*val)
        } else {
            None
        }
    }
}
