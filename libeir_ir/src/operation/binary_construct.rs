//! # Binary construction construct
//! Binary construction is represented as several intrinsics that interact
//! to represent binary construction.
//!
//! Binary construction always consists of a single
//! `binary_construct_start`, any number of `binary_construct_push`,
//! finishing off with a single `binary_construct_finish`.
//!
//! `binary_construct_start` returns a opaque handle that is used to refer
//! to the binary construction in subsequent interactions.
//!
//! Once control flow enters through a `binary_construct_start`, it must
//! ALWAYS exit through a `binary_construct_finish`.
//!
//! Several binary constructions may happen concurrently, as long as they
//! are all eventually finished.
//!
//! A binary construction may contain arbitrary control flow, and as such
//! the number of pushes may vary from run to run.
//!
//! ```ignore
//!                  v
//!        binary_construct_start
//!                  |
//!                  v
//!         binary_construct_push
//!                  |
//!                 ...<-------------
//!                  v              |
//!         binary_construct_push   |
//!                  |              |
//!                 ...--------------
//!                  |
//!                  v
//!         binary_construct_push
//!                  |
//!                  v
//!        binary_construct_finish
//!
//! ````

use std::any::TypeId;
use std::default::Default;

use meta_table::{impl_meta_entry, MetaEntry};

use super::{DynOp, Op, OpBuild};
use crate::dialect::Dialect;
use crate::traits::OpBranches;
use crate::{BinaryEntrySpecifier, Block, Function, FunctionBuilder, Value};

pub struct BinaryConstructToken(());

/// ## `binary_construct_start`
/// (cont: fn(bin_ref))
#[derive(Debug, Clone)]
pub struct BinaryConstructStart;
impl_meta_entry!(BinaryConstructStart);

impl Op for BinaryConstructStart {
    fn name(&self) -> &str {
        "binary_construct_start"
    }
    fn dyn_clone(&self) -> DynOp {
        DynOp::new(self.clone())
    }
    fn type_id(&self) -> TypeId {
        TypeId::of::<Self>()
    }
    fn meta_entry(&self) -> &dyn MetaEntry {
        self
    }
}

impl OpBranches for BinaryConstructStart {
    fn branches_len(&self) -> usize {
        1
    }
    fn branch_num(&self, fun: &Function, block: Block, branch_n: usize) -> Value {
        match branch_n {
            0 => fun.block_reads(block)[0],
            _ => unreachable!(),
        }
    }
}

impl BinaryConstructStart {
    pub fn build(builder: &mut FunctionBuilder, block: Block) -> Block {
        let target = builder.block_insert();
        let _arg = builder.block_arg_insert(target);
        Self::build_target(builder, block, target);
        target
    }

    pub fn build_target(builder: &mut FunctionBuilder, block: Block, target: Block) {
        let target_val = builder.value(target);
        builder.op_intrinsic(
            block,
            BinaryConstructStart,
            &[target_val],
            BinaryConstructToken(()),
        );
    }
}
impl OpBuild for BinaryConstructStart {
    type Token = BinaryConstructToken;
}

/// ## `binary_construct_push`
/// (ok: fn(bin_ref), fail: fn(), bin_ref, value)
/// (ok: fn(bin_ref), fail: fn(), bin_ref, value, size)
#[derive(Debug, Default, Clone, PartialEq)]
pub struct BinaryConstructPush {
    pub specifier: BinaryEntrySpecifier,
}
impl_meta_entry!(BinaryConstructPush);

impl Op for BinaryConstructPush {
    fn name(&self) -> &str {
        "binary_construct_push"
    }
    fn dyn_clone(&self) -> DynOp {
        DynOp::new(self.clone())
    }
    fn type_id(&self) -> TypeId {
        TypeId::of::<Self>()
    }
    fn meta_entry(&self) -> &dyn MetaEntry {
        self
    }
    fn op_eq(&self, other: &dyn Op) -> bool {
        if let Some(other_i) = other.downcast_ref::<Self>() {
            self == other_i
        } else {
            false
        }
    }
}

impl OpBranches for BinaryConstructPush {
    fn branches_len(&self) -> usize {
        2
    }
    fn branch_num(&self, fun: &Function, block: Block, branch_n: usize) -> Value {
        match branch_n {
            0 => fun.block_reads(block)[0],
            1 => fun.block_reads(block)[1],
            _ => unreachable!(),
        }
    }
}

impl BinaryConstructPush {
    pub fn build(
        builder: &mut FunctionBuilder,
        block: Block,
        bin_ref: Value,
        value: Value,
        spec: BinaryEntrySpecifier,
        size: Option<Value>,
    ) -> (Block, Block) {
        let ok = builder.block_insert();
        let _bin_ref = builder.block_arg_insert(ok);
        let fail = builder.block_insert();
        Self::build_target(builder, block, bin_ref, value, spec, size, ok, fail);
        (ok, fail)
    }

    pub fn build_target(
        builder: &mut FunctionBuilder,
        block: Block,
        bin_ref: Value,
        value: Value,
        spec: BinaryEntrySpecifier,
        size: Option<Value>,
        ok: Block,
        fail: Block,
    ) {
        let ok_val = builder.value(ok);
        let fail_val = builder.value(fail);

        if let Some(size) = size {
            builder.op_intrinsic(
                block,
                BinaryConstructPush { specifier: spec },
                &[ok_val, fail_val, bin_ref, value, size],
                BinaryConstructToken(()),
            );
        } else {
            builder.op_intrinsic(
                block,
                BinaryConstructPush { specifier: spec },
                &[ok_val, fail_val, bin_ref, value],
                BinaryConstructToken(()),
            );
        };
    }
}
impl OpBuild for BinaryConstructPush {
    type Token = BinaryConstructToken;
}

/// ## `binary_construct_finish`
/// (cont: fn(result), ref)
#[derive(Debug, Clone)]
pub struct BinaryConstructFinish;
impl_meta_entry!(BinaryConstructFinish);

impl Op for BinaryConstructFinish {
    fn name(&self) -> &str {
        "binary_construct_finish"
    }
    fn dyn_clone(&self) -> DynOp {
        DynOp::new(self.clone())
    }
    fn type_id(&self) -> TypeId {
        TypeId::of::<Self>()
    }
    fn meta_entry(&self) -> &dyn MetaEntry {
        self
    }
}

impl OpBranches for BinaryConstructFinish {
    fn branches_len(&self) -> usize {
        1
    }
    fn branch_num(&self, fun: &Function, block: Block, branch_n: usize) -> Value {
        match branch_n {
            0 => fun.block_reads(block)[0],
            _ => unreachable!(),
        }
    }
}

impl BinaryConstructFinish {
    pub fn build(builder: &mut FunctionBuilder, block: Block, bin_ref: Value) -> Block {
        let target = builder.block_insert();
        let _arg = builder.block_arg_insert(target);
        Self::build_target(builder, block, bin_ref, target);
        target
    }

    pub fn build_target(
        builder: &mut FunctionBuilder,
        block: Block,
        bin_ref: Value,
        target: Block,
    ) {
        let target_val = builder.value(target);
        builder.op_intrinsic(
            block,
            BinaryConstructFinish,
            &[target_val, bin_ref],
            BinaryConstructToken(()),
        );
    }
}
impl OpBuild for BinaryConstructFinish {
    type Token = BinaryConstructToken;
}

pub fn register(dialect: &mut Dialect) {
    dialect.register_op::<BinaryConstructStart>();
    dialect.register_op_branches_impl(&BinaryConstructStart);

    dialect.register_op::<BinaryConstructPush>();
    dialect.register_op_branches_impl(&BinaryConstructPush::default());

    dialect.register_op::<BinaryConstructFinish>();
    dialect.register_op_branches_impl(&BinaryConstructFinish);
}
