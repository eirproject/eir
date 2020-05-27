//! # Receive construct
//! A receive statement is represented as several intrinsics that
//! interact to represent the receive operation semantics of erlang.
//!
//! It consists of 3 operations, `receive_start`, `receive_wait`
//! and `receive_done`. They are always called in this pattern:
//!
//! ```ignore
//!          v
//!     receive_start
//!          |
//!          v
//!     receive_wait <------------
//!     |          |             |
//!  timeout    check_msg   no match on
//!     |          |          message
//!     v          v             |
//!          control flow to  ----
//!          match on message
//!                |
//!          message matches
//!                |
//!                v
//!            receive_done
//!                |
//! ```

use std::any::TypeId;

use meta_table::{impl_meta_entry, MetaEntry};

use super::{DynOp, Op, OpBuild};
use crate::dialect::Dialect;
use crate::traits::OpBranches;
use crate::{Block, Function, FunctionBuilder, Value};

pub struct ReceiveToken(());

/// ## `receive_start`
/// (cont: fn(recv_ref), timeout)
///
/// `recv_ref` is an opaque value that represents the current
/// receive operation. It is up to the runtime implementor
/// to decide what this stores, if anything at all.
/// Since receive constructs can never be nested, storing receive
/// state globally is also a valid strategy.
/// This value can only ever be passed to `receive_wait` or
/// `receive_done`.
///
/// `timeout` is either an atom, `infinity`, or a number.
#[derive(Debug, Clone)]
pub struct ReceiveStart;
impl_meta_entry!(ReceiveStart);

impl Op for ReceiveStart {
    fn name(&self) -> &str {
        "receive_start"
    }
    fn dyn_clone(&self) -> DynOp {
        DynOp::new(self.clone())
    }
    fn type_id(&self) -> TypeId {
        TypeId::of::<ReceiveStart>()
    }
    fn meta_entry(&self) -> &dyn MetaEntry {
        self
    }
    fn op_eq(&self, other: &dyn Op) -> bool {
        self.type_id() == other.type_id()
    }
}

impl OpBranches for ReceiveStart {
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

impl ReceiveStart {
    pub fn build(builder: &mut FunctionBuilder, block: Block, timeout: Value) -> Block {
        let target = builder.block_insert();
        let _arg = builder.block_arg_insert(target);
        Self::build_target(builder, block, timeout, target);
        target
    }

    pub fn build_target(
        builder: &mut FunctionBuilder,
        block: Block,
        timeout: Value,
        target: Block,
    ) {
        let target_val = builder.value(target);
        builder.op_intrinsic(
            block,
            ReceiveStart,
            &[target_val, timeout],
            ReceiveToken(()),
        );
    }
}
impl OpBuild for ReceiveStart {
    type Token = ReceiveToken;
}

/// ## `receive_wait`
/// (timeout: fn(), check_message: fn(msg), recv_ref)
///
/// This increments the mailbox pointer, fetches it, and calls
/// `check_message` with that message. The value passed to
/// `check_message` can not escape the current receive construct
/// without being mapped through `receive_done`, so it is safe for
/// this to be an off-heap value as long as it's guaranteed to be
/// alive until the receive construct is exited by `receive_done`.
///
/// If there are no more messages, this should yield until there is.
///
/// If there is a timeout, `timeout` should be called. When `timeout`
/// is called, the receive construct should be considered finished,
/// that is, `receive_done` should not be called.
#[derive(Debug, Clone)]
pub struct ReceiveWait;
impl_meta_entry!(ReceiveWait);

impl Op for ReceiveWait {
    fn name(&self) -> &str {
        "receive_wait"
    }
    fn dyn_clone(&self) -> DynOp {
        DynOp::new(self.clone())
    }
    fn type_id(&self) -> TypeId {
        TypeId::of::<ReceiveStart>()
    }
    fn meta_entry(&self) -> &dyn MetaEntry {
        self
    }
    fn op_eq(&self, other: &dyn Op) -> bool {
        self.type_id() == other.type_id()
    }
}

impl OpBranches for ReceiveWait {
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

impl ReceiveWait {
    pub fn build(builder: &mut FunctionBuilder, block: Block, recv_ref: Value) -> (Block, Block) {
        let timeout = builder.block_insert();

        let check_message = builder.block_insert();
        let _message = builder.block_arg_insert(check_message);

        Self::build_target(builder, block, recv_ref, timeout, check_message);

        (timeout, check_message)
    }

    pub fn build_target(
        builder: &mut FunctionBuilder,
        block: Block,
        recv_ref: Value,
        timeout: Block,
        check_message: Block,
    ) {
        let timeout_val = builder.value(timeout);
        let check_message_val = builder.value(check_message);
        builder.op_intrinsic(
            block,
            ReceiveWait,
            &[timeout_val, check_message_val, recv_ref],
            ReceiveToken(()),
        );
    }
}
impl OpBuild for ReceiveWait {
    type Token = ReceiveToken;
}

/// ## `receive_done`
/// (next: fn(...), recv_ref, ...)
///
/// Called when the message under the mailbox pointer is successfully
/// matched, and should be removed from the mailbox.
///
/// Called with an arbitrary amount of arguments, these are the values
/// that have been extracted from that message. If these are off-heap
/// references, this operation would presumably copy them to the current
/// process heap, and ensure that they are under juristiction of the
/// process GC. The potentially copied values are then passed on as
/// arguments to `next`.
///
/// After this operation is completed, there will be no live references
/// to the value originally passed to `check_message`, they will have
/// all been mapped through `receive_done`.
#[derive(Debug, Clone)]
pub struct ReceiveDone;
impl_meta_entry!(ReceiveDone);

impl Op for ReceiveDone {
    fn name(&self) -> &str {
        "receive_done"
    }
    fn dyn_clone(&self) -> DynOp {
        DynOp::new(self.clone())
    }
    fn type_id(&self) -> TypeId {
        TypeId::of::<ReceiveStart>()
    }
    fn meta_entry(&self) -> &dyn MetaEntry {
        self
    }
    fn op_eq(&self, other: &dyn Op) -> bool {
        self.type_id() == other.type_id()
    }
}

impl OpBranches for ReceiveDone {
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

impl ReceiveDone {
    pub fn build(
        builder: &mut FunctionBuilder,
        block: Block,
        recv_ref: Value,
        values: &[Value],
    ) -> Block {
        let next = builder.block_insert();
        for _ in values.iter() {
            builder.block_arg_insert(next);
        }

        Self::build_target(builder, block, recv_ref, values, next);

        next
    }

    pub fn build_target(
        builder: &mut FunctionBuilder,
        block: Block,
        recv_ref: Value,
        values: &[Value],
        next: Block,
    ) {
        let next_val = builder.value(next);

        let mut tmp = Vec::with_capacity(values.len() + 1);
        tmp.push(next_val);
        tmp.push(recv_ref);
        tmp.extend(values.iter().cloned());

        builder.op_intrinsic(block, ReceiveDone, &tmp, ReceiveToken(()));
    }
}
impl OpBuild for ReceiveDone {
    type Token = ReceiveToken;
}

pub fn register(dialect: &mut Dialect) {
    dialect.register_op::<ReceiveStart>();
    dialect.register_op_branches_impl(&ReceiveStart);

    dialect.register_op::<ReceiveWait>();
    dialect.register_op_branches_impl(&ReceiveWait);

    dialect.register_op::<ReceiveDone>();
    dialect.register_op_branches_impl(&ReceiveDone);
}
