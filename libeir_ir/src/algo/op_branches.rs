use crate::Function;
use crate::{Block, CallKind, OpKind, Value};

pub struct BranchIter<'a> {
    fun: &'a Function,
    block: Block,
    num: usize,
    len: usize,
}
impl<'a> Iterator for BranchIter<'a> {
    type Item = Value;
    fn next(&mut self) -> Option<Value> {
        if self.num == self.len {
            None
        } else {
            let res = self.fun.op_branch_target(self.block, self.num);
            self.num += 1;
            Some(res)
        }
    }
}

/// These functions provide knowledge about what branches an operation can
/// perform at specific locations in the IR.
///
/// This is different from the information you get from the block graph in
/// several ways:
/// * If one operation has several branches to the same destination block,
///   the block graph tells us nothing about that.
/// * The block graph can contain an outgoing edge to a block that is not
///   truly a direct call, but instead a function capture, this looks only
///   at direct branches.
/// * Likewise, the block graph will not have edges for calls to values,
///   this does.
/// * The block graph gives no information about the expected arity of the
///   destination, and has no information about what those arguments are.
impl Function {
    pub fn op_branch_iter<'a>(&'a self, block: Block) -> BranchIter<'a> {
        BranchIter {
            fun: self,
            block,
            num: 0,
            len: self.op_branch_len(block).unwrap(),
        }
    }

    pub fn op_branch_len(&self, block: Block) -> Option<usize> {
        let res = match self.block_kind(block)? {
            OpKind::Call(CallKind::ControlFlow) => 1,
            OpKind::Call(CallKind::Function) => 2,
            OpKind::Unreachable => 0,
            OpKind::IfBool => {
                let reads = self.block_reads(block);
                match reads.len() {
                    3 => 2,
                    4 => 3,
                    _ => unreachable!(),
                }
            }
            OpKind::Match { branches } => branches.len(),
            OpKind::TraceCaptureRaw => 1,
            OpKind::TraceConstruct => 1,
            OpKind::UnpackValueList(_) => 1,
            OpKind::MapPut { .. } => 2,
            OpKind::Dyn(dyn_op) => {
                let op_branches = self.dialect().get_op_branches(&**dyn_op).unwrap();
                op_branches.branches_len()
            } //OpKind::Intrinsic(name) => {
              //    match name.as_str().get() {
              //        "receive_start" => 1,
              //        "receive_wait" => 2,
              //        "receive_done" => 1,
              //        _ => unimplemented!(),
              //    }
              //}
        };

        Some(res)
    }

    pub fn op_branch_target(&self, block: Block, n: usize) -> Value {
        let reads = self.block_reads(block);
        match (self.block_kind(block).unwrap(), reads.len(), n) {
            // For a control flow call, the only control flow branch
            // is the target.
            (OpKind::Call(CallKind::ControlFlow), _, 0) => reads[0],

            // For a function call, the only control flow branches are
            // the return and escape continuations.
            (OpKind::Call(CallKind::Function), _, 0) => reads[1],
            (OpKind::Call(CallKind::Function), _, 1) => reads[2],

            (OpKind::IfBool, 3, n) if n < 2 => reads[n],
            (OpKind::IfBool, 4, n) if n < 3 => reads[n],

            (OpKind::TraceCaptureRaw, _, 0) => reads[0],
            (OpKind::TraceConstruct, _, 0) => reads[0],
            (OpKind::UnpackValueList(_), _, 0) => reads[0],
            (OpKind::MapPut { .. }, _, n) if n < 2 => reads[n],

            (OpKind::Match { .. }, _, n) => self.value_list_get_n(reads[0], n).unwrap(),

            (OpKind::Dyn(_dyn), _, _) => unimplemented!(),
            //(OpKind::Intrinsic(name), _, n) => {
            //    match name.as_str().get() {
            //        // receive_start only has a single branch target
            //        "receive_start" if n == 0 => reads[0],
            //        // receive_wait only has two branch targets
            //        "receive_wait" if n < 2 => reads[n],
            //        // receive_done only has a single branch target
            //        "receive_done" if n == 0 => reads[0],
            //        _ => unimplemented!(),
            //    }
            //}
            _ => panic!(),
        }
    }
}
