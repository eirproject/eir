use meta_table::impl_cast_from;
use crate::{Function, Block, Value};

/// When an operation implements this trait, it has knowledge about some/all of
/// its outgoing control flow branches.
pub trait OpBranches {
    /// The number of branches this specific instance of the operation has.
    /// This may be dynamic, but needs to be solely decided by the operation
    /// itself.
    fn branches_len(&self) -> usize;

    /// Returns the target of `branch_n`.
    /// This may not be called with a `branch_n` >= `branch_len`.
    fn branch_num(&self, fun: &Function, block: Block, branch_n: usize) -> Value;
}
impl_cast_from!(OpBranches);
