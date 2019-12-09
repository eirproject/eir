use crate::{Function, FunctionBuilder};
use crate::OpKind;

use super::{MangleValue, ToValue, MangleBlock};

/// Trait used to generalize a single mangling implementation over
/// both mangling within a single function container, and across
/// two different containers.
/// Not exposed to user, only used internally.
pub(super) trait MangleReceiver<'b> {
    // NOTE: Would like to do some things differently here, mainly
    // generalizing the either/from/to variants, but not having
    // GATs makes this way to painful.

    /// The function container that is the source of the mangle.
    fn from<'a>(&'a self) -> &'a Function;

    /// The function builder that is the destination of the mangle.
    fn to<'a>(&'a mut self) -> &'a mut FunctionBuilder<'b>;
    fn to_fun<'a>(&'a self) -> &'a Function;

    /// Maps a constant.
    /// This should return a value that is usable in the destination
    /// function.
    fn map_const(&mut self, val: MangleValue) -> ToValue;

    /// Maps a free value.
    /// A free value in this context is a value from outside
    /// the scope of the mangle.
    fn map_free_value(&mut self, val: MangleValue) -> ToValue;

    /// Maps a block operation. This should return an OpKind that is
    /// usable in the destination function.
    fn map_block_op(&mut self, block: MangleBlock) -> OpKind;

}

/// This receiver performs a mangle within a single function container.
pub(super) struct SingleMangleReceiver<'a, 'b> {
    pub fun: &'a mut FunctionBuilder<'b>,
}
impl<'b, 'c> MangleReceiver<'b> for SingleMangleReceiver<'c, 'b> {
    fn from<'a>(&'a self) -> &'a Function {
        // Since only the `To` branch should ever be used here,
        // this should never be called.
        unreachable!()
    }
    fn to<'a>(&'a mut self) -> &'a mut FunctionBuilder<'b> {
        self.fun
    }
    fn to_fun<'a>(&'a self) -> &'a Function {
        self.fun.fun()
    }
    fn map_const(&mut self, val: MangleValue) -> ToValue {
        val.to().unwrap()
    }
    fn map_free_value(&mut self, val: MangleValue) -> ToValue {
        val.to().unwrap()
    }
    fn map_block_op(&mut self, block: MangleBlock) -> OpKind {
        let block = block.to().unwrap().inner();
        self.fun.fun().block_kind(block).unwrap().clone()
    }
}

/// This receiver performs a mangle across to another function container.
/// In the basic case, this is simply a copy across function containers.
pub(super) struct CopyMangleReceiver<'a, 'b> {
    pub from: &'a Function,
    pub to: &'a mut FunctionBuilder<'b>,
}
impl<'b, 'c> MangleReceiver<'b> for CopyMangleReceiver<'c, 'b> {
    fn from<'a>(&'a self) -> &'a Function {
        self.from
    }
    fn to<'a>(&'a mut self) -> &'a mut FunctionBuilder<'b> {
        self.to
    }
    fn to_fun<'a>(&'a self) -> &'a Function {
        self.to.fun()
    }
    fn map_const(&mut self, _val: MangleValue) -> ToValue {
        unimplemented!()
    }
    fn map_free_value(&mut self, _val: MangleValue) -> ToValue {
        panic!()
    }
    fn map_block_op(&mut self, _block: MangleBlock) -> OpKind {
        unimplemented!()
    }
}
