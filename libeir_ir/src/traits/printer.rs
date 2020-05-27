use crate::{Block, DynValue};
use meta_table::impl_cast_from;
use pretty::RefDoc;

pub trait FormatOpCtx<'doc> {
    fn arena(&self) -> &'doc pretty::Arena<'doc>;
    fn value_use_to_doc(&mut self, value: DynValue) -> RefDoc<'doc, ()>;
}

pub trait OpPrinter {
    fn to_doc<'doc>(&self, ctx: &mut dyn FormatOpCtx<'doc>, block: Block) -> RefDoc<'doc, ()>;
}
impl_cast_from!(OpPrinter);
