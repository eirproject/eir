use crate::text::ast::DynToken;
use crate::text::LowerContext;
use crate::Block;

pub trait OpParser: Send + Sync {
    fn parse(
        &self,
        context: &mut LowerContext,
        block: Block,
        tokens: &[DynToken],
    ) -> Result<(), ()>;
}
