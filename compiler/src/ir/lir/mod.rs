use ::ssa::SSAVariable;

pub mod from_hir;
//pub mod to_dot;
pub mod pass;

//pub use ::eir::cfg::{ FunctionCfg, FunctionCfgBuilder, BasicBlock, LabelN };

#[derive(Debug, Clone, Copy)]
pub struct Label(u32);
