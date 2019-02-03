use super::SSAVariable;
use ::ir::hir::Pattern;
use ::Atom;

pub mod from_hir;
pub mod to_dot;
pub mod pass;

use ::eir::LambdaEnvIdx;
pub use ::eir::cfg::{ FunctionCfg, FunctionCfgBuilder, BasicBlock, LabelN };

#[derive(Debug, Clone, Copy)]
pub struct Label(u32);
