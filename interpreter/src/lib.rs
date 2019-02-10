//! LIR interpreter with zero consideration of performance.
//! Made as an experiment to narrow down relevant implementation
//! details.

extern crate eir;
extern crate core_erlang_compiler;

use eir::Atom;
use eir::{ Module, FunctionIdent, SSAVariable, Source };
use eir::cfg::{ BasicBlock, LabelN };
use eir::op::{ OpKind };
use eir::LambdaEnvIdx;
use eir::AtomicTerm;

extern crate lazy_static;

extern crate num_bigint;
extern crate num_traits;

extern crate serde;
extern crate serde_json;

mod term;
pub use term::{ TermType, Term, BoundLambdaEnv, Pid, Reference };
mod pattern;

pub mod erl_lib;
#[cfg(test)] pub mod erl_tests;

mod vm;
pub use vm::{ VMState, WatchType };

mod process;

mod module;

mod receive;

mod trace;

