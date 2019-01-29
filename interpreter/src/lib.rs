//! LIR interpreter with zero consideration of performance.
//! Made as an experiment to narrow down relevant implementation
//! details.

extern crate core_erlang_compiler;
use core_erlang_compiler::intern::Atom;
use core_erlang_compiler::ir::{ Module, FunctionIdent, SSAVariable };
use core_erlang_compiler::ir::lir::{ BasicBlock, LabelN, OpKind, Source };
use core_erlang_compiler::ir::hir::scope_tracker::LambdaEnvIdx;
use core_erlang_compiler::parser::AtomicLiteral;

extern crate lazy_static;

extern crate num_bigint;
extern crate num_traits;

extern crate serde;
extern crate serde_json;

mod term;
pub use term::{ TermType, Term, BoundLambdaEnv, Pid, Reference };
mod pattern;
use pattern::{ CaseContext };

pub mod erl_lib;
#[cfg(test)] pub mod erl_tests;

mod vm;
pub use vm::{ VMState, WatchType };

mod process;

mod module;

mod receive;

mod trace;

