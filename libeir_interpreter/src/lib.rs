//! LIR interpreter with zero consideration of performance.
//! Made as an experiment to narrow down relevant implementation
//! details.

mod term;
pub use term::{ TermType, Term, Pid, Reference, ErlEq, ErlExactEq, ErlOrd };
//mod pattern;

pub mod erl_lib;
//#[cfg(test)] pub mod erl_tests;

mod vm;
pub use vm::{ VMState, WatchType };

mod process;

mod module;

//mod receive;

mod trace;

//mod mailbox;
