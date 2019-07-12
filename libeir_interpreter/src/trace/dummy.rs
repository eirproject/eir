use std::rc::Rc;
use std::collections::HashMap;

use crate::term::{ Term, Pid };

use libeir_ir::{ FunctionIdent, Block };

pub fn set_pid(pid: Pid) {}
pub fn enter_function(ident: &FunctionIdent, lambda: Option<Block>, args: &[Rc<Term>]) {}
//pub fn exit_function(ident: &FunctionIdent, ret: Option<&CallReturn>) {}
//pub fn start_basic_block(module: &Atom, ident: &FunctionIdent, block: LabelN) {
//pub fn end_basic_block() {
pub fn warning(text: String) {}
pub fn warning_args<F>(text: String, make_args: F) where F: FnOnce() -> HashMap<String, ::serde_json::Value> {}
