use std::collections::{BTreeSet, BTreeMap};

use libeir_ir::{Function, Block, Value};
use libeir_ir::{OpKind, CallKind};
use libeir_ir::{LiveValues, FunctionTree};

use petgraph::visit::IntoNeighbors;

#[cfg(test)]
mod tests;

#[derive(Debug, Clone)]
pub struct LowerData {
    /// The live value set is generated as part of the analysis.
    /// It is returned so this calculation doesn't need to happen several
    /// times.
    pub live: LiveValues,

    pub func_tree: FunctionTree,
}

enum Escape {
    Return,
    Throw,
}

pub fn analyze(fun: &Function) -> LowerData {
    let live = fun.live_values();
    let func_tree = fun.func_tree(&live, true);

    LowerData {
        live,
        func_tree,
    }
}
