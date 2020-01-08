use std::collections::{BTreeSet, BTreeMap};

use libeir_ir::{Function, Block, Value};
use libeir_ir::{OpKind, CallKind};
use libeir_ir::LiveValues;

use petgraph::visit::IntoNeighbors;

#[cfg(test)]
mod tests;

#[derive(Debug, Clone)]
pub struct LowerData {
    /// The live value set is generated as part of the analysis.
    /// It is returned so this calculation doesn't need to happen several
    /// times.
    pub live: LiveValues,

    /// The descriptions of the different functions contained within this
    /// container.
    /// This is guaranteed to contain at least one entry, the root function.
    /// The root block itself can be obtained by `Function::block_entry`.
    pub functions: BTreeMap<Block, FunctionData>,
}

#[derive(Debug, Clone)]
pub struct FunctionData {
    /// The entry point of the function.
    /// The continuations are always the first arguments of this block.
    /// * If both `ret` and `thr` are `Some`, the first two are those
    ///   continuations respectively.
    /// * If only one of `ret` and `thr` are `Some`, only the first argument
    ///   is that continuation.
    /// * If both are `None`, this function never returns.
    /// At the moment both will always be `Some`, but this may change in the
    /// future.
    pub entry: Block,

    /// The return continuation for this function.
    /// This value is always either:
    /// * Called with a `CallKind::ControlFlow` operation, this is a return.
    /// * Passed as argument 1 to a `CallKind::Function`, this is a tail call.
    pub ret: Option<Value>,
    /// The throw continuation for this function.
    /// This value is always either:
    /// * Called with a `CallKind::ControlFlow` operation, this is a throw.
    /// * Passed as argument 2 to a `CallKind::Function`, this is a tail call.
    pub thr: Option<Value>,

    /// This is the scope of the function.
    /// This includes all blocks that can be reached by control flow within
    /// the current stack frame.
    /// Does not include calls to functions, those create a new stack frame.
    pub scope: BTreeSet<Block>,
}

enum Escape {
    Return,
    Throw,
}

pub fn analyze(fun: &Function) -> LowerData {
    let live = fun.live_values();
    let graph = fun.block_graph();

    // Identify which blocks get captured.
    // At at least one location in the IR, these are not branched to directly
    // by an operation, but instead captured as a value.
    // For a capture to be valid, it needs to only return through one of its
    // arguments.
    let mut blocks = BTreeSet::new();

    let mut captured_blocks = BTreeSet::new();
    captured_blocks.insert(fun.block_entry());
    {
        let mut branched = BTreeSet::new();
        for block in graph.dfs_iter() {
            blocks.insert(block);

            branched.clear();
            branched.extend(fun.op_branch_iter(block));

            fun.block_walk_nested_values::<_, Result<(), ()>>(block, &mut |v| {
                if let Some(v_block) = fun.value_block(v) {
                    if !branched.contains(&v) {
                        captured_blocks.insert(v_block);
                    }
                }
                Ok(())
            }).unwrap();
        }
    }

    // Find all escape continuations for all captures
    let mut escapes = BTreeMap::new();
    for block in captured_blocks.iter() {
        let args = fun.block_args(*block);
        assert!(args.len() >= 2);
        escapes.insert(args[0], Escape::Return);
        escapes.insert(args[1], Escape::Throw);
    }

    // Validate that at every point only the escape continuations for the
    // current functions are live.
    for block in captured_blocks.iter() {
        let live_at = live.live_at(*block);
        for live_val in live_at.iter() {
            assert!(!escapes.contains_key(&live_val));
        }
    }

    // Validate that escapes are only either:
    // * Called as control flow
    // * Passed as an escape to a function call
    // More specifically, escapes should NEVER be:
    // * Passed as a regular argument to a function call
    // * Passed as an argument to a control flow call
    for (value, escape) in escapes.iter() {
        for usage in fun.value_usages(*value).iter() {
            if !blocks.contains(&usage) {
                continue;
            }

            let check: &dyn Fn(&[Value]) = &|values| {
                for read in values.iter() {
                    fun.value_walk_nested_values::<_, Result<(), ()>>(*read, &mut |v| {
                        assert!(v != *value);
                        Ok(())
                    }).unwrap();
                }
            };

            let reads = fun.block_reads(usage);
            match fun.block_kind(usage).unwrap() {
                OpKind::Call(CallKind::ControlFlow) => {
                    assert!(reads[0] == *value);
                    check(&reads[1..]);
                }
                OpKind::Call(CallKind::Function) => {
                    check(&[reads[0]]);
                    match *escape {
                        Escape::Return => {
                            assert!(reads[1] == *value);
                            check(&[reads[2]]);
                        }
                        Escape::Throw => {
                            check(&[reads[1]]);
                            assert!(reads[2] == *value);
                        }
                    }
                    check(&reads[3..]);
                }
                _ => (),
            }
        }
    }

    // Generate the scopes for all of the functions
    let mut functions = BTreeMap::new();
    let mut cfg_graph = fun.control_flow_graph();
    for block in captured_blocks.iter() {
        cfg_graph.calculate(fun, fun.block_value(*block));

        let mut function_scope = BTreeSet::new();
        let mut to_walk = Vec::new();
        to_walk.push(*block);

        while let Some(block) = to_walk.pop() {
            if function_scope.contains(&block) {
                continue;
            }
            function_scope.insert(block);

            let block_val = fun.block_value(block);

            for out in cfg_graph.neighbors(block_val) {
                if let Some(out_block) = fun.value_block(out) {
                    if !captured_blocks.contains(&out_block) {
                        to_walk.push(out_block);
                    }
                }
            }
        }

        let args = fun.block_args(*block);

        functions.insert(*block, FunctionData {
            entry: *block,

            ret: Some(args[0]),
            thr: Some(args[1]),

            scope: function_scope,
        });
    }

    LowerData {
        live,
        functions,
    }
}
