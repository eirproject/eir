use std::collections::{BTreeMap, BTreeSet};

use petgraph::visit::{Dfs, DfsPostOrder};
use petgraph::visit::{GraphBase, IntoNeighbors, Visitable, Walker};

use crate::graph::EntityVisitMap;
use crate::{Block, CallKind, Function, LiveValues, OpKind, Value};

#[derive(Debug, Clone)]
pub struct FunctionTree {
    /// The root function, specified by the entry block of the function
    /// container.
    pub root_fun: Block,

    /// The descriptions of the different functions contained within this
    /// container.
    /// This is guaranteed to contain at least one entry, the root function.
    pub functions: BTreeMap<Block, FunctionEntry>,
}

#[derive(Debug, Clone)]
pub struct FunctionEntry {
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

    /// The children function entries of this function.
    pub children: BTreeSet<Block>,
}

enum Escape {
    Return,
    Throw,
}

impl Function {
    pub fn func_tree(&self, live: &LiveValues, resolve_continuations: bool) -> FunctionTree {
        let graph = self.block_graph();

        // Identify which blocks get captured.
        // At at least one location in the IR, these are not branched to directly
        // by an operation, but instead captured as a value.
        // For a capture to be valid, it needs to only return through one of its
        // arguments.
        let mut blocks = BTreeSet::new();

        let mut captured_blocks = BTreeSet::new();
        captured_blocks.insert(self.block_entry());
        {
            let mut branched = BTreeSet::new();
            for block in graph.dfs_iter() {
                blocks.insert(block);

                branched.clear();
                branched.extend(self.op_branch_iter(block));

                self.block_walk_nested_values::<_, Result<(), ()>>(block, &mut |v| {
                    if let Some(v_block) = self.value_block(v) {
                        if !branched.contains(&v) {
                            captured_blocks.insert(v_block);
                        }
                    }
                    Ok(())
                })
                .unwrap();
            }
        }

        if resolve_continuations {
            // Find all escape continuations for all captures
            let mut escapes = BTreeMap::new();
            for block in captured_blocks.iter() {
                let args = self.block_args(*block);
                assert!(args.len() >= 2);
                escapes.insert(args[0], Escape::Return);
                escapes.insert(args[1], Escape::Throw);
            }

            // Validate that at every point only the escape continuations for
            // the current functions are live.
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
                for usage in self.value_usages(*value).iter() {
                    if !blocks.contains(&usage) {
                        continue;
                    }

                    let check: &dyn Fn(&[Value]) = &|values| {
                        for read in values.iter() {
                            self.value_walk_nested_values::<_, Result<(), ()>>(*read, &mut |v| {
                                assert!(v != *value);
                                Ok(())
                            })
                            .unwrap();
                        }
                    };

                    let reads = self.block_reads(usage);
                    match self.block_kind(usage).unwrap() {
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
        }

        // Generate the scopes for all of the functions
        let mut functions = BTreeMap::new();
        let mut cfg_graph = self.control_flow_graph();
        for block in captured_blocks.iter() {
            cfg_graph.calculate(self, self.block_value(*block));

            let mut function_scope = BTreeSet::new();
            let mut to_walk = Vec::new();
            to_walk.push(*block);

            while let Some(block) = to_walk.pop() {
                if function_scope.contains(&block) {
                    continue;
                }
                function_scope.insert(block);

                let block_val = self.block_value(block);

                for out in cfg_graph.neighbors(block_val) {
                    if let Some(out_block) = self.value_block(out) {
                        if !captured_blocks.contains(&out_block) {
                            to_walk.push(out_block);
                        }
                    }
                }
            }

            let args = self.block_args(*block);

            let mut children = function_scope
                .intersection(&captured_blocks)
                .cloned()
                .collect::<BTreeSet<_>>();
            children.remove(block);

            let mut ret = None;
            let mut thr = None;
            if resolve_continuations {
                ret = Some(args[0]);
                thr = Some(args[1]);
            }

            functions.insert(
                *block,
                FunctionEntry {
                    entry: *block,

                    ret,
                    thr,

                    children,

                    scope: function_scope,
                },
            );
        }

        let tree = FunctionTree {
            root_fun: self.block_entry(),
            functions,
        };

        assert!(tree
            .validate_no_cycles(tree.root_fun, 0, tree.functions.len())
            .is_some());

        tree
    }
}

impl FunctionTree {
    fn validate_no_cycles(&self, entry: Block, mut curr: usize, limit: usize) -> Option<usize> {
        if curr > limit {
            return None;
        }
        for child in self.functions[&entry].children.iter() {
            curr = self.validate_no_cycles(*child, curr + 1, limit)?;
        }
        Some(curr)
    }

    pub fn dfs(&self) -> Dfs<Block, EntityVisitMap<Block>> {
        Dfs::new(self, self.root_fun)
    }
    pub fn dfs_iter<'a>(&'a self) -> impl Iterator<Item = Block> + 'a {
        self.dfs().iter(self)
    }

    pub fn dfs_post_order(&self) -> DfsPostOrder<Block, EntityVisitMap<Block>> {
        DfsPostOrder::new(self, self.root_fun)
    }
    pub fn dfs_post_order_iter<'a>(&'a self) -> impl Iterator<Item = Block> + 'a {
        self.dfs_post_order().iter(self)
    }
}

impl GraphBase for FunctionTree {
    type NodeId = Block;
    type EdgeId = (Block, Block);
}

impl<'a> IntoNeighbors for &'a FunctionTree {
    type Neighbors = std::iter::Cloned<std::collections::btree_set::Iter<'a, Block>>;
    fn neighbors(self, block: Block) -> Self::Neighbors {
        self.functions[&block].children.iter().cloned()
    }
}

impl Visitable for FunctionTree {
    type Map = EntityVisitMap<Block>;
    fn visit_map(&self) -> EntityVisitMap<Block> {
        EntityVisitMap::new(self.functions.len())
    }
    fn reset_map(&self, map: &mut EntityVisitMap<Block>) {
        map.reset(self.functions.len());
    }
}
