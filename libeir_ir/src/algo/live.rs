use std::collections::HashMap;

use crate::Function;
use crate::{Block, Value};

use cranelift_bforest::{BoundSet, Set, SetForest};
use libeir_util_datastructures::aux_traits::{AuxDebug, AuxImpl, HasAux};

impl Function {
    pub fn live_values(&self) -> LiveValues {
        calculate_live_values(self)
    }
}

/// # Value liveness calculation
/// Utility for calculating live values at every point in a functions
/// CFG.
///
/// TODO
/// Right now this uses a (probably) bad algorithm I made up on the spot.
/// I am sure there are better and more efficient ways of doing this,
/// but this should work for now.
/// For CFGs that are acyclic, this algorithm will complete in a single
/// iteration. For cyclic CFGs, this should take (around) 1 extra iteration
/// for every additional nested cycle.
#[derive(Clone)]
pub struct LiveValues {
    /// Values that need to exist at every block.
    /// Before block arguments.
    live_at: HashMap<Block, Set<Value>>,
    /// Values that need to exist within every block.
    /// After block arguments, before operation.
    live_in: HashMap<Block, Set<Value>>,
    /// The pool where `ebb_live` and `flow_live` is allocated.
    forest: SetForest<Value>,
}

impl HasAux<SetForest<Value>> for LiveValues {
    fn get_aux(&self) -> &SetForest<Value> {
        &self.forest
    }
}

impl<C: HasAux<SetForest<Value>>> AuxDebug<C> for LiveValues {
    fn aux_fmt(&self, f: &mut std::fmt::Formatter<'_>, _aux: &C) -> std::fmt::Result {
        let mut b = f.debug_struct("LiveValues");
        b.field("live_at", &AuxImpl(&self.live_at, self));
        b.field("live_in", &AuxImpl(&self.live_in, self));
        b.finish()
    }
}

impl std::fmt::Debug for LiveValues {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        self.aux_fmt(f, self)
    }
}

impl LiveValues {
    pub fn live_at<'a>(&'a self, block: Block) -> BoundSet<'a, Value, ()> {
        self.live_at[&block].bind(&self.forest, &())
    }
    pub fn live_in<'a>(&'a self, block: Block) -> BoundSet<'a, Value, ()> {
        self.live_in[&block].bind(&self.forest, &())
    }

    pub fn is_live_at(&self, block: Block, value: Value) -> bool {
        self.live_at[&block].contains(value, &self.forest, &())
    }
    pub fn is_live_in(&self, block: Block, value: Value) -> bool {
        self.live_in[&block].contains(value, &self.forest, &())
    }
}

fn dataflow_pass(
    fun: &Function,
    pool: &mut SetForest<Value>,
    live: &mut HashMap<Block, Set<Value>>,
    live_in: &mut HashMap<Block, Set<Value>>,
) -> bool {
    let graph = fun.block_graph();
    let mut visitor = graph.dfs_post_order();

    let mut stable = true;

    // For each Op node in the cfg
    while let Some(block) = visitor.next(&graph) {
        let mut set: Set<Value> = Set::new();

        // For each of the outgoing branches, add its live values to the current set
        for branch in graph.outgoing(block) {
            if let Some(vals) = live.get(&branch) {
                set.union_from(vals, pool, &());
            }
        }

        // Add the reads for the block OP to the current set
        for read in fun.block_reads(block) {
            // Only insert if it actually is a variable, not a block or constant
            fun.value_walk_nested_values::<_, ()>(*read, &mut |v| {
                if fun.value_argument(v).is_some() {
                    set.insert(v, pool, &());
                }
                Ok(())
            })
            .unwrap();
        }

        // Update the live_after values
        if !live_in.contains_key(&block) {
            live_in.insert(block, Set::new());
        }
        live_in.get_mut(&block).unwrap().union_from(&set, pool, &());

        // Remove the block arguments from the current set
        for arg in fun.block_args(block) {
            set.remove(*arg, pool, &());
        }

        // If we don't have a previous live calculation for this block, or if
        // the live set has changed, we are not yet stable.
        if let Some(mut old_set) = live.remove(&block) {
            if !old_set.iter(pool).eq(set.iter(pool)) {
                stable = false;
            }
            old_set.clear(pool);
        } else {
            stable = false;
        }

        // Insert the new calculated live variables
        live.insert(block, set);
    }

    stable
}

pub fn calculate_live_values(fun: &Function) -> LiveValues {
    let mut forest = SetForest::new();

    let mut live_at: HashMap<Block, Set<Value>> = HashMap::new();
    let mut live_in: HashMap<Block, Set<Value>> = HashMap::new();

    // Iterate dataflow until all dependencies have been resolved
    loop {
        let res = dataflow_pass(fun, &mut forest, &mut live_at, &mut live_in);
        if res {
            break;
        }
    }

    // Validate that the live set at entry is empty
    {
        let entry = fun.block_entry();
        assert!(
            live_at[&entry].iter(&forest).count() == 0,
            "{:?}",
            live_at[&entry].bind(&forest, &())
        );
    }

    LiveValues {
        forest,
        live_at,
        live_in,
    }
}

#[cfg(test)]
mod tests {

    #[test]
    fn test_simple() {
        let (ir, map) = crate::parse_function_map_unwrap(
            "
a'foo':a'bar'/1 {
    b1(%ret, %thr):
        b2();
    b2():
        b3();
    b3():
        %ret([]);
}
",
        );

        let b1 = map.get_block("b1");
        let b2 = map.get_block("b2");
        let b3 = map.get_block("b3");

        let b1_ret = map.get_value("ret");

        let live = ir.live_values();

        let b1_live = live.live_at(b1);
        assert!(b1_live.iter().count() == 0);

        let b2_live = live.live_at(b2);
        assert!(b2_live.iter().count() == 1);
        assert!(b2_live.contains(b1_ret));

        let b3_live = live.live_at(b3);
        assert!(b3_live.iter().count() == 1);
        assert!(b3_live.contains(b1_ret));
    }

    #[test]
    fn test_cycle() {
        let (ir, map) = crate::parse_function_map_unwrap(
            "
a'foo':a'bar'/1 {
    b1(%ret, %thr, %a):
        b2(%a, []);
    b2(%b, %c):
        b3();
    b3():
        b4();
    b4():
        b5(b6, %c);
    b5(%e, %f):
        b2(%e, %f);
    b6():
        %ret();
}
",
        );

        let b1 = map.get_block("b1");
        let b3 = map.get_block("b3");
        let b5 = map.get_block("b5");
        let b6 = map.get_block("b6");

        let b1_ret = map.get_value("ret");
        let b2_c = map.get_value("c");

        let live = ir.live_values();

        let b1_live = live.live_at(b1);
        assert!(b1_live.iter().count() == 0);

        let b3_live = live.live_at(b3);
        assert!(b3_live.iter().count() == 2);
        assert!(b3_live.contains(b1_ret));
        assert!(b3_live.contains(b2_c));

        let b5_live = live.live_at(b5);
        assert!(b5_live.iter().count() == 1);
        assert!(b5_live.contains(b1_ret));

        let b6_live = live.live_at(b6);
        assert!(b6_live.iter().count() == 1);
        assert!(b6_live.contains(b1_ret));
    }
}
