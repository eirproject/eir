use std::collections::HashMap;

use crate::{ Function };
use crate::{ Block, Value };

use libeir_util::pooled_entity_set::{ EntitySetPool, PooledEntitySet };

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

#[derive(Debug)]
pub struct LiveValues {
    /// Values that need to exist at every block
    pub live: HashMap<Block, PooledEntitySet<Value>>,
    /// The pool where `ebb_live` and `flow_live` is allocated.
    pub pool: EntitySetPool,
}

fn dataflow_pass(
    fun: &Function,
    pool: &mut EntitySetPool,
    live: &mut HashMap<Block, PooledEntitySet<Value>>,
) -> bool {

    let graph = fun.block_graph();
    let mut visitor = graph.dfs_post_order();

    let mut stable = true;

    // For each Op node in the cfg
    'outer: while let Some(block) = visitor.next(&graph) {

        let mut set: PooledEntitySet<Value> = PooledEntitySet::new();

        // For each of the outgoing branches, add its live values to the current set
        for branch in graph.outgoing(block) {
            if let Some(vals) = live.get(&branch) {
                set.union(vals, pool);
            }
        }

        // Add the reads for the block OP to the current set
        for read in fun.block_reads(block) {
            // Only insert if it actually is a variable, not a block or constant
            if fun.value_arg_definition(*read).is_some() {
                set.insert(*read, pool);
            }
        }

        // Remove the block arguments from the current set
        for arg in fun.block_args(block) {
            set.remove(*arg, pool);
        }

        // If we don't have a previous live calculation for this block, or if
        // the live set has changed, we are not yet stable.
        if let Some(mut old_set) = live.remove(&block) {
            if !old_set.eq(&set, pool) {
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
    let mut pool: EntitySetPool = EntitySetPool::new();

    let mut live: HashMap<Block, PooledEntitySet<Value>> = HashMap::new();

    // Iterate dataflow until all dependencies have been resolved
    loop {
        let res = dataflow_pass(
            fun,
            &mut pool,
            &mut live,
        );
        if res { break; }
    }

    // Validate that the live set at entry is empty
    {
        let entry = fun.block_entry();
        assert!(live[&entry].size(&pool) == 0);
    }

    LiveValues {
        pool: pool,
        live: live,
    }
}

#[cfg(test)]
mod tests {
    use crate::{ FunctionIdent, Function, FunctionBuilder };
    use crate::NilTerm;
    use libeir_intern::Ident;

    #[test]
    fn test_simple() {

        // b1(ret):
        //     b2()
        // b2():
        //     b3()
        // b3():
        //     ret()

        let ident = FunctionIdent {
            module: Ident::from_str("woo"),
            name: Ident::from_str("woo"),
            arity: 1,
        };
        let mut fun = Function::new(ident);
        let mut b = FunctionBuilder::new(&mut fun);

        let b1 = b.block_insert();
        b.block_set_entry(b1);
        let b1_ret = b.block_arg_insert(b1);
        let _b1_nonused = b.block_arg_insert(b1);
        let b2 = b.block_insert();
        let b3 = b.block_insert();

        b.op_call(b1, b2, &[]);
        b.op_call(b2, b3, &[]);
        b.op_call(b3, b1_ret, &[]);

        let live = b.fun().live_values();

        let b1_live = &live.live[&b1];
        assert!(b1_live.size(&live.pool) == 0);

        let b2_live = &live.live[&b2];
        assert!(b2_live.size(&live.pool) == 1);
        assert!(b2_live.contains(b1_ret, &live.pool));

        let b3_live = &live.live[&b3];
        assert!(b3_live.size(&live.pool) == 1);
        assert!(b3_live.contains(b1_ret, &live.pool));
    }

    #[test]
    fn test_cycle() {

        // b1(ret, a):
        //     b2(a, [])
        // b2(b, c):
        //     b3()
        // b3():
        //     b4()
        // b4():
        //     b5(b6, c)
        // b5(e, f):
        //     b2(e, f)
        // b6():
        //     ret()

        let ident = FunctionIdent {
            module: Ident::from_str("woo"),
            name: Ident::from_str("woo"),
            arity: 1,
        };
        let mut fun = Function::new(ident);
        let mut b = FunctionBuilder::new(&mut fun);

        let b1 = b.block_insert();
        b.block_set_entry(b1);
        let b1_ret = b.block_arg_insert(b1);
        let b1_a = b.block_arg_insert(b1);

        let b2 = b.block_insert();
        let b2_b = b.block_arg_insert(b2);
        let b2_c = b.block_arg_insert(b2);

        let b3 = b.block_insert();

        let b4 = b.block_insert();

        let b5 = b.block_insert();
        let b5_e = b.block_arg_insert(b5);
        let b5_f = b.block_arg_insert(b5);

        let b6 = b.block_insert();
        let b6_val = b.value(b6);

        let nil_const = b.value(NilTerm);
        b.op_call(b1, b2, &[b1_a, nil_const]);
        b.op_call(b2, b3, &[]);
        b.op_call(b3, b4, &[]);
        b.op_call(b4, b5, &[b6_val, b2_c]);
        b.op_call(b5, b2, &[b5_e, b5_f]);
        b.op_call(b6, b1_ret, &[]);

        println!("{}", b.fun().to_text());

        let live = b.fun().live_values();

        let b1_live = &live.live[&b1];
        assert!(b1_live.size(&live.pool) == 0);

        let b3_live = &live.live[&b3];
        assert!(b3_live.size(&live.pool) == 2);
        assert!(b3_live.contains(b1_ret, &live.pool));
        assert!(b3_live.contains(b2_c, &live.pool));

        let b5_live = &live.live[&b5];
        assert!(b5_live.size(&live.pool) == 1);
        assert!(b5_live.contains(b1_ret, &live.pool));

        let b6_live = &live.live[&b6];
        assert!(b6_live.size(&live.pool) == 1);
        assert!(b6_live.contains(b1_ret, &live.pool));

    }

}











