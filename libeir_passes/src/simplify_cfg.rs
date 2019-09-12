use std::collections::BTreeMap;

use libeir_ir::{FunctionBuilder, Mangler};
use libeir_ir::{Value, Block, OpKind};

use super::FunctionPass;

/// This pass is a powerful cleanup pass, it will do the following:
/// - Inline closures
/// - Remove needless call chains
pub struct SimplifyCfgPass {
    calls: BTreeMap<Block, Value>,
    block_calls: BTreeMap<Block, Block>,
    map: BTreeMap<Value, Value>,
    val_buf: Vec<Value>,
    mangler: Mangler,
}

impl SimplifyCfgPass {

    pub fn new() -> Self {
        SimplifyCfgPass {
            calls: BTreeMap::new(),
            block_calls: BTreeMap::new(),
            map: BTreeMap::new(),
            val_buf: Vec::new(),
            mangler: Mangler::new(),
        }
    }

}

impl FunctionPass for SimplifyCfgPass {
    fn run_function_pass(&mut self, b: &mut FunctionBuilder) {
        self.simplify_cfg(b);
    }
}

impl SimplifyCfgPass {

    fn resolve(&self, mut val: Value) -> Value {
        while let Some(next) = self.map.get(&val) {
            val = *next;
        }
        val
    }

    fn simplify_cfg(&mut self, b: &mut FunctionBuilder) {
        //println!("{}", b.fun().to_text());

        self.calls.clear();
        self.block_calls.clear();
        self.map.clear();

        let _entry = b.fun().block_entry();
        let graph = b.fun().live_block_graph();

        // Collect all calls in the CFG
        for block in graph.dfs_iter() {
            if let OpKind::Call = b.fun().block_kind(block).unwrap() {
                let target = b.fun().block_reads(block)[0];
                self.calls.insert(block, target);
            }
        }

        // Resolve new block calls until we hit an equilibrium
        loop {
            let mut changed = false;

            // Resolve block call renames
            for (block, target) in self.calls.iter() {
                let target_res = self.resolve(*target);
                if let Some(target_block) = b.fun().value_block(target_res) {

                    // If we have already processed the block, continue
                    if self.block_calls.contains_key(block) { continue; }

                    // If the target block has several incoming edges, this chain is not removable
                    let num_incoming = graph.incoming(target_block).count();
                    if num_incoming > 1 { continue; }
                    assert!(num_incoming == 1);

                    self.block_calls.insert(*block, target_block);
                    changed = true;

                    let to = &b.fun().block_reads(*block)[1..];
                    let from = b.fun().block_args(target_block);
                    assert!(from.len() == to.len());

                    for (from_val, to_val) in from.iter().zip(to.iter()) {
                        assert!(!self.map.contains_key(from_val),
                                "{} => {} (already {} => {})",
                                from_val, to_val, from_val, self.map[from_val]);
                        self.map.insert(*from_val, *to_val);
                    }
                }
            }

            // Update value map with newly discovered mappings
            self.val_buf.clear();
            self.val_buf.extend(self.map.keys().cloned());
            for key in self.val_buf.iter() {
                let val = self.map[key];
                let resolved = self.resolve(val);
                *self.map.get_mut(key).unwrap() = resolved;
            }

            if !changed { break; }
        }

        println!("{}", b.fun().to_text());
        println!("{:?}", self.map);

        //self.mangler.start(entry);
        //self.mangler.copy_entry(b.fun());

        //for (from, to) in self.map.iter() {
        //    self.mangler.add_rename(*from, *to);
        //}

        //let new_block = self.mangler.run(b);
        //b.block_set_entry(new_block);

    }

}
