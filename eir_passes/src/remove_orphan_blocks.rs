//use ::ir::lir::{ FunctionCfg, LabelN };
use ::eir::{ FunctionBuilder, Ebb };
use ::std::collections::HashSet;

/// This pass removes blocks which have no predecessors.
///
/// Useful for cleaning up after other passes which would produce
/// otherwise illegal IR.
/// An example is the `promote_tail_calls` pass which can produce
/// an orphaned chain of blocks which end in a PHI which references
/// a nonexistent SSA variable.
pub fn remove_orphan_blocks(builder: &mut FunctionBuilder) {
    let fun = builder.function_mut();

    let mut all_labels: HashSet<Ebb> = HashSet::new();
    let mut jump_targets: HashSet<Ebb> = HashSet::new();

    loop {
        all_labels.clear();
        jump_targets.clear();

        jump_targets.insert(fun.ebb_entry());

        for ebb in fun.iter_ebb() {
            all_labels.insert(ebb);
            for op in fun.iter_op(ebb) {
                for branch in fun.op_branches(op) {
                    jump_targets.insert(fun.ebb_call_target(*branch));
                }
            }
        }

        let mut found = false;
        for orphan in all_labels.difference(&jump_targets) {
            //println!("Removing orphan: {}", orphan);
            found = true;
            fun.ebb_remove(*orphan);
            //cfg.remove_block(*orphan);
        }
        if !found { break; }

    }
}
