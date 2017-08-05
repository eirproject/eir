use ::std::collections::HashSet;
use ::ir::lir::{ FunctionCfg, Source };

pub fn validate(cfg: &mut FunctionCfg) {

    let mut assigns = HashSet::new();

    for arg in cfg.args_iter() {
        assigns.insert(*arg);
    }

    for block in cfg.blocks_iter() {
        for phi in block.phi_nodes.iter() {
            if assigns.contains(&phi.ssa) {
                panic!("Double assign of {:?}", phi.ssa);
            }
            assigns.insert(phi.ssa);
        }

        for op in block.ops.iter() {
            for write in op.writes.iter() {
                if assigns.contains(write) {
                    panic!("Double assign of {:?}", write);
                }
                assigns.insert(*write);
            }
        }
    }

    for block in cfg.blocks_iter() {
        for phi in block.phi_nodes.iter() {
            for &(_label, ssa) in phi.entries.iter() {
                if !assigns.contains(&ssa) {
                    panic!("Use of unassigned {:?}", ssa);
                }
            }
        }

        for op in block.ops.iter() {
            for read in op.reads.iter() {
                if let Source::Variable(ref ssa) = *read {
                    if !assigns.contains(&ssa) {
                        panic!("Use of unassigned {:?}", ssa);
                    }
                }
            }
        }
    }

}
