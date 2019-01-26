use ::parser::AtomicLiteral;
use ::ir::SSAVariable;
use ::ir::lir::{ OpKind, Source, FunctionCfg };
use ::std::collections::HashMap;

pub fn propagate_atomics(cfg: &mut FunctionCfg) {

    // Write => Read
    let mut constants: HashMap<SSAVariable, AtomicLiteral> = HashMap::new();
    let mut moves: HashMap<SSAVariable, SSAVariable> = HashMap::new();

    for block in cfg.blocks_iter_mut() {
        block.ops.retain(|op| {
            if let OpKind::Move = op.kind {
                if let Source::Constant(ref constant) = op.reads[0] {
                    constants.insert(op.writes[0], constant.clone());
                    return false;
                }
                if let Source::Variable(ref ssa) = op.reads[0] {
                    moves.insert(op.writes[0], *ssa);
                    return false;
                }
            }
            true
        });
    }

    let mut moves_prop: HashMap<SSAVariable, SSAVariable> = moves.clone();
    for (o_write, _) in moves.iter() {
        loop {
            let o_read = *moves_prop.get(o_write).unwrap();
            if let Some(i_read) = moves_prop.get(&o_read).cloned() {
                moves_prop.insert(*o_write, i_read);
            } else {
                break;
            }
        }
    }

    for block in cfg.blocks_iter_mut() {
        for phi in block.phi_nodes.iter_mut() {
            for entry in phi.entries.iter_mut() {
                if let Source::Variable(var) = entry.1 {
                    if let Some(constant) = constants.get(&var) {
                        entry.1 = Source::Constant(constant.clone());
                    } else {
                        if let Some(rssa) = moves_prop.get(&var) {
                            if let Some(constant) = constants.get(rssa) {
                                entry.1 = Source::Constant(constant.clone());
                            } else {
                                entry.1 = Source::Variable(*rssa);
                            }
                        }
                    }
                }
            }
        }
        for op in block.ops.iter_mut() {
            for read in op.reads.iter_mut() {
                if let Source::Variable(var) = *read {
                    if let Some(constant) = constants.get(&var) {
                        *read = Source::Constant(constant.clone());
                    } else {
                        if let Some(rssa) = moves_prop.get(&var) {
                            if let Some(constant) = constants.get(rssa) {
                                *read = Source::Constant(constant.clone());
                            } else {
                                *read = Source::Variable(*rssa);
                            }
                        }
                    }
                }
            }
        }
    }
}
