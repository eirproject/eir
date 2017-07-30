use ::parser::AtomicLiteral;
use ::ir::SSAVariable;
use ::ir::lir::{ BasicBlock, OpKind, Source };
use ::std::collections::HashMap;

pub fn propagate_atomics(blocks: &mut [BasicBlock]) {
    let mut constants: HashMap<SSAVariable, AtomicLiteral> = HashMap::new();

    for block in blocks.iter_mut() {
        block.ops.retain(|op| {
            if let OpKind::Move = op.kind {
                if let Source::Constant(ref constant) = op.reads[0] {
                    constants.insert(op.writes[0], constant.clone());
                    return false;
                }
            }
            true
        });
    }

    for block in blocks.iter_mut() {
        for op in block.ops.iter_mut() {
            for read in op.reads.iter_mut() {
                if let Source::Variable(var) = *read {
                    if let Some(constant) = constants.get(&var) {
                        *read = Source::Constant(constant.clone());
                    }
                }
            }
        }
    }
}
