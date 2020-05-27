use libeir_ir::Block;

use super::AnalysisContext;

pub(super) fn propagate(ctx: &mut AnalysisContext, block: Block) -> bool {
    let reads = ctx.fun.block_reads(block);

    let val = match reads.len() {
        3 => reads[2],
        4 => reads[3],
        _ => panic!(),
    };
    let val = ctx.follow(val);

    if let Some(cons) = ctx.fun.value_const(val) {
        let branch = match ctx.fun.cons().as_bool(cons) {
            Some(true) => 0,
            Some(false) => 1,
            None => {
                if reads.len() == 3 {
                    panic!("IfBool without fallback branch on non-bool value")
                } else {
                    2
                }
            }
        };
        let target = reads[branch];
        ctx.set_branch(target);

        true
    } else {
        false
    }
}
