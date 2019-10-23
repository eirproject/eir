use libeir_ir::Block;

use super::AnalysisContext;

pub(super) fn propagate(ctx: &mut AnalysisContext, block: Block) -> bool {

    let reads = ctx.fun.block_reads(block);

    let target = ctx.follow(reads[0]);
    ctx.set_branch(target);

    if let Some(target_block) = ctx.fun.value_block(target) {

        for (read, arg) in reads[1..].iter()
            .zip(ctx.fun.block_args(target_block))
        {
            ctx.add_rename(
                target_block,
                *read, *arg,
            );
        }

        true
    } else {
        false
    }
}
