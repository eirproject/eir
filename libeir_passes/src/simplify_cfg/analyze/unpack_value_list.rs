use libeir_ir::{Block, PrimOpKind};

use super::AnalysisContext;

pub(super) fn propagate(ctx: &mut AnalysisContext, block: Block, n: usize) -> bool {

    let reads = ctx.fun.block_reads(block);

    assert!(reads.len() == 2);

    let target = ctx.follow(reads[0]);
    let list = ctx.follow(reads[1]);

    if let Some(target_block) = ctx.fun.value_block(target) {
        let target_args = ctx.fun.block_args(target_block);
        assert!(n == target_args.len());

        // Value list of length 1 can't exist, always the value itself
        if n == 1 {
            ctx.set_branch(target);
            ctx.add_rename(target_block, reads[1], 0);
            return true;
        }

        if let Some(prim) = ctx.fun.value_primop(list) {
            if let PrimOpKind::ValueList = ctx.fun.primop_kind(prim) {
                let prim_reads = ctx.fun.primop_reads(prim);
                assert!(target_args.len() == prim_reads.len());

                ctx.set_branch(target);

                for (idx, read) in prim_reads.iter().enumerate() {
                    ctx.add_rename(target_block, *read, idx);
                }

                return true;
            } else {
                panic!()
            }
        }
    }

    false
}
