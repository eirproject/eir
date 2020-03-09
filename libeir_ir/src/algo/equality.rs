use std::collections::{BTreeMap, VecDeque};

use snafu::Snafu;

use crate::Function;
use crate::{Block, Value, PrimOp, Const};
use crate::ValueKind;

#[derive(Snafu, Debug, PartialEq, Eq)]
pub enum EqualityFail {

    BlockArity {
        left: Block,
        right: Block,
    },

    BlockOp {
        left: Block,
        right: Block,
    },

    BlockReadsLength {
        left: Block,
        right: Block,
    },

    MismatchingValue {
        left: Value,
        right: Value,
    },

    PrimReadsLength {
        left: PrimOp,
        right: PrimOp,
    },

    MismatchingConst {
        left: Const,
        right: Const,
    },

}

struct EqCtx<'a> {
    lf: &'a Function,
    rf: &'a Function,
    to_walk: VecDeque<(Block, Block)>,
    to_check: Vec<(Value, Value)>,
    map: BTreeMap<Value, Value>,
}

impl Function {

    pub fn graph_eq(&self, lhs_block: Block, rhs: &Function, rhs_block: Block) -> Result<(), EqualityFail> {

        let mut ctx = EqCtx {
            lf: self,
            rf: rhs,

            map: BTreeMap::new(),

            to_walk: VecDeque::new(),
            to_check: Vec::new(),
        };

        // 1.
        // Walk block graph and validate basic cfg isomorphism.
        // We can't just use normal block graph DFS when iterating here
        // because search order is not defined.
        // In this case we need to discover the mapping between blocks
        // in the two block scopes we are comparing, so order of values
        // is critical.
        ctx.to_walk.push_back((lhs_block, rhs_block));
        ctx.map.insert(ctx.lf.block_value(lhs_block), ctx.rf.block_value(rhs_block));

        while let Some((l_b, r_b)) = ctx.to_walk.pop_front() {

            // The mapping for the blocks should always have been created
            // when inserting it into the `to_walk` list.
            let l_block_val = ctx.lf.block_value(l_b);
            let r_block_val = ctx.rf.block_value(r_b);
            debug_assert_eq!(ctx.map.get(&l_block_val), Some(&r_block_val));

            // Insert mappings between the block args.
            let l_args = ctx.lf.block_args(l_b);
            let r_args = ctx.rf.block_args(r_b);
            if l_args.len() != r_args.len() {
                return Result::Err(EqualityFail::BlockArity { left: l_b, right: r_b });
            }
            for (l, r) in l_args.iter().zip(r_args.iter()) {
                ctx.map.insert(*l, *r);
            }

            // Check equality between the operations themselves
            if !ctx.lf.block_op_eq(l_b, &ctx.rf, r_b) {
                return Result::Err(EqualityFail::BlockOp { left: l_b, right: r_b });
            }

            // Traverse the block OP read values
            let l_reads = ctx.lf.block_reads(l_b);
            let r_reads = ctx.rf.block_reads(r_b);
            if l_reads.len() != r_reads.len() {
                return Result::Err(EqualityFail::BlockReadsLength {
                    left: l_b, right: r_b });
            }
            for (l, r) in l_reads.iter().zip(r_reads.iter()) {
                traverse_value(&mut ctx, *l, *r)?;
            }
        }

        // 2. Check all remaining values that are arguments
        for (l, r) in ctx.to_check.iter() {
            if ctx.map.get(l) != Some(r) {
                return Result::Err(EqualityFail::MismatchingValue {
                    left: *l, right: *r });
            }
        }

        Ok(())
    }

}

fn traverse_value<'a>(ctx: &mut EqCtx<'a>, l: Value, r: Value) -> Result<(), EqualityFail> {
    if let Some(nr) = ctx.map.get(&l) {
        if *nr == r {
            return Ok(());
        } else {
            return Err(EqualityFail::MismatchingValue { left: l, right: r });
        }
    }

    match (ctx.lf.value_kind(l), ctx.rf.value_kind(r)) {
        (ValueKind::Block(lb), ValueKind::Block(rb)) => {
            ctx.map.insert(l, r);
            ctx.to_walk.push_back((lb, rb));
            Ok(())
        }
        (ValueKind::Argument(_, _), ValueKind::Argument(_, _)) => {
            ctx.to_check.push((l, r));
            Ok(())
        }
        (ValueKind::Const(lc), ValueKind::Const(rc)) => {
            if !ctx.lf.cons().eq_other(lc, ctx.rf.cons(), rc) {
                return Err(EqualityFail::MismatchingConst { left: lc, right: rc });
            }
            Ok(())
        }
        (ValueKind::PrimOp(lp), ValueKind::PrimOp(rp)) => {
            let l_reads = ctx.lf.primop_reads(lp);
            let r_reads = ctx.rf.primop_reads(rp);
            if l_reads.len() != r_reads.len() {
                return Err(EqualityFail::PrimReadsLength { left: lp, right: rp });
            }
            for (l, r) in l_reads.iter().zip(r_reads.iter()) {
                traverse_value(ctx, *l, *r)?;
            }
            Ok(())
        }
        _ => Err(EqualityFail::MismatchingValue { left: l, right: r }),
    }
}

#[cfg(test)]
mod tests {
    use crate::parse_function_unwrap;

    #[test]
    fn basic_equality() {
        let ir1 = parse_function_unwrap("
a'foo':a'bar'/1 {
    entry(%ret, %thr, %arg1):
        block2(%arg1);
    block2(%b):
        block3(%b);
    block3(%a):
        %ret(%a);
}
");

        let ir2 = parse_function_unwrap("
a'a':a'b'/1 {
    entry(%ret, %thr, %arg1):
        block2(%arg1);
    block3(%a):
        %ret(%a);
    block2(%b):
        block3(%b);
}
");

        assert!(ir1.graph_eq(ir1.block_entry(), &ir2, ir2.block_entry()).is_ok());
    }

    #[test]
    fn args_length_inequality() {
        let ir1 = parse_function_unwrap("
a'foo':a'bar'/2 {
    entry(%ret, %thr, %arg1, %arg2):
        %ret(%arg1);
}
");

        let ir2 = parse_function_unwrap("
a'foo':a'bar'/1 {
    entry(%ret, %thr, %arg1):
        %ret(%arg1);
}
");

        assert!(ir1.graph_eq(ir1.block_entry(), &ir2, ir2.block_entry()).is_err());
    }

    #[test]
    fn args_read_inequality() {
        let ir1 = parse_function_unwrap("
a'foo':a'bar'/2 {
    entry(%ret, %thr, %arg1, %arg2):
        %ret(%arg1);
}
");

        let ir2 = parse_function_unwrap("
a'foo':a'bar'/2 {
    entry(%ret, %thr, %arg1, %arg2):
        %ret(%arg2);
}
");

        assert!(ir1.graph_eq(ir1.block_entry(), &ir2, ir2.block_entry()).is_err());
    }

    #[test]
    fn args_read_const_equality() {
        let ir1 = parse_function_unwrap("
a'foo':a'bar'/2 {
    entry(%ret, %thr, %arg1, %arg2):
        %ret(a'a');
}
");

        let ir2 = parse_function_unwrap("
a'foo':a'bar'/2 {
    entry(%ret, %thr, %arg1, %arg2):
        %ret(a'a');
}
");

        assert!(ir1.graph_eq(ir1.block_entry(), &ir2, ir2.block_entry()).is_ok());
    }

    #[test]
    fn args_read_const_inequality() {
        let ir1 = parse_function_unwrap("
a'foo':a'bar'/2 {
    entry(%ret, %thr, %arg1, %arg2):
        %ret(a'a');
}
");

        let ir2 = parse_function_unwrap("
a'foo':a'bar'/2 {
    entry(%ret, %thr, %arg1, %arg2):
        %ret(a'b');
}
");

        assert!(ir1.graph_eq(ir1.block_entry(), &ir2, ir2.block_entry()).is_err());
    }

    #[test]
    fn args_prim_inequality() {
        let ir1 = parse_function_unwrap("
a'foo':a'bar'/1 {
    entry(%ret, %thr, %arg1):
        %fun = a'a':a'a'/1;
        %fun(%ret, %thr, %arg1);
}
");

        let ir2 = parse_function_unwrap("
a'foo':a'bar'/1 {
    entry(%ret, %thr, %arg1):
        %fun = a'a':a'b'/1;
        %fun(%ret, %thr, %arg1);
}
");

        assert!(ir1.graph_eq(ir1.block_entry(), &ir2, ir2.block_entry()).is_err());
    }

}
