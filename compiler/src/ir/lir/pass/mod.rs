mod propagate_atomics;
pub use self::propagate_atomics::propagate_atomics;

mod validate;
pub use self::validate::validate;

//mod compile_pattern;
//pub use self::compile_pattern::compile_pattern;

mod simplify_branches;
pub use self::simplify_branches::simplify_branches;

mod remove_orphan_blocks;
pub use self::remove_orphan_blocks::remove_orphan_blocks;

mod promote_tail_calls;
pub use self::promote_tail_calls::promote_tail_calls;
