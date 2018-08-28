mod propagate_atomics;
pub use self::propagate_atomics::propagate_atomics;

mod validate;
pub use self::validate::validate;

mod compile_pattern;
pub use self::compile_pattern::compile_pattern;

mod simplify_branches;
pub use self::simplify_branches::simplify_branches;
