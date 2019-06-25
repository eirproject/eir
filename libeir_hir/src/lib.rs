mod ir;
pub use ir::{ HirModule };
pub use ir::{ Expr, Variable, Function, FunctionRef, Clause, LogicOpKind };

mod lower;
pub use lower::lower;

pub use libeir_ir::pattern::*;

use pretty::{ BoxDoc, Doc };
pub trait ToDoc {
    fn to_doc<'a>(&'a self) -> Doc<'a, BoxDoc>;
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_works() {
    }
}
