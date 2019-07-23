#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum BinOp {
    /// ==
    Equal,
    /// /=
    NotEqual,
    /// =<
    LessEqual,
    /// <
    Less,
    /// >=
    GreaterEqual,
    /// >
    Greater,
    /// =:=
    ExactEqual,
    /// =/=
    ExactNotEqual,
}
impl BinOp {

    pub fn symmetric(self) -> bool {
        match self {
            BinOp::Equal => true,
            BinOp::NotEqual => true,
            BinOp::ExactEqual => true,
            BinOp::ExactNotEqual => true,
            _ => false,
        }
    }

}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum PrimOpKind {

    /// (lhs, rhs)
    BinOp(BinOp),

    /// (terms..)
    Tuple,

    /// (head, tail)
    ListCell,

    /// (k1, v1, ... kn, vn)
    Map,

    /// (terms..)
    ValueList,

    /// (terms..)
    LogicAnd,
    /// (terms..)
    LogicOr,

}
