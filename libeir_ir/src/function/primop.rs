use serde::{ Serialize, Deserialize };

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
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

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum LogicOp {
    /// All arguments are equal
    Eq,
    And,
    Or,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum PrimOpKind {

    /// (lhs, rhs)
    BinOp(BinOp),

    /// (terms..)
    LogicOp(LogicOp),

    /// (terms..)
    Tuple,

    /// (head, tail)
    ListCell,

    /// (k1, v1, ... kn, vn)
    Map,

    /// (terms..)
    ValueList,

    /// Returns a function of arity `a`.
    /// If the function does not exists, this must return a function that
    /// throws badarg when called.
    /// For function capture semantics, the `CaptureFunction` op should be
    /// used instead. This will throw badarg at capture time.
    /// `(m, f, a)`
    CaptureFunction,

}
