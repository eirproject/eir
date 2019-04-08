use super::Atom;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AtomicTerm {
    Integer(num_bigint::BigInt),
    Float,
    Atom(Atom),
    Char(char),
    String(String),
    Nil,
}
impl From<Atom> for AtomicTerm {
    fn from(val: Atom) -> Self {
        AtomicTerm::Atom(val)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConstantTerm {
    Atomic(AtomicTerm),
    List(Vec<ConstantTerm>, Box<ConstantTerm>),
}
impl ConstantTerm {

    pub fn atom(&self) -> Option<Atom> {
        if let ConstantTerm::Atomic(atomic) = self {
            if let AtomicTerm::Atom(atom) = atomic {
                Some(atom.clone())
            } else {
                None
            }
        } else {
            None
        }
    }

}
