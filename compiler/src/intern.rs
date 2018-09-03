use ::string_intern::{ Validator, Symbol };
use ::std::str::FromStr;
use ::std::fmt::Display;

struct AtomSymbol;
impl Validator for AtomSymbol {
    type Err = ::std::string::ParseError;
    fn validate_symbol(_val: &str) -> Result<(), Self::Err> {
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Atom(Symbol<AtomSymbol>);

impl Display for Atom {
    fn fmt(&self, f: &mut ::std::fmt::Formatter) -> ::std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl Atom {

    pub fn from(string: &'static str) -> Self {
        Atom(Symbol::from(string))
    }

    pub fn from_str(string: &str) -> Self {
        Atom(FromStr::from_str(string).unwrap())
    }

    pub fn as_str(&self) -> &str {
        &self.0
    }

}

pub type Variable = Atom;

lazy_static! {
    pub static ref RAISE: Atom = Atom::from("raise");
    pub static ref TRUE: Atom = Atom::from("true");
    pub static ref FALSE: Atom = Atom::from("false");
}
