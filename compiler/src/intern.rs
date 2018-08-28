use ::string_intern::{ Validator, Symbol };

pub struct AtomSymbol;

impl Validator for AtomSymbol {
    type Err = ::std::string::ParseError;
    fn validate_symbol(_val: &str) -> Result<(), Self::Err> {
        Ok(())
    }
}

pub type Atom = Symbol<AtomSymbol>;

pub struct VariableSymbol;

impl Validator for VariableSymbol {
    type Err = ::std::string::ParseError;
    fn validate_symbol(_val: &str) -> Result<(), Self::Err> {
        Ok(())
    }
}

pub type Variable = Symbol<VariableSymbol>;
