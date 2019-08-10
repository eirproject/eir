use crate::FunctionIdent;
use libeir_intern::Ident;

impl FunctionIdent {

    pub fn parse(string: &str) -> Result<Self, ()> {
        lazy_static::lazy_static! {
            static ref FUNCTION_IDENT_RE: regex::Regex = {
                regex::Regex::new("^([^/]+):([^:]+)/(\\d+)$").unwrap()
            };
        }

        let captures = FUNCTION_IDENT_RE.captures(string)
            .map(Ok)
            .unwrap_or(Err(()))?;

        let res = FunctionIdent {
            module: Ident::from_str(&captures[1]),
            name: Ident::from_str(&captures[2]),
            arity: captures[3].parse().unwrap(),
        };
        Ok(res)
    }

    pub fn parse_with_module(string: &str, module: Ident) -> Result<Self, ()> {
        lazy_static::lazy_static! {
            static ref FUNCTION_IDENT_RE: regex::Regex = {
                regex::Regex::new("^([^:]+)/(\\d+)$").unwrap()
            };
        }

        let captures = FUNCTION_IDENT_RE.captures(string)
            .map(Ok)
            .unwrap_or(Err(()))?;

        let res = FunctionIdent {
            module,
            name: Ident::from_str(&captures[1]),
            arity: captures[2].parse().unwrap(),
        };
        Ok(res)
    }

}

