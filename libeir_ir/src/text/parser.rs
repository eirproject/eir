use crate::FunctionIdent;
use libeir_intern::Ident;

impl FunctionIdent {

    pub fn parse_with_module(string: &str, module: Ident) -> Result<Self, ()> {
        lazy_static::lazy_static! {
            static ref FUNCTION_IDENT_RE: regex::Regex = {
                regex::Regex::new("^([^:@/]+)(@(\\d+)\\.(\\d+))?/(\\d+)$").unwrap()
            };
        }

        let captures = FUNCTION_IDENT_RE.captures(string)
            .map(|ok| Ok(ok))
            .unwrap_or(Err(()))?;

        let res = FunctionIdent {
            module: module,
            name: Ident::from_str(&captures[1]),
            arity: captures[5].parse().unwrap(),
        };
        Ok(res)
    }

}

