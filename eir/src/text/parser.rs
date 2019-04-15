use crate::FunctionIdent;
use crate::ClosureEnv;
use crate::intern::Atom;

impl FunctionIdent {

    pub fn parse_with_module(string: &str, module: Atom) -> Result<Self, ()> {
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
            name: Atom::from_str(&captures[1]),
            arity: captures[5].parse().unwrap(),
            lambda: captures.get(3).map(|v| {
                (
                    ClosureEnv::from_num(v.as_str().parse().unwrap()),
                    captures[4].parse().unwrap(),
                )
            }),
        };
        Ok(res)
    }

}

