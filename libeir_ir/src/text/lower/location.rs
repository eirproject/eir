use crate::text::parse_dyn::{DynParserError, ParseCtx};
use libeir_intern::Symbol;
use libeir_util_number::ToPrimitive;
use std::convert::TryInto;

pub struct ParsedTerminalLocation {
    pub file: Option<Symbol>,
    pub line: Option<u32>,
    pub module: Option<Symbol>,
    pub function: Option<Symbol>,
}

pub struct ParsedLocation {
    pub terminals: Vec<ParsedTerminalLocation>,
}

pub fn parse_terminal_location(
    ctx: &mut ParseCtx,
) -> Result<ParsedTerminalLocation, DynParserError> {
    let mut module = None;
    let mut function = None;
    let mut file = None;
    let mut line = None;

    if let Ok(module_i) = ctx.try_parse(|ctx| ctx.tok_string()) {
        module = Some(module_i.name);
        if ctx.try_parse(|ctx| ctx.tok_colon()).is_ok() {
            function = Some(ctx.tok_string()?.name);
        }
    }

    if ctx.try_parse(|ctx| ctx.tok_at()).is_ok() {
        file = Some(ctx.tok_string()?.name);
        if ctx.try_parse(|ctx| ctx.tok_colon()).is_ok() {
            line = Some(ctx.tok_integer()?.0);
        }
    }

    Ok(ParsedTerminalLocation {
        file,
        line: line.map(|i| i.to_u32().unwrap()),
        module,
        function,
    })
}

pub fn parse_location(ctx: &mut ParseCtx) -> Result<ParsedLocation, DynParserError> {
    let (toks, span) = ctx.tok_square_brackets()?;
    let mut ictx = ParseCtx::new(toks, span);

    let locs = ictx.comma(parse_terminal_location)?;
    ictx.eof()?;

    ctx.eof()?;

    Ok(ParsedLocation { terminals: locs })
}
