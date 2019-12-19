use std::sync::{Arc, RwLock};

use libeir_util_parse::{Scanner, Parse, ParserConfig, Source, SourceError};
use libeir_diagnostics::{CodeMap, ByteIndex};

use super::ast;
use super::token::{Lexer, Token};

#[cfg_attr(rustfmt, rustfmt_skip)]
#[allow(unknown_lints)]
#[allow(clippy)]
#[allow(unused)]
pub(crate) mod grammar {
    // During the build step, `build.rs` will output the generated parser to `OUT_DIR` to avoid
    // adding it to the source directory, so we just directly include the generated parser here.
    //
    // Even with `.gitignore` and the `exclude` in the `Cargo.toml`, the generated parser can still
    // end up in the source directory. This could happen when `cargo build` builds the file out of
    // the Cargo cache (`$HOME/.cargo/registrysrc`), and the build script would then put its output
    // in that cached source directory because of https://github.com/lalrpop/lalrpop/issues/280.
    // Later runs of `cargo vendor` then copy the source from that directory, including the
    // generated file.
    include!(concat!(env!("OUT_DIR"), "/grammar.rs"));
}

#[derive(Debug)]
pub enum ParseError {
    LalrPop(lalrpop_util::ParseError<ByteIndex, Token, ()>),
}
impl From<lalrpop_util::ParseError<ByteIndex, Token, ()>> for ParseError {
    fn from(v: lalrpop_util::ParseError<ByteIndex, Token, ()>) -> Self {
        ParseError::LalrPop(v)
    }
}

pub struct ParseConfig {
    pub codemap: Arc<RwLock<CodeMap>>,
}
impl ParserConfig for ParseConfig {
    fn codemap(&self) -> &Arc<RwLock<CodeMap>> {
        &self.codemap
    }
}
impl Default for ParseConfig {
    fn default() -> Self {
        ParseConfig {
            codemap: Arc::new(RwLock::new(CodeMap::new())),
        }
    }
}

impl Parse for ast::Root {
    type Parser = grammar::RootParser;
    type Error = ParseError;
    type Config = ParseConfig;
    type Token = Result<(ByteIndex, Token, ByteIndex), ()>;

    fn file_map_error(_err: SourceError) -> Self::Error {
        unimplemented!()
    }

    fn parse<S>(_config: &ParseConfig, source: S) -> Result<Self, Self::Error>
    where
        S: Source,
    {
        let scanner = Scanner::new(source);
        let lexer = Lexer::new(scanner);
        Self::parse_tokens(lexer)
    }

    fn parse_tokens<S>(tokens: S) -> Result<Self, Self::Error>
    where
        S: IntoIterator<Item = Self::Token>,
    {
        Self::Parser::new()
            .parse(tokens)
            .map_err(|e| e.into())
    }

}

#[cfg(test)]
mod test {
    use libeir_util_parse::{Parser, Parse};
    use super::{ParseConfig, ParseError};
    use super::ast::Root;

    fn parse<'a, T>(input: &'a str) -> T
    where
        T: Parse<T, Config = ParseConfig, Error = ParseError>
    {
        let config = ParseConfig::default();
        let parser = Parser::new(config);
        let err = match parser.parse_string::<&'a str, T>(input) {
            Ok(ast) => return ast,
            Err(err) => err,
        };
        panic!("{:?}", err);
    }

    #[test]
    fn simple() {
        let _: Root = parse("
{woo, '123fwoo', {}}.
");
    }

    #[test]
    fn basic_ast() {
        let _: Root = parse("
{attribute,1,file,{\"woo.erl\",1}}.
{attribute,1,module,woo}.
{attribute,3,export,[{foo,2},{bar,1},{barr,1}]}.
{function,5,foo,2,
    [{clause,5,
    [{var,5,'A'},{var,5,'B'}],
    [],
    [{op,5,'+',{var,5,'A'},{var,5,'B'}}]}]}.
{function,7,bar,1,
    [{clause,7,[{integer,7,1}],[],[{integer,7,2}]},
    {clause,8,[{integer,8,2}],[],[{integer,8,4}]},
    {clause,9,[{var,9,'N'}],[],[{var,9,'N'}]}]}.
{function,11,barr,1,
    [{clause,11,[{integer,11,1}],[],[{integer,11,2}]},
    {clause,12,[{integer,12,2}],[],[{integer,12,4}]}]}.
{function,14,binary,0,
    [{clause,14,[],[],
    [{bin,14,[{bin_element,14,{string,14,\"woo\"},default,default}]}]}]}.
{function,16,string,0,[{clause,16,[],[],[{string,16,\"woo\"}]}]}.
{eof,17}.
");
    }

}
