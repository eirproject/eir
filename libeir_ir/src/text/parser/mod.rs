use std::borrow::Cow;
use std::sync::{Arc, RwLock};
use std::path::Path;

use libeir_diagnostics::{CodeMap, FileName, ByteIndex};
use libeir_intern::Ident;
use libeir_util_parse::{Source, FileMapSource, Scanner, ErrorReceiver};

use crate::FunctionIdent;

mod lexer;
use lexer::{Lexer, Token};

mod errors;
use self::errors::{ParserError, ParserErrors};
use super::ast::LowerMap;

pub fn module(text: &str) -> (Result<crate::Module, ()>, ParserErrors) {
    let parser = Parser::new();
    let mut errors = ParserErrors::new(parser.codemap.clone());

    let module: super::ast::Module = match parser.parse_string(&mut errors, text) {
        Ok(module) => module,
        Err(()) => return (Err(()), errors),
    };

    match module.lower(&mut errors) {
        Ok(module) => (Ok(module), errors),
        Err(()) => (Err(()), errors),
    }
}

pub fn module_unwrap(text: &str) -> crate::Module {
    match module(text) {
        (Ok(module), errors) => {
            errors.print();
            module
        },
        (Err(()), errors) => {
            errors.print();
            panic!();
        },
    }
}

pub fn function_map(text: &str) -> (Result<(crate::Function, LowerMap), ()>, ParserErrors) {
    let parser = Parser::new();
    let mut errors = ParserErrors::new(parser.codemap.clone());

    let (name, function): (Ident, super::ast::Function) = match parser.parse_string(&mut errors, text) {
        Ok(module) => module,
        Err(()) => return (Err(()), errors),
    };

    match function.lower(&mut errors, name) {
        Ok(res) => (Ok(res), errors),
        Err(()) => (Err(()), errors),
    }
}

pub fn function(text: &str) -> (Result<crate::Function, ()>, ParserErrors) {
    match function_map(text) {
        (Ok((fun, _)), errors) => (Ok(fun), errors),
        (Err(()), errors) => (Err(()), errors),
    }
}

pub fn function_unwrap(text: &str) -> crate::Function {
    match function(text) {
        (Ok(fun), errors) => {
            errors.print();
            fun
        },
        (Err(()), errors) => {
            errors.print();
            panic!();
        },
    }
}

pub fn function_map_unwrap(text: &str) -> (crate::Function, LowerMap) {
    match function_map(text) {
        (Ok(fun), errors) => {
            errors.print();
            fun
        },
        (Err(()), errors) => {
            errors.print();
            panic!();
        },
    }
}

#[cfg_attr(rustfmt, rustfmt_skip)]
#[allow(unknown_lints)]
#[allow(clippy)]
#[allow(unused_variables, dead_code, unused_imports)]
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
    include!(concat!(env!("OUT_DIR"), "/text/parser/grammar.rs"));
}

pub struct Parser {
    pub codemap: Arc<RwLock<CodeMap>>,
}

impl Parser {

    pub fn new() -> Self {
        Parser {
            codemap: Arc::new(RwLock::new(CodeMap::new())),
        }
    }

    pub fn parse_string<S, T>(
        &self,
        errors: &mut dyn ErrorReceiver<ParserError, ParserError>,
        source: S,
    ) -> Result<T, ()>
    where
        S: AsRef<str>,
        T: Parse,
    {
        let filemap =
            self.codemap.write().unwrap().add_filemap(
                FileName::Virtual(Cow::Borrowed("nofile")),
                source.as_ref().to_owned(),
            );
        T::parse(FileMapSource::new(filemap), errors)
    }

    pub fn parse_file<P, T>(
        &self,
        errors: &mut dyn ErrorReceiver<ParserError, ParserError>,
        path: P,
    ) -> Result<T, ()>
    where
        P: AsRef<Path>,
        T: Parse,
    {
        match FileMapSource::from_path(self.codemap.clone(), path) {
            Err(_err) => unimplemented!(),
            Ok(source) => T::parse(source, errors),
        }
    }

}

pub trait Parse: Sized {

    fn parse<S>(
        source: S,
        errors: &mut dyn ErrorReceiver<ParserError, ParserError>
    ) -> std::result::Result<Self, ()>
    where
        S: Source,
    {
        let scanner = Scanner::new(source);
        let lexer = Lexer::new(scanner);
        Self::parse_tokens(lexer, errors)
    }

    fn parse_tokens<S>(
        tokens: S,
        errors: &mut dyn ErrorReceiver<ParserError, ParserError>
    ) -> std::result::Result<Self, ()>
    where
        S: IntoIterator<Item = std::result::Result<(ByteIndex, Token, ByteIndex), ParserError>>;

}

impl Parse for super::ast::Module {
    fn parse_tokens<S>(tokens: S, errors: &mut dyn ErrorReceiver<ParserError, ParserError>) -> Result<Self, ()>
    where
        S: IntoIterator<Item = std::result::Result<(ByteIndex, Token, ByteIndex), ParserError>>
    {
        let result = self::grammar::ModuleParser::new()
            .parse(errors, tokens);

        let ret;
        match result {
            Ok(ok) => ret = ok,
            Err(err) => {
                errors.error(err.into());
                return Err(());
            }
        }

        if errors.is_failed() {
            Err(())
        } else {
            Ok(ret)
        }
    }
}
impl Parse for (Ident, super::ast::Function) {
    fn parse_tokens<S>(tokens: S, errors: &mut dyn ErrorReceiver<ParserError, ParserError>) -> Result<Self, ()>
    where
        S: IntoIterator<Item = std::result::Result<(ByteIndex, Token, ByteIndex), ParserError>>
    {
        let result = self::grammar::StandaloneFunctionParser::new()
            .parse(errors, tokens);

        let ret;
        match result {
            std::result::Result::Ok(ok) => ret = ok,
            std::result::Result::Err(err) => {
                errors.error(err.into());
                return Err(());
            }
        }

        if errors.is_failed() {
            Err(())
        } else {
            Ok(ret)
        }
    }
}

impl FunctionIdent {

    pub fn parse(string: &str) -> std::result::Result<Self, ()> {
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

    pub fn parse_with_module(string: &str, module: Ident) -> std::result::Result<Self, ()> {
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

#[cfg(test)]
mod test {

    use super::Parser;
    use crate::text::ast;
    use super::errors::ParserErrors;
    use super::function_unwrap;

    use libeir_intern::{Ident};

    use pretty_assertions::assert_eq;

    #[test]
    fn parse_empty_function() {
        let parser = Parser::new();
        let mut errors = ParserErrors::new(parser.codemap.clone());

        let _module: (Ident, ast::Function) = parser.parse_string(&mut errors, "foo:foo/1 {}")
            .unwrap();
    }

    #[test]
    #[should_panic]
    fn lower_empty_function_fails() {
        function_unwrap("foo:foo/1 {}");
    }

    #[test]
    fn parse_kitchen_sink() {
        let parser = Parser::new();
        let mut errors = ParserErrors::new(parser.codemap.clone());

        let module: ast::Module = parser.parse_string(&mut errors, "
kitchen_sink {
    something/1 {
        entry(%return, %throw, %num):
            %something = a'true';
            %fun = a'a':a'b'/a'c';
            %foobar(%woo, %hoo);
    }
    second_fun/0 {}
}
").unwrap();

        let ref_module = ast::Module {
            name: Ident::from_str("kitchen_sink"),
            items: vec![
                ast::ModuleItem::Function(ast::Function {
                    name: Ident::from_str("something"),
                    arity: 1.into(),
                    items: vec![
                        ast::FunctionItem::Label(ast::Label {
                            name: ast::Value::Block(Ident::from_str("entry")),
                            args: vec![
                                ast::Value::Value(Ident::from_str("return")),
                                ast::Value::Value(Ident::from_str("throw")),
                                ast::Value::Value(Ident::from_str("num")),
                            ],
                        }),
                        ast::FunctionItem::Assignment(ast::Assignment {
                            lhs: ast::Value::Value(Ident::from_str("something")),
                            rhs: ast::Value::Atom(Ident::from_str("true")),
                        }),
                        ast::FunctionItem::Assignment(ast::Assignment {
                            lhs: ast::Value::Value(Ident::from_str("fun")),
                            rhs: ast::Value::CaptureFunction(
                                Box::new(ast::Value::Atom(Ident::from_str("a"))),
                                Box::new(ast::Value::Atom(Ident::from_str("b"))),
                                Box::new(ast::Value::Atom(Ident::from_str("c"))),
                            ),
                        }),
                        ast::FunctionItem::Op(ast::Op::CallControlFlow(ast::CallControlFlowOp {
                            target: ast::Value::Value(Ident::from_str("foobar")),
                            args: vec![
                                ast::Value::Value(Ident::from_str("woo")),
                                ast::Value::Value(Ident::from_str("hoo")),
                            ],
                        })),
                    ],
                }),
                ast::ModuleItem::Function(ast::Function {
                    name: Ident::from_str("second_fun"),
                    arity: 0.into(),
                    items: vec![],
                }),
            ],
        };

        assert_eq!(module, ref_module);
    }

    #[test]
    fn lower_add_one() {
        let _fun = function_unwrap("
foo:add_one/1 {
    entry(%return, %throw, %num):
        %add_fun = a'erlang':a'+'/2;
        %add_fun(%return, %throw, %num, 1);
}
");
    }

}
