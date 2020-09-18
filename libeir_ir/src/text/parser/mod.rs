use std::sync::Arc;

use libeir_diagnostics::{CodeMap, SourceIndex};
use libeir_intern::Ident;
use libeir_util_parse::{
    self as parse, error_tee, ErrorReceiver, Parse, Scanner, Source, SourceError,
};

use crate::FunctionIdent;

mod lexer;
use lexer::{Lexer, Token};

mod errors;
use self::errors::Errors;
pub use self::errors::ParserError;

use super::lower::LowerMap;

pub type Parser = parse::Parser<()>;

type ParserErrorReceiver<'a> = dyn ErrorReceiver<E = ParserError, W = ParserError> + 'a;

/// Used in the grammar for easy span creation
macro_rules! span {
    ($l:expr, $r:expr) => {
        SourceSpan::new($l, $r)
    };
    ($i:expr) => {
        SourceSpan::new($i, $i)
    };
}

#[cfg_attr(rustfmt, rustfmt_skip)]
#[allow(unknown_lints)]
#[allow(clippy)]
#[allow(unused_variables, dead_code, unused_imports, unused_parens)]
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

pub struct NamedFunction {
    pub name: Ident,
    pub function: super::ast::Function,
}

impl Parse for super::ast::Module {
    type Parser = self::grammar::ModuleParser;
    type Error = ParserError;
    type Config = ();
    type Token = std::result::Result<(SourceIndex, Token, SourceIndex), ParserError>;

    fn root_file_error(source: std::io::Error, path: std::path::PathBuf) -> Self::Error {
        ParserError::RootFileError { source, path }
    }

    fn parse_tokens<S>(errors: &mut ParserErrorReceiver, tokens: S) -> Result<Self, ()>
    where
        S: IntoIterator<Item = std::result::Result<(SourceIndex, Token, SourceIndex), ParserError>>,
    {
        let result = self::grammar::ModuleParser::new().parse(errors, tokens);

        let ret;
        match result {
            Ok(ok) => ret = ok,
            Err(err) => {
                errors.error(err.into());
                return Err(());
            }
        }

        if (*errors).is_failed() {
            Err(())
        } else {
            Ok(ret)
        }
    }

    fn parse<S>(_parser: &Parser, errors: &mut ParserErrorReceiver, source: S) -> Result<Self, ()>
    where
        S: Source,
    {
        let scanner = Scanner::new(source);
        let lexer = Lexer::new(scanner);
        Self::parse_tokens(errors, lexer)
    }
}
impl Parse for NamedFunction {
    type Parser = self::grammar::StandaloneFunctionParser;
    type Error = ParserError;
    type Config = ();
    type Token = std::result::Result<(SourceIndex, Token, SourceIndex), ParserError>;

    fn root_file_error(source: std::io::Error, path: std::path::PathBuf) -> Self::Error {
        ParserError::RootFileError { source, path }
    }

    fn parse_tokens<S>(errors: &mut ParserErrorReceiver, tokens: S) -> Result<Self, ()>
    where
        S: IntoIterator<Item = std::result::Result<(SourceIndex, Token, SourceIndex), ParserError>>,
    {
        let result = self::grammar::StandaloneFunctionParser::new().parse(errors, tokens);

        let name;
        let function;
        match result {
            std::result::Result::Ok((i, f)) => {
                name = i;
                function = f;
            }
            std::result::Result::Err(err) => {
                errors.error(err.into());
                return Err(());
            }
        }

        if (*errors).is_failed() {
            Err(())
        } else {
            Ok(NamedFunction { name, function })
        }
    }

    fn parse<'a, S>(
        _parser: &Parser,
        errors: &'a mut ParserErrorReceiver<'a>,
        source: S,
    ) -> Result<Self, ()>
    where
        S: Source,
    {
        let scanner = Scanner::new(source);
        let lexer = Lexer::new(scanner);
        Self::parse_tokens(errors, lexer)
    }
}

impl FunctionIdent {
    pub fn parse(string: &str) -> std::result::Result<Self, ()> {
        lazy_static::lazy_static! {
            static ref FUNCTION_IDENT_RE: regex::Regex = {
                regex::Regex::new("^([^/]+):([^:]+)/(\\d+)$").unwrap()
            };
        }

        let captures = FUNCTION_IDENT_RE
            .captures(string)
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

        let captures = FUNCTION_IDENT_RE
            .captures(string)
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

pub fn module_codemap(text: &str, codemap: Arc<CodeMap>) -> (Result<crate::Module, ()>, Errors) {
    let mut errors = Errors::new();

    let parser = Parser::new((), codemap);

    let res = match parser.parse_string(&mut errors, text) {
        Ok(module) => {
            let module: super::ast::Module = module;

            error_tee(&mut errors, |mut errors| {
                let mut adapter = errors.make_into_adapter();

                match module.lower(&mut adapter) {
                    Ok(module) => Ok(module),
                    Err(()) => Err(()),
                }
            })
        }
        Err(()) => Err(()),
    };

    (res, errors)
}

pub fn module(text: &str) -> (Result<crate::Module, ()>, Errors) {
    module_codemap(text, Arc::new(CodeMap::new()))
}

pub fn module_unwrap(text: &str) -> crate::Module {
    let codemap = Arc::new(CodeMap::new());
    match module_codemap(text, codemap.clone()) {
        (Ok(module), errors) => {
            errors.print(&codemap);
            module
        }
        (Err(()), errors) => {
            errors.print(&codemap);
            panic!();
        }
    }
}

pub fn function_map_codemap(
    text: &str,
    codemap: Arc<CodeMap>,
) -> (Result<(crate::Function, LowerMap), ()>, Errors) {
    let mut errors = Errors::new();

    let parser = Parser::new((), codemap);

    let ret = match parser.parse_string(&mut errors, text) {
        Ok(named) => {
            let named: NamedFunction = named;

            error_tee(&mut errors, |mut errors| {
                let mut adapter = errors.make_into_adapter();

                match named.function.lower(&mut adapter, named.name) {
                    Ok(res) => Ok(res),
                    Err(()) => Err(()),
                }
            })
        }
        Err(()) => Err(()),
    };

    (ret, errors)
}

pub fn function_map(text: &str) -> (Result<(crate::Function, LowerMap), ()>, Errors) {
    function_map_codemap(text, Arc::new(CodeMap::new()))
}

pub fn function_codemap(
    text: &str,
    codemap: Arc<CodeMap>,
) -> (Result<crate::Function, ()>, Errors) {
    match function_map_codemap(text, codemap) {
        (Ok((fun, _)), errors) => (Ok(fun), errors),
        (Err(()), errors) => (Err(()), errors),
    }
}

pub fn function(text: &str) -> (Result<crate::Function, ()>, Errors) {
    match function_map(text) {
        (Ok((fun, _)), errors) => (Ok(fun), errors),
        (Err(()), errors) => (Err(()), errors),
    }
}

pub fn function_unwrap(text: &str) -> crate::Function {
    let codemap = Arc::new(CodeMap::new());
    match function_codemap(text, codemap.clone()) {
        (Ok(fun), errors) => {
            errors.print(&codemap);
            fun
        }
        (Err(()), errors) => {
            errors.print(&codemap);
            panic!();
        }
    }
}

pub fn function_map_unwrap(text: &str) -> (crate::Function, LowerMap) {
    let codemap = Arc::new(CodeMap::new());
    match function_map_codemap(text, codemap.clone()) {
        (Ok(fun), errors) => {
            errors.print(&codemap);
            fun
        }
        (Err(()), errors) => {
            errors.print(&codemap);
            panic!();
        }
    }
}

#[cfg(test)]
mod test {

    use std::sync::Arc;

    use super::function_unwrap;
    use super::{NamedFunction, Parser};
    use crate::text::ast;

    use libeir_diagnostics::{CodeMap, SourceSpan};
    use libeir_intern::Ident;
    use libeir_util_parse::Errors;

    use pretty_assertions::assert_eq;

    #[test]
    fn parse_empty_function() {
        let codemap = Arc::new(CodeMap::new());
        let mut errors = Errors::new();

        let parser = Parser::new((), codemap);

        let _module: NamedFunction = parser
            .parse_string(&mut errors, "a'foo':a'foo'/1 {}")
            .unwrap();
    }

    #[test]
    #[should_panic]
    fn lower_empty_function_fails() {
        function_unwrap("a'foo':a'foo'/1 {}");
    }

    #[test]
    fn parse_kitchen_sink() {
        let codemap = Arc::new(CodeMap::new());
        let mut errors = Errors::new();
        let parser = Parser::new((), codemap);
        let module: ast::Module = parser
            .parse_string(
                &mut errors,
                "
a'kitchen_sink' {
    a'something'/1 {
        entry(%return, %throw, %num):
            %something = a'true';
            %fun = a'a':a'b'/a'c';
            %foobar(%woo, %hoo);
    }
    a'second_fun'/0 {}
}
",
            )
            .unwrap();

        //let ref_module = ast::Module {
        //    span: SourceSpan::UNKNOWN,
        //    name: Ident::from_str("kitchen_sink"),
        //    items: vec![
        //        ast::ModuleItem::Function(ast::Function {
        //            span: SourceSpan::UNKNOWN,
        //            name: Ident::from_str("something"),
        //            arity: 1.into(),
        //            items: vec![
        //                ast::FunctionItem::Label(ast::Label {
        //                    span: SourceSpan::UNKNOWN,
        //                    name: ast::Value::Block(Ident::from_str("entry")),
        //                    args: vec![
        //                        ast::Value::Value(Ident::from_str("return")),
        //                        ast::Value::Value(Ident::from_str("throw")),
        //                        ast::Value::Value(Ident::from_str("num")),
        //                    ],
        //                }),
        //                ast::FunctionItem::Assignment(ast::Assignment {
        //                    span: SourceSpan::UNKNOWN,
        //                    lhs: ast::Value::Value(Ident::from_str("something")),
        //                    rhs: ast::Value::Atom(Ident::from_str("true")),
        //                }),
        //                ast::FunctionItem::Assignment(ast::Assignment {
        //                    span: SourceSpan::UNKNOWN,
        //                    lhs: ast::Value::Value(Ident::from_str("fun")),
        //                    rhs: ast::Value::CaptureFunction(
        //                        Box::new(ast::Value::Atom(Ident::from_str("a"))),
        //                        Box::new(ast::Value::Atom(Ident::from_str("b"))),
        //                        Box::new(ast::Value::Atom(Ident::from_str("c"))),
        //                    ),
        //                }),
        //                ast::FunctionItem::Op(ast::Op::CallControlFlow(ast::CallControlFlowOp {
        //                    span: SourceSpan::UNKNOWN,
        //                    target: ast::Value::Value(Ident::from_str("foobar")),
        //                    args: vec![
        //                        ast::Value::Value(Ident::from_str("woo")),
        //                        ast::Value::Value(Ident::from_str("hoo")),
        //                    ],
        //                })),
        //            ],
        //        }),
        //        ast::ModuleItem::Function(ast::Function {
        //            span: SourceSpan::UNKNOWN,
        //            name: Ident::from_str("second_fun"),
        //            arity: 0.into(),
        //            items: vec![],
        //        }),
        //    ],
        //};

        //assert_eq!(module, ref_module);
    }

    #[test]
    fn lower_add_one() {
        let _fun = function_unwrap(
            "
a'foo':a'add_one'/1 {
    entry(%return, %throw, %num):
        %add_fun = a'erlang':a'+'/2;
        %add_fun(%return, %throw, %num, 1);
}
",
        );
    }

    #[test]
    fn location() {
        let _fun = function_unwrap(
            "
a'foo':a'bar'/1 {
  !location [\"foo\":\"bar\"@\"foo.erl\":12];
  entry(%ret, %thr, %num):
    %ret(%num);
}
",
        );
    }
}
