/// Used in the grammar for easy span creation
macro_rules! span {
    ($l:expr, $r:expr) => {
        SourceSpan::new($l, $r)
    };
    ($i:expr) => {
        SourceSpan::new($i, $i)
    };
}

/// Convenience function for building parser errors
macro_rules! to_lalrpop_err (
    ($error:expr) => (lalrpop_util::ParseError::User { error: $error })
);

type ParserErrorReceiver<'a> = dyn ErrorReceiver<E = ParserError, W = ParserError> + 'a;

#[cfg_attr(rustfmt, rustfmt_skip)]
#[allow(unknown_lints)]
#[allow(clippy)]
#[allow(unused_parens)]
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
    include!(concat!(env!("OUT_DIR"), "/parser/grammar.rs"));
}

#[macro_use]
mod macros;

pub mod ast;
mod errors;

use std::collections::VecDeque;
use std::path::PathBuf;

use libeir_util_parse::{error_tee, ErrorReceiver, Scanner, Source, SourceError};
use libeir_util_parse::{Parse as GParse, Parser as GParser};

pub type Parser = GParser<ParseConfig>;
pub trait Parse<T> = GParse<T, Config = ParseConfig, Error = ParserError>;

use crate::lexer::Lexer;
use crate::preprocessor::{MacroContainer, Preprocessed, Preprocessor};

pub use self::ast::{NodeId, NodeIdGenerator};
pub use self::errors::*;

/// The type of result returned from parsing functions
pub type ParseResult<T> = Result<T, Vec<ParserError>>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ParseConfig {
    pub warnings_as_errors: bool,
    pub no_warn: bool,
    pub include_paths: VecDeque<PathBuf>,
    pub code_paths: VecDeque<PathBuf>,
    pub macros: Option<MacroContainer>,
}
impl ParseConfig {
    pub fn new() -> Self {
        ParseConfig::default()
    }
}
impl Default for ParseConfig {
    fn default() -> Self {
        ParseConfig {
            warnings_as_errors: false,
            no_warn: false,
            include_paths: VecDeque::new(),
            code_paths: VecDeque::new(),
            macros: None,
        }
    }
}

impl GParse for ast::Module {
    type Parser = grammar::ModuleParser;
    type Error = ParserError;
    type Config = ParseConfig;
    type Token = Preprocessed;

    fn file_map_error(err: SourceError) -> Self::Error {
        err.into()
    }

    fn parse<'a, S>(
        parser: &Parser,
        err: &'a mut ParserErrorReceiver<'a>,
        source: S,
    ) -> Result<Self, ()>
    where
        S: Source,
    {
        error_tee(err, |mut errors| {
            let scanner = Scanner::new(source);
            let lexer = Lexer::new(scanner);
            error_tee(&mut errors.clone().make_into_adapter(), |preproc_errors| {
                let tokens = Preprocessor::new(parser, lexer, preproc_errors);
                Self::parse_tokens(&mut errors, tokens)
            })
        })
    }

    fn parse_tokens<'a, S: IntoIterator<Item = Preprocessed>>(
        err: &'a mut ParserErrorReceiver<'a>,
        tokens: S,
    ) -> Result<Self, ()> {
        let mut nid = NodeIdGenerator::new();
        let result = Self::Parser::new().parse(err, &mut nid, tokens);
        to_parse_result(err, result)
    }
}

impl GParse for ast::Expr {
    type Parser = grammar::ExprParser;
    type Error = ParserError;
    type Config = ParseConfig;
    type Token = Preprocessed;

    fn file_map_error(err: SourceError) -> Self::Error {
        err.into()
    }

    fn parse<S>(parser: &Parser, err: &mut ParserErrorReceiver, source: S) -> Result<Self, ()>
    where
        S: Source,
    {
        error_tee(err, |mut errors| {
            let scanner = Scanner::new(source);
            let lexer = Lexer::new(scanner);
            error_tee(&mut errors.clone().make_into_adapter(), |preproc_errors| {
                let tokens = Preprocessor::new(parser, lexer, preproc_errors);
                Self::parse_tokens(&mut errors, tokens)
            })
        })
    }

    fn parse_tokens<S: IntoIterator<Item = Preprocessed>>(
        err: &mut ParserErrorReceiver,
        tokens: S,
    ) -> Result<Self, ()> {
        let mut nid = NodeIdGenerator::new();
        let result = Self::Parser::new().parse(err, &mut nid, tokens);
        to_parse_result(err, result)
    }
}

fn to_parse_result<T>(
    errs: &mut ParserErrorReceiver,
    result: Result<T, ParseError>,
) -> Result<T, ()> {
    match result {
        Ok(ast) => {
            if (*errs).is_failed() {
                return Err(());
            }
            Ok(ast)
        }
        Err(lalrpop_util::ParseError::User { .. }) => Err(()),
        Err(err) => {
            errs.error(err.into());
            Err(())
        }
    }
}

#[cfg(test)]
mod test {
    use std::sync::Arc;

    use pretty_assertions::assert_eq;

    use super::ast::*;
    use super::*;

    use libeir_diagnostics::*;
    use libeir_util_parse::{ErrorOrWarning, Errors};

    use crate::lexer::{Ident, Symbol};
    use crate::preprocessor::PreprocessorError;

    fn fail_with<E, W>(errors: &Errors<E, W>, codemap: &CodeMap, message: &'static str) -> !
    where
        E: ToDiagnostic,
        W: ToDiagnostic,
    {
        use term::termcolor::{ColorChoice, StandardStream};

        let config = term::Config::default();
        let mut out = StandardStream::stderr(ColorChoice::Always);
        for diagnostic in errors.iter_diagnostics() {
            term::emit(&mut out, &config, codemap, &diagnostic).unwrap();
        }
        panic!(message);
    }

    fn parse<T, S>(config: ParseConfig, codemap: Arc<CodeMap>, input: S) -> T
    where
        T: Parse<T, Config = ParseConfig, Error = ParserError>,
        S: AsRef<str>,
    {
        let mut errors = Errors::new();
        let parser = Parser::new(config, codemap);
        match parser.parse_string::<T, S>(&mut errors, input) {
            Ok(ast) => return ast,
            Err(_errs) => fail_with(&errors, &parser.codemap, "parse failed"),
        }
    }

    fn parse_fail<T, S>(
        config: ParseConfig,
        codemap: Arc<CodeMap>,
        input: S,
    ) -> Errors<ParserError, ParserError>
    where
        T: Parse<T, Config = ParseConfig, Error = ParserError>,
        S: AsRef<str>,
    {
        let mut errors = Errors::new();
        let parser = Parser::new(config, codemap);
        match parser.parse_string::<T, S>(&mut errors, input) {
            Err(()) => errors,
            _ => panic!("expected parse to fail, but it succeeded!"),
        }
    }

    macro_rules! module {
        ($codemap:expr, $nid:expr, $name:expr, $body:expr) => {{
            let mut errs = Errors::new();
            let module = Module::new(&mut errs, SourceSpan::UNKNOWN, $nid, $name, $body);
            if errs.is_failed() {
                fail_with(&errs, $codemap, "failed to create expected module!");
            }
            module
        }};
    }

    #[test]
    fn parse_empty_module() {
        let codemap = Arc::new(CodeMap::new());
        let config = ParseConfig::default();
        let result: Module = parse(config, codemap.clone(), "-module(foo).");
        let mut nid = NodeIdGenerator::new();
        let expected = module!(&codemap, &mut nid, ident!("foo"), vec![]);
        assert_eq!(result, expected);
    }

    #[test]
    fn parse_module_with_multi_clause_function() {
        let codemap = Arc::new(CodeMap::new());
        let config = ParseConfig::default();
        let result: Module = parse(
            config,
            codemap.clone(),
            "-module(foo).

foo([], Acc) -> Acc;
foo([H|T], Acc) -> foo(T, [H|Acc]).
",
        );

        let mut id_gen = NodeIdGenerator::new();
        let nid = &mut id_gen;

        let mut clauses = Vec::new();
        clauses.push(FunctionClause {
            span: SourceSpan::UNKNOWN,
            name: ident_opt!(foo),
            params: vec![nil!(nid), var!(nid, Acc)],
            guard: None,
            body: vec![var!(nid, Acc)],
        });
        clauses.push(FunctionClause {
            span: SourceSpan::UNKNOWN,
            name: ident_opt!(foo),
            params: vec![cons!(nid, var!(nid, H), var!(nid, T)), var!(nid, Acc)],
            guard: None,
            body: vec![apply!(
                nid,
                atom!(nid, foo),
                var!(nid, T),
                cons!(nid, var!(nid, H), var!(nid, Acc))
            )],
        });
        let mut body = Vec::new();
        body.push(TopLevel::Function(NamedFunction {
            span: SourceSpan::UNKNOWN,
            id: nid.next(),
            name: ident!("foo"),
            arity: 2,
            clauses,
            spec: None,
        }));
        let expected = module!(&codemap, nid, ident!(foo), body);
        assert_eq!(result, expected);
    }

    #[test]
    fn parse_if_expressions() {
        let codemap = Arc::new(CodeMap::new());
        let config = ParseConfig::default();
        let result: Module = parse(
            config,
            codemap.clone(),
            "-module(foo).

unless(false) ->
    true;
unless(true) ->
    false;
unless(Value) ->
    if
        Value == 0 -> true;
        Value -> false;
        else -> true
    end.

",
        );

        let mut id_gen = NodeIdGenerator::new();
        let nid = &mut id_gen;

        let mut clauses = Vec::new();
        clauses.push(FunctionClause {
            span: SourceSpan::UNKNOWN,
            name: ident_opt!(unless),
            params: vec![atom!(nid, false)],
            guard: None,
            body: vec![atom!(nid, true)],
        });
        clauses.push(FunctionClause {
            span: SourceSpan::UNKNOWN,
            name: ident_opt!(unless),
            params: vec![atom!(nid, true)],
            guard: None,
            body: vec![atom!(nid, false)],
        });
        clauses.push(FunctionClause {
            span: SourceSpan::UNKNOWN,
            name: ident_opt!(unless),
            params: vec![var!(nid, Value)],
            guard: None,
            body: vec![Expr::If(If {
                span: SourceSpan::UNKNOWN,
                id: nid.next(),
                clauses: vec![
                    IfClause {
                        span: SourceSpan::UNKNOWN,
                        id: nid.next(),
                        guards: vec![Guard {
                            span: SourceSpan::UNKNOWN,
                            conditions: vec![Expr::BinaryExpr(BinaryExpr {
                                span: SourceSpan::UNKNOWN,
                                id: nid.next(),
                                lhs: Box::new(var!(nid, Value)),
                                op: BinaryOp::Equal,
                                rhs: Box::new(int!(nid, 0.into())),
                            })],
                        }],
                        body: vec![atom!(nid, true)],
                    },
                    IfClause {
                        span: SourceSpan::UNKNOWN,
                        id: nid.next(),
                        guards: vec![Guard {
                            span: SourceSpan::UNKNOWN,
                            conditions: vec![var!(nid, Value)],
                        }],
                        body: vec![atom!(nid, false)],
                    },
                    IfClause {
                        span: SourceSpan::UNKNOWN,
                        id: nid.next(),
                        guards: vec![Guard {
                            span: SourceSpan::UNKNOWN,
                            conditions: vec![atom!(nid, else)],
                        }],
                        body: vec![atom!(nid, true)],
                    },
                ],
            })],
        });
        let mut body = Vec::new();
        body.push(TopLevel::Function(NamedFunction {
            span: SourceSpan::UNKNOWN,
            id: nid.next(),
            name: ident!(unless),
            arity: 1,
            clauses,
            spec: None,
        }));
        let expected = module!(&codemap, nid, ident!(foo), body);
        assert_eq!(result, expected);
    }

    #[test]
    fn parse_case_expressions() {
        let codemap = Arc::new(CodeMap::new());
        let config = ParseConfig::default();
        let result: Module = parse(
            config,
            codemap.clone(),
            "-module(foo).

typeof(Value) ->
    case Value of
        [] -> nil;
        [_|_] -> list;
        N when is_number(N) -> N;
        _ -> other
    end.

",
        );

        let mut id_gen = NodeIdGenerator::new();
        let nid = &mut id_gen;

        let mut clauses = Vec::new();
        clauses.push(FunctionClause {
            span: SourceSpan::UNKNOWN,
            name: ident_opt!(typeof),
            params: vec![var!(nid, Value)],
            guard: None,
            body: vec![Expr::Case(Case {
                span: SourceSpan::UNKNOWN,
                id: nid.next(),
                expr: Box::new(var!(nid, Value)),
                clauses: vec![
                    Clause {
                        span: SourceSpan::UNKNOWN,
                        id: nid.next(),
                        pattern: nil!(nid),
                        guard: None,
                        body: vec![atom!(nid, nil)],
                    },
                    Clause {
                        span: SourceSpan::UNKNOWN,
                        id: nid.next(),
                        pattern: cons!(nid, var!(nid, _), var!(nid, _)),
                        guard: None,
                        body: vec![atom!(nid, list)],
                    },
                    Clause {
                        span: SourceSpan::UNKNOWN,
                        id: nid.next(),
                        pattern: var!(nid, N),
                        guard: Some(vec![Guard {
                            span: SourceSpan::UNKNOWN,
                            conditions: vec![apply!(nid, atom!(nid, is_number), var!(nid, N))],
                        }]),
                        body: vec![var!(nid, N)],
                    },
                    Clause {
                        span: SourceSpan::UNKNOWN,
                        id: nid.next(),
                        pattern: var!(nid, _),
                        guard: None,
                        body: vec![atom!(nid, other)],
                    },
                ],
            })],
        });
        let mut body = Vec::new();
        body.push(TopLevel::Function(NamedFunction {
            span: SourceSpan::UNKNOWN,
            id: nid.next(),
            name: ident!(typeof),
            arity: 1,
            clauses,
            spec: None,
        }));
        let expected = module!(&codemap, nid, ident!(foo), body);
        assert_eq!(result, expected);
    }

    #[test]
    fn parse_receive_expressions() {
        let codemap = Arc::new(CodeMap::new());
        let config = ParseConfig::default();
        let result: Module = parse(
            config,
            codemap.clone(),
            "-module(foo).

loop(State, Timeout) ->
    receive
        {From, {Ref, Msg}} ->
            From ! {Ref, ok},
            handle_info(Msg, State);
        _ ->
            exit(io_lib:format(\"unexpected message: ~p~n\", [Msg]))
    after
        Timeout ->
            timeout
    end.
",
        );

        let mut id_gen = NodeIdGenerator::new();
        let nid = &mut id_gen;

        let mut clauses = Vec::new();
        clauses.push(FunctionClause {
            span: SourceSpan::UNKNOWN,
            name: ident_opt!(loop),
            params: vec![var!(nid, State), var!(nid, Timeout)],
            guard: None,
            body: vec![Expr::Receive(Receive {
                span: SourceSpan::UNKNOWN,
                id: nid.next(),
                clauses: Some(vec![
                    Clause {
                        span: SourceSpan::UNKNOWN,
                        id: nid.next(),
                        pattern: tuple!(
                            nid,
                            var!(nid, From),
                            tuple!(nid, var!(nid, Ref), var!(nid, Msg))
                        ),
                        guard: None,
                        body: vec![
                            Expr::BinaryExpr(BinaryExpr {
                                span: SourceSpan::UNKNOWN,
                                id: nid.next(),
                                lhs: Box::new(var!(nid, From)),
                                op: BinaryOp::Send,
                                rhs: Box::new(tuple!(nid, var!(nid, Ref), atom!(nid, ok))),
                            }),
                            apply!(
                                nid,
                                atom!(nid, handle_info),
                                var!(nid, Msg),
                                var!(nid, State)
                            ),
                        ],
                    },
                    Clause {
                        span: SourceSpan::UNKNOWN,
                        id: nid.next(),
                        pattern: var!(nid, _),
                        guard: None,
                        body: vec![apply!(
                            nid,
                            atom!(nid, exit),
                            apply!(
                                nid,
                                remote!(nid, io_lib, format),
                                Expr::Literal(Literal::String(
                                    nid.next(),
                                    ident!("unexpected message: ~p~n")
                                )),
                                cons!(nid, var!(nid, Msg), nil!(nid))
                            )
                        )],
                    },
                ]),
                after: Some(After {
                    span: SourceSpan::UNKNOWN,
                    id: nid.next(),
                    timeout: Box::new(var!(nid, Timeout)),
                    body: vec![atom!(nid, timeout)],
                }),
            })],
        });
        let mut body = Vec::new();
        body.push(TopLevel::Function(NamedFunction {
            span: SourceSpan::UNKNOWN,
            id: nid.next(),
            name: ident!(loop),
            arity: 2,
            clauses,
            spec: None,
        }));
        let expected = module!(&codemap, nid, ident!(foo), body);
        assert_eq!(result, expected);
    }

    #[test]
    fn parse_preprocessor_if() {
        let codemap = Arc::new(CodeMap::new());
        let config = ParseConfig::default();
        let result: Module = parse(
            config,
            codemap.clone(),
            "-module(foo).
-define(TEST, true).
-define(OTP_VERSION, 21).

-ifdef(TEST).
env() ->
    test.
-else.
env() ->
    release.
-endif.

-if(?OTP_VERSION > 21).
system_version() ->
    future.
-elif(?OTP_VERSION == 21).
system_version() ->
    ?OTP_VERSION.
-else.
system_version() ->
    old.
-endif.
",
        );

        let mut id_gen = NodeIdGenerator::new();
        let nid = &mut id_gen;

        let mut body = Vec::new();
        let mut clauses = Vec::new();
        clauses.push(FunctionClause {
            span: SourceSpan::UNKNOWN,
            name: ident_opt!(env),
            params: vec![],
            guard: None,
            body: vec![atom!(nid, test)],
        });
        let env_fun = NamedFunction {
            span: SourceSpan::UNKNOWN,
            id: nid.next(),
            name: ident!(env),
            arity: 0,
            clauses,
            spec: None,
        };
        body.push(TopLevel::Function(env_fun));

        let mut clauses = Vec::new();
        clauses.push(FunctionClause {
            span: SourceSpan::UNKNOWN,
            name: ident_opt!(system_version),
            params: vec![],
            guard: None,
            body: vec![int!(nid, 21.into())],
        });
        let system_version_fun = NamedFunction {
            span: SourceSpan::UNKNOWN,
            id: nid.next(),
            name: ident!(system_version),
            arity: 0,
            clauses,
            spec: None,
        };
        body.push(TopLevel::Function(system_version_fun));
        let expected = module!(&codemap, nid, ident!(foo), body);
        assert_eq!(result, expected);
    }

    #[test]
    fn parse_preprocessor_warning_error() {
        // NOTE: Warnings are not printed with cfg(test), as we
        // cannot control where they end up without refactoring to pass
        // a writer everywhere. You can change this for testing by
        // going to the Preprocessor and finding the line where we handle
        // the warning directive and toggle the config flag
        let codemap = Arc::new(CodeMap::default());
        let config = ParseConfig::default();
        let mut errs = parse_fail::<Module, &str>(
            config,
            codemap.clone(),
            "-module(foo).
-warning(\"this is a compiler warning\").
-error(\"this is a compiler error\").
",
        );
        match errs.errors.pop() {
            Some(ErrorOrWarning::Error(ParserError::Preprocessor {
                source: PreprocessorError::CompilerError { .. },
            })) => (),
            Some(err) => panic!(
                "expected compiler error, but got a different error instead: {:?}",
                err
            ),
            None => panic!("expected compiler error, but didn't get any errors!"),
        }
    }

    #[test]
    fn parse_try() {
        let codemap = Arc::new(CodeMap::default());
        let config = ParseConfig::default();
        let result: Module = parse(
            config,
            codemap.clone(),
            "-module(foo).

example(File) ->
    try read(File) of
        {ok, Contents} ->
            {ok, Contents}
    catch
        error:{Mod, Code} ->
            {error, Mod:format_error(Code)};
        Reason ->
            {error, Reason}
    after
        close(File)
    end.
",
        );

        let mut id_gen = NodeIdGenerator::new();
        let nid = &mut id_gen;

        let mut clauses = Vec::new();
        clauses.push(FunctionClause {
            span: SourceSpan::UNKNOWN,
            name: ident_opt!(example),
            params: vec![var!(nid, File)],
            guard: None,
            body: vec![Expr::Try(Try {
                span: SourceSpan::UNKNOWN,
                id: nid.next(),
                exprs: vec![apply!(nid, atom!(nid, read), var!(nid, File))],
                clauses: Some(vec![Clause {
                    span: SourceSpan::UNKNOWN,
                    id: nid.next(),
                    pattern: tuple!(nid, atom!(nid, ok), var!(nid, Contents)),
                    guard: None,
                    body: vec![tuple!(nid, atom!(nid, ok), var!(nid, Contents))],
                }]),
                catch_clauses: Some(vec![
                    TryClause {
                        span: SourceSpan::UNKNOWN,
                        id: nid.next(),
                        kind: Name::Atom(ident!(error)),
                        error: tuple!(nid, var!(nid, Mod), var!(nid, Code)),
                        trace: ident!(_),
                        guard: None,
                        body: vec![tuple!(
                            nid,
                            atom!(nid, error),
                            apply!(
                                nid,
                                remote!(nid, var!(nid, Mod), atom!(nid, format_error)),
                                var!(nid, Code)
                            )
                        )],
                    },
                    TryClause {
                        span: SourceSpan::UNKNOWN,
                        id: nid.next(),
                        kind: Name::Atom(ident!(throw)),
                        error: var!(nid, Reason),
                        trace: ident!(_),
                        guard: None,
                        body: vec![tuple!(nid, atom!(nid, error), var!(nid, Reason))],
                    },
                ]),
                after: Some(vec![apply!(nid, atom!(nid, close), var!(nid, File))]),
            })],
        });
        let mut body = Vec::new();
        body.push(TopLevel::Function(NamedFunction {
            span: SourceSpan::UNKNOWN,
            id: nid.next(),
            name: ident!(example),
            arity: 1,
            clauses,
            spec: None,
        }));
        let expected = module!(&codemap, nid, ident!(foo), body);
        assert_eq!(result, expected);
    }

    #[test]
    fn parse_try2() {
        let codemap = Arc::new(CodeMap::default());
        let config = ParseConfig::default();
        let _result: Module = parse(
            config,
            codemap.clone(),
            "-module(foo).

example(File < 2) ->
    try read(File) of
        {ok, Contents} ->
            {ok, Contents}
    catch
        error:{Mod, Code} ->
            {error, Mod:format_error(Code)};
        Reason ->
            {error, Reason}
    after
        close(File)
    end.

exw(File) ->
    case File of
        File < 2 ->
            ok
    end.
",
        );
    }

    #[test]
    fn parse_numbers() {
        let _result: Module = parse(
            ParseConfig::default(),
            Arc::new(CodeMap::new()),
            "-module(foo).

foo(F) -> F-1+1/1*1.

bar() -> - 2.
",
        );
    }

    #[test]
    fn parse_spec() {
        let _result: Module = parse(
            ParseConfig::default(),
            Arc::new(CodeMap::new()),
            "-module(foo).

-spec bar() -> number.
bar() -> 2.
",
        );
    }

    #[test]
    fn parse_binary_spec_constant() {
        let _result: Module = parse(
            ParseConfig::default(),
            Arc::new(CodeMap::new()),
            "-module(foo).

-type txs_hash() :: <<_:(32 * 8)>>.
-type a() :: <<_:A * (12 * 8)>>.
",
        );
    }

    #[test]
    fn parse_elixir_enum_erl() {
        use std::io::Read;

        let file = std::fs::File::open("../test_data/Elixir.Enum.erl");
        let mut string = String::new();
        file.unwrap().read_to_string(&mut string).unwrap();

        let codemap = Arc::new(CodeMap::new());
        let _result: Module = parse(ParseConfig::default(), codemap, &string);
    }
}
