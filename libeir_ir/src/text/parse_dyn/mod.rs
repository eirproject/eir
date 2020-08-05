use libeir_diagnostics::{Diagnostic, Label, SourceSpan, ToDiagnostic};
use libeir_intern::{Ident, Symbol};
use snafu::Snafu;

use crate::constant::Integer;

use super::ast::{DynToken, Value};

#[derive(Copy, Clone)]
pub enum Nesting {
    Top,

    Parens(SourceSpan),
    Braces(SourceSpan),
    MapBraces(SourceSpan),
    SquareBrackets(SourceSpan),
    AngleBrackets(SourceSpan),
}

pub enum Token {
    ParenOpen,
    ParenClose,
    BraceOpen,
    BraceClose,
    MapBraceOpen,
    SquareBracketOpen,
    SquareBracketClose,
    AngleBracketOpen,
    AngleBracketClose,

    Ident(Symbol),
    Variable(Symbol),

    Atom(Symbol),
    Integer(Integer),
    Float(Symbol),
    String(Symbol),

    Percent,
    Colon,
    SemiColon,
    Comma,
    Question,
    ForwardSlash,
    Equals,
    EqualsEquals,
    FatArrow,
    Underscore,
    Pipe,
    At,
    Bang,

    Unpack,
    Unreachable,
    Arity,
    IfBool,
    TraceCaptureRaw,
    Value,
    Match,
    Type,
    Case,
    Guard,
    Except,

    EOF,
}

pub fn flatten_dyn_token(tok: &[DynToken]) -> Vec<(Token, SourceSpan)> {
    fn flatten(toks: &[DynToken], out: &mut Vec<(Token, SourceSpan)>) {
        for tok in toks {
            match tok {
                DynToken::Parens(inner, span) => {
                    out.push((Token::ParenOpen, *span));
                    flatten(inner, out);
                    out.push((Token::ParenClose, *span));
                }
                DynToken::Braces(inner, span) => {
                    out.push((Token::BraceOpen, *span));
                    flatten(inner, out);
                    out.push((Token::BraceClose, *span));
                }
                DynToken::MapBraces(inner, span) => {
                    out.push((Token::MapBraceOpen, *span));
                    flatten(inner, out);
                    out.push((Token::BraceClose, *span));
                }
                DynToken::SquareBrackets(inner, span) => {
                    out.push((Token::SquareBracketOpen, *span));
                    flatten(inner, out);
                    out.push((Token::SquareBracketClose, *span));
                }
                DynToken::AngleBrackets(inner, span) => {
                    out.push((Token::AngleBracketOpen, *span));
                    flatten(inner, out);
                    out.push((Token::AngleBracketClose, *span));
                }

                DynToken::Ident(ident) => out.push((Token::Ident(ident.name), ident.span)),
                DynToken::Variable(ident) => out.push((Token::Variable(ident.name), ident.span)),
                DynToken::Atom(ident) => out.push((Token::Atom(ident.name), ident.span)),
                DynToken::Integer(int, span) => out.push((Token::Integer(int.clone()), *span)),
                DynToken::Float(ident) => out.push((Token::Float(ident.name), ident.span)),
                DynToken::String(ident) => out.push((Token::String(ident.name), ident.span)),

                DynToken::Percent(span) => out.push((Token::Percent, *span)),
                DynToken::Colon(span) => out.push((Token::Colon, *span)),
                DynToken::SemiColon(span) => out.push((Token::SemiColon, *span)),
                DynToken::Comma(span) => out.push((Token::Comma, *span)),
                DynToken::Question(span) => out.push((Token::Question, *span)),
                DynToken::ForwardSlash(span) => out.push((Token::ForwardSlash, *span)),
                DynToken::Equals(span) => out.push((Token::Equals, *span)),
                DynToken::EqualsEquals(span) => out.push((Token::EqualsEquals, *span)),
                DynToken::FatArrow(span) => out.push((Token::FatArrow, *span)),
                DynToken::Underscore(span) => out.push((Token::Underscore, *span)),
                DynToken::Pipe(span) => out.push((Token::Pipe, *span)),
                DynToken::At(span) => out.push((Token::At, *span)),
                DynToken::Bang(span) => out.push((Token::Bang, *span)),

                DynToken::Unpack(span) => out.push((Token::Unpack, *span)),
                DynToken::Unreachable(span) => out.push((Token::Unreachable, *span)),
                DynToken::Arity(span) => out.push((Token::Arity, *span)),
                DynToken::IfBool(span) => out.push((Token::IfBool, *span)),
                DynToken::TraceCaptureRaw(span) => out.push((Token::TraceCaptureRaw, *span)),
                DynToken::Value(span) => out.push((Token::Value, *span)),
                DynToken::Match(span) => out.push((Token::Match, *span)),
                DynToken::Type(span) => out.push((Token::Type, *span)),
                DynToken::Case(span) => out.push((Token::Case, *span)),
                DynToken::Guard(span) => out.push((Token::Guard, *span)),
                DynToken::Except(span) => out.push((Token::Except, *span)),
            }
        }
    }

    let mut out = vec![];
    flatten(tok, &mut out);
    out
}

//pub struct DynTokenLexer {
//    stack: Vec<(Nesting, &'a [DynToken], usize)>,
//    containing: SourceSpan,
//}
//
//impl<'a> DynTokenLexer<'a> {
//    pub fn new(tokens: &'a [DynToken], containing: SourceSpan) -> Self {
//        DynTokenLexer {
//            stack: vec![Nesting::Top, tokens, 0],
//            containing,
//        }
//    }
//}
//
//impl Iterator for DynTokenLexer {
//    type Item = (SourceIndex, Token, SourceIndex);
//    fn next(&mut self) -> Option<Self::Item> {
//        enum Action<'b> {
//            PopLex(Nesting),
//            PushLex(Nesting, &'b [DynToken]),
//        }
//
//        let action = match self.stack.last_mut() {
//            Some((nesting, tokens, cursor)) if cursor >= tokens.len() => Action::PopLex(*nesting),
//            Some((_, tokens, cursor)) => match &tokens[cursor] {
//                DynToken::Parens(children, span) => {
//                    Action::PushLex(Nesting::Parens(*span), children)
//                }
//                DynToken::Braces(children, span) => {
//                    Action::PushLex(Nesting::Braces(*span), children)
//                }
//                DynToken::MapBraces(children, span) => {
//                    Action::PushLex(Nesting::MapBraces(*span), children)
//                }
//                DynToken::SquareBrackets(children, span) => {
//                    Action::PushLex(Nesting::SquareBrackets(*span), children)
//                }
//                DynToken::AngleBrackets(children, span)
//                _ => unimplemented!(),
//            },
//            None => unreachable!(),
//        };
//
//        match action {
//            Action::PopLex(Nesting::Top) => {
//                (self.containing.end(), Token::Eof, self.containing.end())
//            }
//            Action::PopLex(Nesting::Parens(span)) => {
//                self.stack.pop();
//                (span.start(), Token::ParenClose, span.end())
//            }
//            Action::PopLex(Nesting::Braces(span)) => {
//                self.stack.pop();
//                (span.start(), Token::BraceClose, span.end())
//            }
//            Action::PopLex(Nesting::MapBraces(span)) => {
//                self.stack.pop();
//                (span.start(), Token::BraceClose, span.end())
//            }
//            Action::PopLex(Nesting::SquareBrackets(span)) => {
//                self.stack.pop();
//                (span.start(), Token::SquareBracketClose, span.end())
//            }
//            Action::PopLex(Nesting::AngleBrackets(span)) => {
//                self.stack.pop();
//                (span.start(), Token::AngleBracketClose, span.end())
//            }
//        }
//    }
//}

macro_rules! try_seq {
    ($ctx:expr) => {};
}

macro_rules! container_token {
	($name:ident, $ident:ident) => {
        pub fn $name(&mut self) -> Result<(&'a [DynToken], SourceSpan), DynParserError> {
            match self.pop()? {
                DynToken::$ident(inner, span) => Ok((inner, *span)),
                other => Err(DynParserError::UnexpectedToken { span: other.span() }),
            }
        }
	};
}

pub struct ParseCtx<'a> {
    containing: SourceSpan,
    tokens: &'a [DynToken],
    pos: usize,
}

#[derive(Debug, Snafu)]
#[snafu(visibility = "pub")]
pub enum DynParserError {
    UnexpectedToken { span: SourceSpan },
    UnexpectedEnd { span: SourceSpan },
}

impl ToDiagnostic for DynParserError {
    fn to_diagnostic(&self) -> Diagnostic {
        match self {
            DynParserError::UnexpectedEnd { span } => Diagnostic::error()
                .with_message("unexpected end")
                .with_labels(vec![Label::primary(span.source_id(), *span)]),
            DynParserError::UnexpectedToken { span } => Diagnostic::error()
                .with_message("unexpected token")
                .with_labels(vec![Label::primary(span.source_id(), *span)]),
        }
    }
}

impl<'a> ParseCtx<'a> {
    pub fn new(tokens: &'a [DynToken], span: SourceSpan) -> Self {
        ParseCtx {
            containing: span,
            tokens,
            pos: 0,
        }
    }

    pub fn pop(&mut self) -> Result<&'a DynToken, DynParserError> {
        if self.pos < self.tokens.len() {
            let tok = &self.tokens[self.pos];
            self.pos += 1;
            Ok(tok)
        } else {
            UnexpectedEnd {
                span: self
                    .tokens
                    .last()
                    .map(|tok| tok.span())
                    .unwrap_or(self.containing),
            }
            .fail()?
        }
    }

    pub fn peek(&self) -> Option<&'a DynToken> {
        self.tokens.get(self.pos)
    }

    pub fn try_parse<R, E>(
        &mut self,
        fun: impl FnOnce(&mut ParseCtx) -> Result<R, E>,
    ) -> Result<R, E> {
        let pos = self.pos;
        match fun(self) {
            Ok(res) => Ok(res),
            Err(err) => {
                self.pos = pos;
                Err(err)
            }
        }
    }

    pub fn comma<R, E>(
        &mut self,
        fun: impl Fn(&mut ParseCtx) -> Result<R, E>,
    ) -> Result<Vec<R>, E> {
        let mut res = vec![];
        loop {
            match fun(self) {
                Ok(val) => res.push(val),
                Err(err) => break,
            }
            match self.peek() {
                Some(&DynToken::Comma(_)) => (),
                _ => break,
                None => break,
            }
        }
        return Ok(res);
    }

    pub fn repeat_any<R, E>(&mut self, fun: impl Fn(&mut ParseCtx) -> Result<R, E>) -> Vec<R> {
        let mut res = vec![];
        loop {
            match fun(self) {
                Ok(val) => res.push(val),
                Err(err) => break,
            }
        }
        return res;
    }

    pub fn eof(&mut self) -> Result<(), DynParserError> {
        match self.peek() {
            Some(tok) => Err(DynParserError::UnexpectedToken { span: tok.span() }),
            None => Ok(()),
        }
    }

    pub fn tok_string(&mut self) -> Result<Ident, DynParserError> {
        match self.pop()? {
            DynToken::String(string) => Ok(*string),
            tok => Err(DynParserError::UnexpectedToken { span: tok.span() }),
        }
    }
    pub fn tok_integer(&mut self) -> Result<(&'a Integer, SourceSpan), DynParserError> {
        match self.pop()? {
            DynToken::Integer(int, span) => Ok((int, *span)),
            tok => Err(DynParserError::UnexpectedToken { span: tok.span() }),
        }
    }

    pub fn tok_colon(&mut self) -> Result<SourceSpan, DynParserError> {
        match self.pop()? {
            DynToken::Colon(span) => Ok(*span),
            tok => Err(DynParserError::UnexpectedToken { span: tok.span() }),
        }
    }
    pub fn tok_at(&mut self) -> Result<SourceSpan, DynParserError> {
        match self.pop()? {
            DynToken::At(span) => Ok(*span),
            tok => Err(DynParserError::UnexpectedToken { span: tok.span() }),
        }
    }

    container_token!(tok_parens, Parens);
    container_token!(tok_braces, Braces);
    container_token!(tok_map_braces, MapBraces);
    container_token!(tok_square_brackets, SquareBrackets);
    container_token!(tok_angle_brackets, AngleBrackets);
}

pub struct ParsePos(usize);

pub fn val(ctx: &mut ParseCtx) -> Result<Value, DynParserError> {
    println!("val PEEK {:?}", ctx.peek());
    match ctx.peek() {
        Some(DynToken::SquareBrackets(_, _)) => val_list(ctx),
        Some(DynToken::AngleBrackets(_, _)) => val_value_list(ctx),
        _ => val_atomic(ctx),
    }
}

//pub fn value_max(ctx: &mut ParseCtx) -> Result<Value, DynParserError> {
//    ctx.try_parse(|ctx| match ctx.pop() {
//        DynToken::SquareBrackets(inner, span) => unimplemented!(),
//        _ => panic!(),
//    })
//}

pub fn val_value_list(ctx: &mut ParseCtx) -> Result<Value, DynParserError> {
    ctx.try_parse(|ctx| {
        let (inner, span) = ctx.tok_angle_brackets()?;

        let mut ictx = ParseCtx::new(inner, span);
        let vec = ictx.comma(val)?;

        println!("{:?}", vec);

        println!("before value list eof");
        ictx.eof()?;
        println!("after value list eof");

        Ok(Value::ValueList(vec))
    })
}

pub fn val_list(ctx: &mut ParseCtx) -> Result<Value, DynParserError> {
    ctx.try_parse(|ctx| {
        let (inner, span) = ctx.tok_square_brackets()?;

        let mut ictx = ParseCtx::new(inner, span);
        let vec = ictx.comma(val)?;

        match ictx.pop() {
            Err(_) => Ok(Value::List(vec, None)),
            Ok(DynToken::Pipe(_span)) => {
                let tail = val(&mut ictx)?;
                ictx.eof()?;
                Ok(Value::List(vec, Some(Box::new(tail))))
            }
            Ok(other) => Err(DynParserError::UnexpectedToken { span: other.span() }),
        }
    })
}

pub fn val_atomic(ctx: &mut ParseCtx) -> Result<Value, DynParserError> {
    println!("val_atomic PEEK {:?}", ctx.peek());
    ctx.try_parse(|ctx| match ctx.pop()? {
        DynToken::Atom(atom) => Ok(Value::Atom(*atom)),
        DynToken::Integer(int, span) => Ok(Value::Integer(int.clone())),
        DynToken::Ident(block) => Ok(Value::Block(*block)),
        DynToken::Variable(var) => Ok(Value::Value(*var)),
        tok => {
            println!("unexpected tok {:?}", tok);
            UnexpectedToken { span: tok.span() }.fail()?
        }
    })
}

//pub fn value(ctx: &mut ParseCtx) -> Result<Value, ()> {
//    if let Ok(head_val) = value(ctx) {
//    } else {
//        unimplemented!()
//    }
//}
