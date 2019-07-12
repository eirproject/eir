use crate::ast::{ Annotated, Module };
use crate::lexer::{ Tokenizer, Tok };

mod grammar;

pub fn parse<'input>(text: &'input str) -> Result<Annotated<Module>,
                                                  ::lalrpop_util::ParseError<usize, Tok<'input>, ()>> {

    let tokenizer = Tokenizer::new(text);
    let parser = grammar::AnnotatedModuleParser::new();
    parser.parse(text, tokenizer)
}
