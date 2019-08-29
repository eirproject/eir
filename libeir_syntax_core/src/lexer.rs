use regex::Regex;
use std::str::CharIndices;
use lazy_static::lazy_static;

lazy_static! {
    static ref TEST: Regex = Regex::new("...").unwrap();
}

#[derive(Clone, PartialEq, Eq, Debug)]
pub enum Tok<'input> {

    // Keywords
    Module,
    Attributes,
    Fun,
    Case,
    Call,
    Apply,
    When,
    End,
    Catch,
    Do,
    Let,
    In,
    Of,
    Primop,
    Try,
    Receive,
    After,
    Letrec,

    // Identifiers/atomics
    Atom(&'input str),
    Variable(&'input str),
    Integer((bool, &'input str)),
    Float(&'input str),
    Char(char),
    String(&'input str),

    // Symbols
    ParenOpen,
    ParenClose,
    CurlyOpen,
    CurlyClose,
    SquareOpen,
    SquareClose,
    TriOpen,
    TriClose,
    MapOpen,
    MapMatch,
    MapClose,
    BitstringOpen,
    BitstringClose,
    BitstringPatternOpen,
    BitstringPatternSep,

    Annotation,

    Colon,
    Comma,
    ForwardSlash,
    Equals,
    Pipe,
    Arrow,
    HashRocket,
}

const KEYWORDS: &'static [(&'static str, Tok<'static>)] = &[
    ("module", Tok::Module),
    ("attributes", Tok::Attributes),
    ("fun", Tok::Fun),
    ("case", Tok::Case),
    ("call", Tok::Call),
    ("apply", Tok::Apply),
    ("when", Tok::When),
    ("end", Tok::End),
    ("catch", Tok::Catch),
    ("do", Tok::Do),
    ("let", Tok::Let),
    ("in", Tok::In),
    ("of", Tok::Of),
    ("primop", Tok::Primop),
    ("try", Tok::Try),
    ("receive", Tok::Receive),
    ("after", Tok::After),
    ("letrec", Tok::Letrec),
];

fn is_escapechar(c: char) -> bool {
    c == 'b' || c == 'd' || c == 'e'
        || c == 'f' || c == 'n' || c == 'r'
        || c == 's' || c == 't' || c == 'v'
        || c == '"' || c == '\'' || c == '\\'
}
fn is_control(c: char) -> bool {
    c >= '\u{0000}' && c <= '\u{001f}'
}
fn is_inputchar(c: char) -> bool {
    c != '\n' && c != '\r'
}
fn is_digit(c: char) -> bool {
    c >= '0' && c <= '9'
}
fn is_octal(c: char) -> bool {
    c >= '0' && c <= '7'
}
fn is_uppercase(c: char) -> bool {
    (c >= 'A' && c <= 'Z')
        || (c >= '\u{00c0}' && c <= '\u{00d6}')
        || (c >= '\u{00d8}' && c <= '\u{00de}')
}
fn is_lowercase(c: char) -> bool {
    (c >= 'a' && c <= 'z')
        || (c >= '\u{00df}' && c <= '\u{00f6}')
        || (c >= '\u{00f8}' && c <= '\u{00ff}')
}
fn is_namechar(c: char) -> bool {
    is_uppercase(c) || is_lowercase(c) || is_digit(c)
        || (c == '@') || (c == '_')
}

pub struct Tokenizer<'input> {
    text: &'input str,
    chars: CharIndices<'input>,
    next: Option<(usize, char)>,
}

impl<'input> Tokenizer<'input> {

    pub fn new(text: &'input str) -> Self {
        let mut t = Tokenizer {
            text: text,
            chars: text.char_indices(),
            next: None,
        };
        t.bump();
        t
    }

    fn bump(&mut self) -> Option<(usize, char)> {
        self.next = self.chars.next();
        self.next
    }

    fn bump_escape(&mut self) -> Result<(), ()> {
        match self.bump() {
            Some((_idx0, '^')) => {
                match self.bump() {
                    Some((_idx1, c)) if c >= '\u{0040}' && c <= '\u{005f}' => {
                        self.bump();
                        Ok(())
                    }
                    _ => Err(()),
                }
            }
            Some((_idx0, c)) if is_octal(c) => {
                match self.bump() {
                    Some((_idx1, c)) if is_octal(c) => {
                        match self.bump() {
                            Some((_idx1, c)) if is_octal(c) => {
                                self.bump();
                                Ok(())
                            }
                            _ => Ok(()),
                        }
                    }
                    _ => Ok(()),
                }
            },
            Some((_idx0, c)) if is_escapechar(c) => {
                self.bump();
                Ok(())
            }
            _ => Err(()),
        }
    }

    fn take_while<F>(&mut self, fun: F) -> &'input str where F: Fn(char) -> bool {
        let start = self.next.unwrap().0;
        let mut end = self.next.unwrap().0;
        loop {
            match self.next {
                Some((idx0, c)) if fun(c) => {
                    end = idx0;
                    self.bump();
                }
                Some((idx0, _c)) => {
                    end = idx0;
                    break;
                }
                None => {
                    end += 1; // TODO: Very wrong for unicode
                    break;
                }
            }
        }

        return &self.text[start..end];
    }

    fn next_token(&mut self) -> Option<Result<(usize, Tok<'input>, usize), ()>> {
        'outer: loop {
            return match self.next {

                // Keywords and variables
                Some((idx0, c)) if is_namechar(c) && !is_digit(c) => {
                    let mut end = idx0;
                    'inner: loop {
                        match self.bump() {
                            Some((idx1, c)) if is_namechar(c) => {
                                end = idx1;
                            }
                            Some((idx1, _)) => {
                                end = idx1;
                                break 'inner;
                            }
                            None => {
                                end += 1;
                                break 'inner;
                            }
                        }
                    }

                    let word = &self.text[idx0..end];

                    // Check for keywords
                    let kw = KEYWORDS.iter()
                        .filter(|&&(w, _)| w == word)
                        .next();
                    if let Some((_, kw)) = kw {
                        return Some(Ok((idx0, kw.clone(), end)));
                    }

                    // Variable
                    if is_uppercase(c) || c == '_' {
                        return Some(Ok((idx0, Tok::Variable(word), end)));
                    }

                    Some(Err(()))
                }

                // Atoms
                Some((idx0, '\'')) => {
                    self.bump();
                    let end;
                    loop {
                        match self.next {
                            Some((_idx1, '\\')) => {
                                match self.bump_escape() {
                                    Ok(()) => (),
                                    Err(()) => return Some(Err(())),
                                }
                            }
                            Some((idx1, '\'')) => {
                                self.bump();
                                end = idx1 + 1; // TODO: Very very wrong for unicode
                                break;
                            }
                            Some((_idx1, c)) if is_control(c) => return Some(Err(())),
                            Some((_idx1, c)) if is_inputchar(c) => {
                                self.bump();
                            }
                            _ => return Some(Err(())),
                        }
                    }

                    let atom = &self.text[idx0+1..end-1];
                    Some(Ok((idx0, Tok::Atom(atom), end)))
                }

                // Numbers
                Some((idx0, c)) if is_digit(c) => {
                    let string = self.take_while(is_digit);
                    Some(Ok((idx0, Tok::Integer((true, string)), idx0+1)))
                }
                Some((idx0, '+')) => {
                    match self.bump() {
                        Some((idx1, c)) if is_digit(c) => {
                            let string = self.take_while(is_digit);
                            Some(Ok((idx0, Tok::Integer((true, string)), idx1+1)))
                        }
                        _ => return Some(Err(())),
                    }
                }

                // Symbols
                Some((idx0, '(')) => {
                    self.bump();
                    Some(Ok((idx0, Tok::ParenOpen, idx0+1)))
                }
                Some((idx0, ')')) => {
                    self.bump();
                    Some(Ok((idx0, Tok::ParenClose, idx0+1)))
                }
                Some((idx0, '{')) => {
                    self.bump();
                    Some(Ok((idx0, Tok::CurlyOpen, idx0+1)))
                }
                Some((idx0, '~')) => {
                    match self.bump() {
                        Some((idx1, '{')) => {
                            self.bump();
                            Some(Ok((idx0, Tok::MapOpen, idx1+1)))
                        }
                        _ => return Some(Err(())),
                    }
                }
                Some((idx0, '}')) => {
                    match self.bump() {
                        Some((idx1, '#')) => {
                            self.bump();
                            Some(Ok((idx0, Tok::BitstringClose, idx1+1)))
                        }
                        Some((idx1, '~')) => {
                            self.bump();
                            Some(Ok((idx0, Tok::MapClose, idx1+1)))
                        }
                        _ => {
                            Some(Ok((idx0, Tok::CurlyClose, idx0+1)))
                        }
                    }
                }
                Some((idx0, '[')) => {
                    self.bump();
                    Some(Ok((idx0, Tok::SquareOpen, idx0+1)))
                }
                Some((idx0, ']')) => {
                    self.bump();
                    Some(Ok((idx0, Tok::SquareClose, idx0+1)))
                }
                Some((idx0, '#')) => {
                    match self.bump() {
                        Some((idx1, '{')) => {
                            self.bump();
                            Some(Ok((idx0, Tok::BitstringOpen, idx1+1)))
                        }
                        Some((idx1, '<')) => {
                            self.bump();
                            Some(Ok((idx0, Tok::BitstringPatternOpen, idx1+1)))
                        }
                        _ => Some(Err(())),
                    }
                }
                Some((idx0, '>')) => {
                    match self.bump() {
                        Some((idx1, '(')) => {
                            self.bump();
                            Some(Ok((idx0, Tok::BitstringPatternSep, idx1+1)))
                        }
                        _ => Some(Ok((idx0, Tok::TriClose, idx0+1))),
                    }
                }
                Some((idx0, '<')) => {
                    self.bump();
                    Some(Ok((idx0, Tok::TriOpen, idx0+1)))
                }
                Some((idx0, ':')) => {
                    match self.bump() {
                        Some((idx1, '=')) => {
                            self.bump();
                            Some(Ok((idx0, Tok::MapMatch, idx1+1)))
                        }
                        _ => Some(Ok((idx0, Tok::Colon, idx0+1)))
                    }
                }
                Some((idx0, '-')) => {
                    match self.bump() {
                        Some((idx1, '|')) => {
                            self.bump();
                            Some(Ok((idx0, Tok::Annotation, idx1+1)))
                        }
                        Some((idx1, '>')) => {
                            self.bump();
                            Some(Ok((idx0, Tok::Arrow, idx1+1)))
                        }
                        Some((idx1, c)) if is_digit(c) => {
                            let string = self.take_while(is_digit);
                            Some(Ok((idx0, Tok::Integer((false, string)), idx1+1)))
                        }
                        _ => Some(Err(())),
                    }
                }
                Some((idx0, ',')) => {
                    self.bump();
                    Some(Ok((idx0, Tok::Comma, idx0+1)))
                }
                Some((idx0, '/')) => {
                    self.bump();
                    Some(Ok((idx0, Tok::ForwardSlash, idx0+1)))
                }
                Some((idx0, '=')) => {
                    match self.bump() {
                        Some((idx1, '>')) => {
                            self.bump();
                            Some(Ok((idx0, Tok::HashRocket, idx1+1)))
                        }
                        _ => Some(Ok((idx0, Tok::Equals, idx0+1))),
                    }
                }
                Some((idx0, '|')) => {
                    self.bump();
                    Some(Ok((idx0, Tok::Pipe, idx0+1)))
                }

                // Supressed
                Some((_idx0, '%')) => {
                    loop {
                        match self.bump() {
                            Some((_idx1, '\n')) => {
                                continue 'outer;
                            }
                            Some((_idx1, '\r')) => {
                                continue 'outer;
                            }
                            _ => (),
                        }
                    }
                }
                Some((_idx0, c)) if c == ' ' || c == '\t' => {
                    loop {
                        match self.bump() {
                            Some((_idx1, ' ')) => (),
                            Some((_idx1, '\t')) => (),
                            _ => continue 'outer,
                        }
                    }
                }

                Some((_idx1, '\n')) => {
                    self.bump();
                    continue 'outer;
                }
                Some((_idx1, '\r')) => {
                    match self.bump() {
                        Some((_idx2, '\n')) => {
                            self.bump();
                            continue 'outer;
                        }
                        _ => continue 'outer,
                    }
                }

                None => None,

                c => unimplemented!("{:?}", c),
            }
        }
    }

}

impl<'input> Iterator for Tokenizer<'input> {
    type Item = Result<(usize, Tok<'input>, usize), ()>;

    fn next(&mut self) -> Option<Result<(usize, Tok<'input>, usize), ()>> {
        self.next_token()
    }
}

#[test]
fn test_symbols() {
    let text = "( ) { } [ ] #{ }# #< >( -| , / =";
    let mut tok = Tokenizer::new(&text);

    assert!(tok.next_token().unwrap().unwrap().1 == Tok::ParenOpen);
    assert!(tok.next_token().unwrap().unwrap().1 == Tok::ParenClose);
    assert!(tok.next_token().unwrap().unwrap().1 == Tok::CurlyOpen);
    assert!(tok.next_token().unwrap().unwrap().1 == Tok::CurlyClose);
    assert!(tok.next_token().unwrap().unwrap().1 == Tok::SquareOpen);
    assert!(tok.next_token().unwrap().unwrap().1 == Tok::SquareClose);
    assert!(tok.next_token().unwrap().unwrap().1 == Tok::BitstringOpen);
    assert!(tok.next_token().unwrap().unwrap().1 == Tok::BitstringClose);
    assert!(tok.next_token().unwrap().unwrap().1 == Tok::BitstringPatternOpen);
    assert!(tok.next_token().unwrap().unwrap().1 == Tok::BitstringPatternSep);

    assert!(tok.next_token().unwrap().unwrap().1 == Tok::Annotation);

    assert!(tok.next_token().unwrap().unwrap().1 == Tok::Comma);
    assert!(tok.next_token().unwrap().unwrap().1 == Tok::ForwardSlash);
    assert!(tok.next_token().unwrap().unwrap().1 == Tok::Equals);

    assert!(tok.next_token() == None);
}

#[test]
fn test_keywords() {
    // Common path, don't bother testing all
    let text = "module attributes fun case call of primop";
    let mut tok = Tokenizer::new(&text);

    assert!(tok.next_token().unwrap().unwrap().1 == Tok::Module);
    assert!(tok.next_token().unwrap().unwrap().1 == Tok::Attributes);
    assert!(tok.next_token().unwrap().unwrap().1 == Tok::Fun);
    assert!(tok.next_token().unwrap().unwrap().1 == Tok::Case);
    assert!(tok.next_token().unwrap().unwrap().1 == Tok::Call);
    assert!(tok.next_token().unwrap().unwrap().1 == Tok::Of);
    assert!(tok.next_token().unwrap().unwrap().1 == Tok::Primop);

    assert!(tok.next_token() == None);
}

#[test]
fn test_atoms() {
    let text = "'something' '\\'' '\\5' '\\55' '\\555' '\\n' '\\^@' '\\^H' '\\^_'";
    let mut tok = Tokenizer::new(&text);

    assert!(tok.next_token().unwrap().unwrap().1 == Tok::Atom("something"));
    assert!(tok.next_token().unwrap().unwrap().1 == Tok::Atom("\\'"));
    assert!(tok.next_token().unwrap().unwrap().1 == Tok::Atom("\\5"));
    assert!(tok.next_token().unwrap().unwrap().1 == Tok::Atom("\\55"));
    assert!(tok.next_token().unwrap().unwrap().1 == Tok::Atom("\\555"));
    assert!(tok.next_token().unwrap().unwrap().1 == Tok::Atom("\\n"));
    assert!(tok.next_token().unwrap().unwrap().1 == Tok::Atom("\\^@"));
    assert!(tok.next_token().unwrap().unwrap().1 == Tok::Atom("\\^H"));
    assert!(tok.next_token().unwrap().unwrap().1 == Tok::Atom("\\^_"));

    assert!(tok.next_token() == None);
}

#[test]
fn test_variables() {
    let text = "Abc ABC _abcd A@_cD";
    let mut tok = Tokenizer::new(&text);

    assert!(tok.next_token().unwrap().unwrap().1 == Tok::Variable("Abc"));
    assert!(tok.next_token().unwrap().unwrap().1 == Tok::Variable("ABC"));
    assert!(tok.next_token().unwrap().unwrap().1 == Tok::Variable("_abcd"));
    assert!(tok.next_token().unwrap().unwrap().1 == Tok::Variable("A@_cD"));

    assert!(tok.next_token() == None);
}

#[test]
fn test_lex_compile_core_file() {
    use ::std::io::Read;

    let mut f = ::std::fs::File::open("../test_data/compile.core").unwrap();
    let mut s = String::new();
    f.read_to_string(&mut s);

    let mut tok = Tokenizer::new(&s);

    loop {
        let res = tok.next_token();
        println!("{:?}", res);
        assert!(res != Some(Err(())));
        if (res == None) {
            break;
        }
    }
}











