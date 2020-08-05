use crate::{Reader, Term};

macro_rules! make_reader {
    ($var:ident = $path:expr) => {
        let bin = std::fs::read($path).unwrap();
        let mut cursor = std::io::Cursor::new(&bin);
        let mut $var = Reader::new(&mut cursor);
    };
}

#[test]
fn int_tuple() {
    make_reader!(reader = "test_data/int_tuple.etf");

    reader.header().unwrap();
    let term = reader.term().unwrap();

    assert_eq!(
        term,
        Term::Tuple(vec![Term::Integer(1), Term::Integer(2), Term::Integer(3)])
    );
}

#[test]
fn test_1() {
    make_reader!(reader = "test_data/test_1.etf");

    reader.header().unwrap();
    let term = reader.term().unwrap();

    assert_eq!(
        term,
        Term::List(
            vec![
                Term::Atom("hello".into()),
                Term::Integer(1),
                Term::Nil,
                Term::Integer(2222)
            ],
            Box::new(Term::Nil)
        )
    );
}

#[test]
fn test_2() {
    make_reader!(reader = "test_data/test_2.etf");

    reader.header().unwrap();
    let term = reader.term().unwrap();

    println!("{:?}", term);
}
