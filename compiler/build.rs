extern crate peg;

fn main() {
    peg::cargo_build("src/parser/grammar.rustpeg");
}
