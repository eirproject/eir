//extern crate peg;                         
extern crate lalrpop;

fn main() {
    lalrpop::Configuration::new()
        .force_build(true)
        .generate_in_source_tree()
        .process_file("src/parser/grammar.lalrpop")
        .unwrap();

    //lalrpop::process_root().unwrap();
    //peg::cargo_build("src/parser/grammar.rustpeg", true);

    println!("cargo:rerun-if-changed=src/parser/grammar.lalrpop");
}
