extern crate lalrpop;

fn main() {
    lalrpop::Configuration::new()
        .force_build(true)
        .generate_in_source_tree()
        .process_file("src/grammar.lalrpop")
        .unwrap();

    println!("cargo:rerun-if-changed=src/grammar.lalrpop");
}
