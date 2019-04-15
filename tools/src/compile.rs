use clap::{ Arg, App, SubCommand, arg_enum, value_t };

use std::io::Read;
use std::io::Write;

use eir::FunctionIdent;
use eir::text::ToEirText;

arg_enum!{
    #[derive(Debug, PartialEq, Eq)]
    pub enum OutputType {
        Eir,
        Dot,
    }
}

arg_enum!{
    #[derive(Debug)]
    pub enum CompileLevel {
        High,
        Normal,
        CPS,
    }
}

arg_enum!{
    #[derive(Debug)]
    pub enum CompilePass {
        RemoveOrphanBlocks,
        CompilePattern,
        SimplifyBranches,
        Validate,
    }
}

fn main() {

    let matches = App::new("Eir Compiler CLI")
        .version("alpha")
        .author("Hans Elias B. Josephsen")
        .about("CLI interface to various Eir compiler functionality")
        .arg(Arg::with_name("IN_FILE")
             .help("Input file for compiler")
             .required(true))
        .arg(Arg::from_usage("<FORMAT> -f,--format <FORMAT> 'output format'")
             .default_value("eir")
             .required(true)
             .case_insensitive(true)
             .possible_values(&OutputType::variants()))
        .arg(Arg::from_usage("<FUN_IDENT> -i,--ident <IDENT> 'select single function'")
             .required(false))
        .arg(Arg::from_usage("<OUT_FILE> -o,--output <FILE> 'output file'")
             .required(false))
        .arg(Arg::from_usage("-s,--to-stdout 'outputs to stdout'"))
        .arg(Arg::from_usage("<DOT_FORMAT> --run-dot <FORMAT>")
             .required(false))
        .arg(Arg::from_usage("<COMPILE_LEVEL> -l,--compile-level <COMPILE_LEVEL> 'compilation level'")
             .default_value("normal")
             .required(false)
             .case_insensitive(true)
             .possible_values(&CompileLevel::variants()))
        //.arg(Arg::from_usage("-p,--pass <PASS> 'run the given compilation pass'")
        //     .multiple(true)
        //     .possible_values(&CompilePass::variants()))
        .get_matches();

    let in_file_name = matches.value_of("IN_FILE").unwrap();

    let mut in_str = String::new();
    std::fs::File::open(&in_file_name).unwrap()
        .read_to_string(&mut in_str).unwrap();

    let parse_res = core_erlang_compiler::parser::parse(&in_str).unwrap();
    let mut eir = core_erlang_compiler::ir::parsed_to_eir(&parse_res.0);

    match value_t!(matches, "COMPILE_LEVEL", CompileLevel).unwrap() {
        CompileLevel::High => {},
        CompileLevel::Normal => {
            core_erlang_compiler::ir::eir_normal_passes(&mut eir);
        },
        CompileLevel::CPS => {
            core_erlang_compiler::ir::eir_normal_passes(&mut eir);
            eir = cps_transform::transform_module(&eir);
        },
    }

    let selected_function = matches.value_of("FUN_IDENT")
        .map(|val| FunctionIdent::parse_with_module(
            val, eir.name.clone()).unwrap());

    let mut out_data = Vec::new();
    let out_ext;
    let out_type = value_t!(matches, "FORMAT", OutputType).unwrap();
    match out_type {
        OutputType::Eir => {
            if let Some(selected) = selected_function {
                let fun = &eir.functions[&selected];
                fun.to_eir_text(0, &mut out_data).unwrap();
            } else {
                eir.to_eir_text(0, &mut out_data).unwrap();
            }
            out_ext = "eir";
        },
        OutputType::Dot => {
            let selected_function = selected_function.expect(
                "Expected function ident with -i <FUN_IDENT>");
            let fun = &eir.functions[&selected_function];

            ::eir::text::function_to_dot(&fun, &mut out_data).unwrap();

            out_ext = "dot";
        }
    }

    let out_file_name = matches.value_of("OUT_FILE")
        .map(|s| s.to_string())
        .unwrap_or_else(|| {
            format!("{}.{}", in_file_name, out_ext)
        });

    println!("Writing to {}", out_file_name);
    let mut out = ::std::fs::File::create(&out_file_name).unwrap();
    out.write(&out_data).unwrap();

    if let Some(dot_format) = matches.value_of("DOT_FORMAT") {
        assert!(out_type == OutputType::Dot);
        println!("Running dot...");

        let format_str = format!("-T{}", dot_format);
        let res = std::process::Command::new("dot")
            .arg(&format_str)
            .arg("-O")
            .arg(&out_file_name)
            .output()
            .expect("Failed to run dot");
        assert!(res.status.success(), "Failed to run dot");
    }

}
