use clap::{ Arg, App, ArgMatches, arg_enum, value_t };

use std::io::Read;
use std::io::Write;
use std::path::PathBuf;

use libeir_ir::{ Module, FunctionIdent, ToEirTextContext, ToEirText };

use libeir_syntax_erl::{
    lower_module,
    ParseConfig,
    Parser,
};
use libeir_diagnostics::{
    ColorChoice, Emitter, StandardStreamEmitter
};
use libeir_passes::PassManager;

//use eir::FunctionIdent;
//use eir::text::{ ToEirText, ToEirTextContext, EirLiveValuesAnnotator };

arg_enum!{
    #[derive(Debug, PartialEq, Eq)]
    pub enum OutputType {
        Eir,
        Dot,
    }
}

arg_enum!{
    #[derive(Debug, PartialEq, Eq)]
    pub enum InputType {
        Erl,
    }
}

arg_enum!{
    #[derive(Debug)]
    pub enum CompileLevel {
        High,
        Normal,
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

fn handle_erl(in_str: &str, matches: &ArgMatches) -> Option<Module> {
    let mut config = ParseConfig::default();

    if let Some(includes) = matches.values_of("INCLUDE_PATHS") {
        for include in includes {
            config.include_paths.push_front(PathBuf::from(include));
        }
    }
    if let Some(includes) = matches.values_of("CODE_PATHS") {
        for include in includes {
            config.code_paths.push_front(PathBuf::from(include));
        }
    }

    let parser = Parser::new(config);

    match parser.parse_string(in_str) {
        Ok(ast) => {
            let (res, messages) = lower_module(&ast);

            let emitter = StandardStreamEmitter::new(ColorChoice::Auto)
                .set_codemap(parser.config.codemap.clone());
            for msg in messages.iter() {
                emitter.diagnostic(&msg.to_diagnostic()).unwrap();
            }

            res.ok()
        }
        Err(err) => {
            let emitter = StandardStreamEmitter::new(ColorChoice::Auto)
                .set_codemap(parser.config.codemap.clone());
            for msg in err.iter() {
                emitter.diagnostic(&msg.to_diagnostic()).unwrap();
            }

            None
        }
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
        .arg(Arg::from_usage("<IN_FORMAT> -f,--in-format <IN_FORMAT> 'input format'")
             .default_value("erl")
             .required(true)
             .case_insensitive(true)
             .possible_values(&InputType::variants()))
        .arg(Arg::from_usage("<OUT_FORMAT> -p,--out-format <OUT_FORMAT> 'output format'")
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
        .arg(Arg::from_usage("[ANNOTATE_LIVE] --annotate-live 'annotate calculated live variables in ir"))
        .arg(Arg::from_usage("<INCLUDE_PATHS> -I <INCLUDE_PATH> 'add include path for the erlang preprocessor'")
             .required(false)
             .multiple(true))
        .arg(Arg::from_usage("<CODE_PATHS> -C <CODE_PATH> 'add code path for the erlang preprocessor'")
             .required(false)
             .multiple(true))
        //.arg(Arg::from_usage("-p,--pass <PASS> 'run the given compilation pass'")
        //     .multiple(true)
        //     .possible_values(&CompilePass::variants()))
        .get_matches();

    let in_file_name = matches.value_of("IN_FILE").unwrap();

    let mut in_str = String::new();
    std::fs::File::open(&in_file_name).unwrap()
        .read_to_string(&mut in_str).unwrap();

    let eir_ret = match value_t!(matches, "IN_FORMAT", InputType).unwrap() {
        InputType::Erl => handle_erl(&in_str, &matches),
    };

    let mut eir = if let Some(eir) = eir_ret {
        eir
    } else {
        return;
    };

    match value_t!(matches, "COMPILE_LEVEL", CompileLevel).unwrap() {
        CompileLevel::High => {},
        CompileLevel::Normal => {
            let mut pass_manager = PassManager::default();
            pass_manager.run(&mut eir);
        },
    }

    let selected_function = matches.value_of("FUN_IDENT")
        .map(|val| FunctionIdent::parse_with_module(
            val, eir.name.clone()).unwrap());

    let mut print_ctx = ToEirTextContext::new();
    //if matches.is_present("ANNOTATE_LIVE") {
    //    print_ctx.add_annotator(EirLiveValuesAnnotator::new());
    //}

    let mut out_data = Vec::new();
    let out_ext;
    let out_type = value_t!(matches, "OUT_FORMAT", OutputType).unwrap();
    match out_type {
        OutputType::Eir => {
            if let Some(selected) = selected_function {
                let fun = &eir.functions[&selected];
                fun.to_eir_text(&mut print_ctx, 0, &mut out_data).unwrap();
            } else {
                eir.to_eir_text(&mut print_ctx, 0, &mut out_data).unwrap();
            }
            out_ext = "eir";
        },
        OutputType::Dot => {
            let selected_function = selected_function.expect(
                "Expected function ident with -i <FUN_IDENT>");
            let fun = &eir.functions[&selected_function];

            ::libeir_ir::text::function_to_dot(&fun, &mut out_data).unwrap();

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
