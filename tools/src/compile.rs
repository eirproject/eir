use clap::{Arg, App, ArgMatches, arg_enum, value_t, values_t};

use std::io::Write;
use std::path::{Path, PathBuf};

use libeir_ir::FunctionIdent;

use libeir_diagnostics::{
    ColorChoice, Emitter, StandardStreamEmitter
};
use libeir_passes::PassManager;

use libeir_frontend::{
    AnyFrontend, DynFrontend,
    erlang::ErlangFrontend,
    abstr_erlang::AbstrErlangFrontend,
    eir::EirFrontend,
};
use libeir_util_parse::ArcCodemap;

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
        Eir,
        Abstr,
        Erl,
    }
}

arg_enum!{
    #[derive(Debug)]
    pub enum CompileLevel {
        High,
        Normal,
        Custom,
    }
}

arg_enum!{
    #[derive(Debug)]
    pub enum CompilePass {
        CompilePatterns,
        SimplifyCfg,
        NaiveInlineClosures,
        Validate,
    }
}

arg_enum!{
    #[derive(Debug, Copy, Clone)]
    pub enum LogLevel {
        Error,
        Warn,
        Info,
        Debug,
        Trace,
    }
}
impl LogLevel {
    pub fn to_filter(self) -> log::LevelFilter {
        match self {
            LogLevel::Error => log::LevelFilter::Error,
            LogLevel::Warn => log::LevelFilter::Warn,
            LogLevel::Info => log::LevelFilter::Info,
            LogLevel::Debug => log::LevelFilter::Debug,
            LogLevel::Trace => log::LevelFilter::Trace,
        }
    }
}

fn make_erlang_frontend(matches: &ArgMatches) -> ErlangFrontend {
    use libeir_syntax_erl::ParseConfig;

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

    ErlangFrontend::new(config)
}

fn make_frontend(matches: &ArgMatches) -> AnyFrontend {
    match value_t!(matches, "IN_FORMAT", InputType).unwrap() {
        InputType::Erl => make_erlang_frontend(matches).into(),
        InputType::Abstr => AbstrErlangFrontend::new().into(),
        InputType::Eir => EirFrontend::new().into(),
    }
}

fn setup_logger(level: log::LevelFilter) {
    fern::Dispatch::new()
        .format(|out, message, record| {
            out.finish(format_args!(
                "[{}][{}] {}",
                record.target(),
                record.level(),
                message
            ))
        })
        .level(level)
        .chain(std::io::stdout())
        .apply()
        .unwrap();
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
        .arg(Arg::from_usage("<PASSES> --pass <PASS> 'run the given compilation pass'")
             .required(false)
             .multiple(true)
             .number_of_values(1)
             .possible_values(&CompilePass::variants()))
        .arg(Arg::from_usage("<LOG_LEVEL> -L,--log-level <LOG_LEVEL> 'log level'")
             .default_value("info")
             .required(false)
             .case_insensitive(true)
             .possible_values(&LogLevel::variants()))
        .get_matches();

    setup_logger(value_t!(matches, "LOG_LEVEL", LogLevel).unwrap().to_filter());

    let frontend = make_frontend(&matches);

    let in_file_name = matches.value_of("IN_FILE").unwrap();
    let in_file_path = Path::new(in_file_name);

    let codemap = ArcCodemap::default();
    let (eir_res, diagnostics) = frontend.parse_file_dyn(codemap.clone(), &in_file_path);

    let emitter = StandardStreamEmitter::new(ColorChoice::Auto)
        .set_codemap(codemap.clone());
    for diag in diagnostics.iter() {
        emitter.diagnostic(diag).unwrap();
    }

    if eir_res.is_err() {
        return;
    }
    let mut eir = eir_res.unwrap();

    match value_t!(matches, "COMPILE_LEVEL", CompileLevel).unwrap() {
        CompileLevel::High => {},
        CompileLevel::Normal => {
            let mut pass_manager = PassManager::default();
            pass_manager.run(&mut eir);
        },
        CompileLevel::Custom => {
            let mut pass_manager = PassManager::new();
            if matches.is_present("PASSES") {
                for pass_type in values_t!(matches.values_of("PASSES"), CompilePass).unwrap() {
                    match pass_type {
                        CompilePass::CompilePatterns => {
                            pass_manager.push_function_pass(
                                libeir_passes::CompilePatternPass::new());
                        },
                        CompilePass::SimplifyCfg => {
                            pass_manager.push_function_pass(
                                libeir_passes::SimplifyCfgPass::new());
                        },
                        CompilePass::NaiveInlineClosures => {
                            pass_manager.push_function_pass(
                                libeir_passes::NaiveInlineClosuresPass::new());
                        },
                        CompilePass::Validate => {
                            pass_manager.push_function_pass(
                                libeir_passes::ValidatePass::new());
                        },
                    }
                }
            }
            pass_manager.run(&mut eir);
        },
    }

    let selected_function = matches.value_of("FUN_IDENT")
        .map(|val| FunctionIdent::parse_with_module(
            val, eir.name().clone()).unwrap());

    //if matches.is_present("ANNOTATE_LIVE") {
    //    print_ctx.add_annotator(EirLiveValuesAnnotator::new());
    //}

    let out_data;
    let out_ext;
    let out_type = value_t!(matches, "OUT_FORMAT", OutputType).unwrap();
    match out_type {
        OutputType::Eir => {
            if let Some(selected) = selected_function {
                out_data = eir[&selected].function().to_text_standard();
            } else {
                out_data = eir.to_text_standard();
            }
            out_ext = "eir";
        },
        OutputType::Dot => {
            let selected_function = selected_function.expect(
                "Expected function ident with -i <FUN_IDENT>");
            let fun_def = &eir[&selected_function];
            let fun = fun_def.function();

            out_data = ::libeir_ir::text::function_to_dot(&fun);

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
    out.write(out_data.as_bytes()).unwrap();

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
