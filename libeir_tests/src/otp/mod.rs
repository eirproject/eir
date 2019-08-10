use std::path::PathBuf;

use crate::{ ParseConfig, lower_file };
use crate::ct_runner::run_ct_suite;

use libeir_intern::Ident;
use libeir_passes::PassManager;

use libeir_interpreter::{ VMState, Term };

#[test]
fn compiler() {
    let mut config = ParseConfig::default();

    config.include_paths.push_front(PathBuf::from("../otp/lib/compiler/src/"));
    config.include_paths.push_front(PathBuf::from("../otp/bootstrap/lib/stdlib/include/"));

    let mut eir_mod = lower_file(
        "../otp/lib/compiler/src/compile.erl", config).unwrap();

    for fun in eir_mod.functions.values() {
        fun.graph_validate_global();
    }

    let mut pass_manager = PassManager::default();
    pass_manager.run(&mut eir_mod);
}

#[test]
fn beam_disasm() {
    let mut config = ParseConfig::default();

    config.include_paths.push_front(PathBuf::from("../otp/lib/compiler/src/"));
    config.include_paths.push_front(PathBuf::from("../otp/bootstrap/lib/stdlib/include/"));

    let mut eir_mod = lower_file(
        "../otp/lib/compiler/src/beam_disasm.erl", config).unwrap();

    for fun in eir_mod.functions.values() {
        fun.graph_validate_global();
    }

    let mut pass_manager = PassManager::default();
    pass_manager.run(&mut eir_mod);

}

#[test]
fn core_parse() {
    let mut config = ParseConfig::default();

    config.include_paths.push_front(PathBuf::from("../otp/lib/compiler/src/"));
    config.include_paths.push_front(PathBuf::from("../otp/bootstrap/lib/stdlib/include/"));

    let mut eir_mod = lower_file(
        "../otp/lib/compiler/src/core_parse.erl", config).unwrap();

    for fun in eir_mod.functions.values() {
        fun.graph_validate_global();
    }

    let mut pass_manager = PassManager::default();
    pass_manager.run(&mut eir_mod);
}

#[test]
fn maps() {
    let config = ParseConfig::default();
    let mut eir_mod = lower_file(
        "../otp/lib/stdlib/src/maps.erl", config).unwrap();

    for fun in eir_mod.functions.values() {
        fun.graph_validate_global();
    }

    let mut pass_manager = PassManager::default();
    pass_manager.run(&mut eir_mod);
}

#[test]
fn match_suite() {
    let mut config = ParseConfig::default();
    config.code_paths.push_front(PathBuf::from("../otp/lib/"));

    let mut eir_mod = lower_file(
        "../otp/lib/compiler/test/match_SUITE.erl", config).unwrap();

    for fun in eir_mod.functions.values() {
        fun.graph_validate_global();
    }

    let mut pass_manager = PassManager::default();
    pass_manager.run(&mut eir_mod);

    let mut vm = VMState::new();
    vm.add_builtin_modules();
    vm.add_erlang_module(eir_mod);

    run_ct_suite(&mut vm, Ident::from_str("match_SUITE"));
}

#[test]
fn bs_match_suite() {
    let mut config = ParseConfig::default();
    config.code_paths.push_front(PathBuf::from("../otp/lib/"));

    let mut eir_mod = lower_file(
        "../otp_build/bs_match_SUITE_patched.erl", config).unwrap();

    for fun in eir_mod.functions.values() {
        fun.graph_validate_global();
    }

    let mut pass_manager = PassManager::default();
    pass_manager.run(&mut eir_mod);

    let mut vm = VMState::new();
    vm.add_builtin_modules();
    vm.add_erlang_module(eir_mod);

    run_ct_suite(&mut vm, Ident::from_str("bs_match_SUITE"));
}

//#[test]
fn maps_suite() {
    let config = ParseConfig::default();
    let mut maps_eir_mod = lower_file(
        "../otp/lib/stdlib/src/maps.erl", config).unwrap();

    let config = ParseConfig::default();
    let mut eir_mod = lower_file(
        "../otp/lib/compiler/test/map_SUITE.erl", config).unwrap();

    for fun in maps_eir_mod.functions.values() {
        fun.graph_validate_global();
    }
    for fun in eir_mod.functions.values() {
        fun.graph_validate_global();
    }

    let mut pass_manager = PassManager::default();
    pass_manager.run(&mut maps_eir_mod);
    pass_manager.run(&mut eir_mod);

    let mut vm = VMState::new();
    vm.add_builtin_modules();
    vm.add_erlang_module(eir_mod);
    vm.add_erlang_module(maps_eir_mod);

    run_ct_suite(&mut vm, Ident::from_str("map_SUITE"));
}

#[test]
fn xmerl_scan() {
    let mut config = ParseConfig::default();
    config.code_paths.push_front(PathBuf::from("../otp/lib/"));
    config.include_paths.push_front(PathBuf::from("../otp/lib/xmerl/src/"));
    config.include_paths.push_front(PathBuf::from("../otp/lib/xmerl/include/"));

    let mut eir_mod = lower_file(
        "../otp/lib/xmerl/src/xmerl_scan.erl", config).unwrap();

    for fun in eir_mod.functions.values() {
        fun.graph_validate_global();
    }

    let mut pass_manager = PassManager::default();
    pass_manager.run(&mut eir_mod);
}

#[test]
fn xmerl_sax_parser_utf8() {
    let mut config = ParseConfig::default();
    config.code_paths.push_front(PathBuf::from("../otp/lib/"));
    config.include_paths.push_front(PathBuf::from("../otp/lib/xmerl/src/"));
    config.include_paths.push_front(PathBuf::from("../otp/lib/xmerl/include/"));

    let mut eir_mod = lower_file(
        "../otp/lib/xmerl/src/xmerl_sax_parser_latin1.erl", config).unwrap();

    for fun in eir_mod.functions.values() {
        fun.graph_validate_global();
    }

    let mut pass_manager = PassManager::default();
    pass_manager.run(&mut eir_mod);
}

#[test]
fn foo() {
    let config = ParseConfig::default();
    let mut eir_mod = lower_file(
        "foo.erl", config).unwrap();

    for fun in eir_mod.functions.values() {
        fun.graph_validate_global();
    }

    let mut pass_manager = PassManager::default();
    pass_manager.run(&mut eir_mod);

    panic!("{:?}");
}
