use std::path::PathBuf;

use crate::{ ParseConfig, lower_file, lower };
use crate::ct_runner::run_ct_suite;

use libeir_intern::Ident;
use libeir_passes::PassManager;

use libeir_interpreter::{ VMState };

use libeir_lowerutils::analyze;

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

    for fun in eir_mod.functions.values() {
        analyze(fun);
    }
}

#[ignore]
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

#[ignore]
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

#[ignore]
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

#[ignore]
#[test]
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

#[ignore]
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

#[ignore]
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


#[test]
fn unary_op_1() {
    let text = "
-module(foo).

unary_op_1(Vop@1) ->
    %% If all optimizations are working as they should, there should
    %% be no stack frame and all '=:=' tests should be coalesced into
    %% a single select_val instruction.

    case Vop@1 =:= '&' of
        true ->
            {non_associative,30};
        false ->
            case
                case Vop@1 =:= '^' of
                    true ->
                        true;
                    false ->
                        case Vop@1 =:= 'not' of
                            true ->
                                true;
                            false ->
                                case Vop@1 =:= '+' of
                                    true ->
                                        true;
                                    false ->
                                        case Vop@1 =:= '-' of
                                            true ->
                                                true;
                                            false ->
                                                case Vop@1 =:= '~~~' of
                                                    true ->
                                                        true;
                                                    false ->
                                                        Vop@1 =:= '!'
                                                end
                                        end
                                end
                        end
                end
            of
                true ->
                    {non_associative,300};
                false ->
                    case Vop@1 =:= '@' of
                        true ->
                            {non_associative,320};
                        false ->
                            error
                    end
            end
    end.
";

    let config = ParseConfig::default();
    let mut eir_mod = lower(text, config).unwrap();

    for fun in eir_mod.functions.values() {
        fun.graph_validate_global();
    }

    let mut pass_manager = PassManager::default();
    pass_manager.run(&mut eir_mod);

    for fun in eir_mod.functions.values() {
        fun.live_values();
    }

}

#[test]
fn unary_op_1_a() {
    let text = "
-module(foobarrr).

unary_op_1(Vop@1) ->
    case
        case Vop@1 =:= '^' of
            true ->
                a;
            false ->
                b
        end
    of
        true ->
            c
    end.
";

    let config = ParseConfig::default();
    let mut eir_mod = lower(text, config).unwrap();

    for fun in eir_mod.functions.values() {
        fun.graph_validate_global();
    }

    let mut pass_manager = PassManager::default();
    pass_manager.run(&mut eir_mod);

    for fun in eir_mod.functions.values() {
        fun.live_values();
    }

}
