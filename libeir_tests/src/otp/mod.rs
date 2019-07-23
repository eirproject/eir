use crate::{ ParseConfig, lower_file };
use std::path::PathBuf;

#[test]
fn compiler() {
    let mut config = ParseConfig::default();

    config.include_paths.push_front(PathBuf::from("../otp/lib/compiler/src/"));
    config.include_paths.push_front(PathBuf::from("../otp/bootstrap/lib/stdlib/include/"));

    let _ir = lower_file("../otp/lib/compiler/src/compile.erl", config).unwrap();
}

#[test]
fn beam_disasm() {
    let mut config = ParseConfig::default();

    config.include_paths.push_front(PathBuf::from("../otp/lib/compiler/src/"));
    config.include_paths.push_front(PathBuf::from("../otp/bootstrap/lib/stdlib/include/"));

    let _ir = lower_file("../otp/lib/compiler/src/beam_disasm.erl", config).unwrap();
}

#[test]
fn core_parse() {
    let mut config = ParseConfig::default();

    config.include_paths.push_front(PathBuf::from("../otp/lib/compiler/src/"));
    config.include_paths.push_front(PathBuf::from("../otp/bootstrap/lib/stdlib/include/"));

    let _ir = lower_file("../otp/lib/compiler/src/core_parse.erl", config).unwrap();
}

#[test]
fn maps() {
    let mut config = ParseConfig::default();

    let _ir = lower_file("../otp/lib/stdlib/src/maps.erl", config).unwrap();
}
