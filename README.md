# Eir Project
Erlang compiler and IR implemented in Rust

## Details

Current features:
* Unified Thorin based IR representation named Eir
* Erlang frontend
* Core frontend
* Pattern match compilation passes
* Basic optimization and cleanup passes
* Naive interpreter used for testing

The project is split into several crates:
* `libeir_ir` - Contains the core Eir IR data structures, validation, printing
* `libeir_syntax_erl` - Frontend for Erlang, lowers to Eir.
* `libeir_syntax_core` - Frontend for Core Erlang, lowers to Eir.
* `libeir_passes` - Compiler passes operating on Eir.
* `libeir_lowerutils` - Utilities for lowering Eir to SSA form.
* `libeir_interpreter` - Naive interpreter for Eir. Used to run OTP test suites.
* `libeir_intern` - Symbol interning. Used by most other crates.
* `libeir_diagnostics` - Source span handling and diagnostics printing.
* `libeir_util` - Kitchen sink of utilities used by other crates.
* `pattern-compiler` - A generic pattern matching tree compiler.
* `tools` - CLI tools used to work with the IR.
