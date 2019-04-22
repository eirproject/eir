# core_erlang
Core Erlang compiler implemented in Rust

Pictured below: CFG for `proplists:get_value/3`
![Example CFG](example_cfg.png)

## Details

Current area of work: NIF generation proof of concept ([niffy](https://github.com/eirproject/niffy))

The project is split into several crates:
* `eir` - Contains the core IR data structures, validation, printing
* `compiler` - The CORE Erlang -> Eir compiler
* `gen_nif` - Eir -> LLVM backend. Can emit code for both NIFs and WASM, others easily addable.
* `interpreter` - Reference interpreter for Eir
* `pattern-compiler` - Generic implementation of [this paper](https://www.cs.tufts.edu/~nr/cs257/archive/luc-maranget/jun08.pdf). Used by pattern compilation Eir pass.
* `tools` - Various command line utilities. (dot generation, eir text generation)
* `util` - Utilities used by several other crates
* `cps_transform` - Transforms normal Eir to CPS form Eir

Other directories:
* `otp_build` - Scripts for building parts of OTP directly to `.core` files
* `test_data` - Data used for test suites

Current features:
* Compiles Core erlang to Eir (SSA resolving, lambda lifting)
* Pattern match compilation
* Simple optimizations on Eir
* Performs CPS transformation
* Various IR analysis utilities
* Printing of Eir (both textual and dot graph)
* Executes (part of, for now) Eir in reference interpreter
* Generates LLVM IR for compilation into a NIF
