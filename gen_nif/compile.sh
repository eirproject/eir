#!/bin/bash

set -e

erlc +to_core testing.erl
cargo run

echo "Compiling proto_nif.c"
clang -O0 -S -emit-llvm proto_nif.c
llc-7 -relocation-model=pic -filetype=obj proto_nif.ll

echo "Disassembling module.bc"
llvm-dis-7 module.bc

echo "Compiling module.bc"
llc-7 -relocation-model=pic -filetype=obj module.ll

echo "Linking"
clang -fPIC -shared -o woohoo.so proto_nif.o module.o

echo "Running"
elixir testing.exs
