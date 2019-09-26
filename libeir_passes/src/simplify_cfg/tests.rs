// 1. Go through all live + read values in the target
// 2. If the value is already processed, skip
// 2. Go through all nested values, add to process list
// 3. If the value is not relevant to the phi, skip
// 4. Walk and merge phi nodes
// 5. Add phi destinations to process list
// 6. If all destinations have the same from value, add to static map
//    else add to cond map




use libeir_ir::{FunctionIdent, Function};
use libeir_intern::{Ident, Symbol};

use crate::FunctionPass;
use super::SimplifyCfgPass;

#[test]
fn primop_in_chain() {

    let ident = FunctionIdent {
        module: Ident::from_str("foo"),
        name: Ident::from_str("foo"),
        arity: 0,
    };
    let mut function = Function::new(ident);

    let mut b = function.builder();

    let entry = b.block_insert();
    let entry_arg_1 = b.block_arg_insert(entry);
    let entry_arg_2 = b.block_arg_insert(entry);
    b.block_set_entry(entry);

    let false_val = b.prim_value_list(&[]);
    let block_1 = b.op_unpack_value_list(entry, false_val, 0);

    let block_2 = b.block_insert();
    let block_2_arg_1 = b.block_arg_insert(block_2);

    let block_3 = b.block_insert();
    let block_3_arg_1 = b.block_arg_insert(block_3);

    b.op_call(block_1, block_2, &[entry_arg_2]);

    let tup = b.prim_tuple(&[block_2_arg_1]);
    b.op_call(block_2, block_3, &[tup]);

    b.op_call(block_3, entry_arg_1, &[block_3_arg_1]);

    println!("{}", b.fun().to_text());

    let mut simplify_cfg_pass = SimplifyCfgPass::new();
    simplify_cfg_pass.run_function_pass(&mut b);

    println!("{}", b.fun().to_text());

    let mut out = Vec::new();
    b.fun().validate(&mut out);
    println!("{:?}", out);
    assert!(out.len() == 0);

}

#[test]
fn tail_call_elimination() {
    let ident = FunctionIdent {
        module: Ident::from_str("foo"),
        name: Ident::from_str("foo"),
        arity: 0,
    };
    let mut function = Function::new(ident);

    let mut b = function.builder();

    let entry = b.block_insert();
    let entry_arg_1 = b.block_arg_insert(entry);
    let entry_arg_2 = b.block_arg_insert(entry);
    b.block_set_entry(entry);

    let block_2 = b.block_insert();
    let block_2_val = b.value(block_2);
    let block_3 = b.block_insert();
    let block_3_val = b.value(block_3);

    let block_1 = b.op_capture_function(entry, Symbol::intern("foo"),
                                        Symbol::intern("foo"), 0);
    let block_1_arg_1 = b.fun().block_args(block_1)[0];
    b.op_call(block_1, block_1_arg_1, &[block_2_val, block_3_val]);

    let block_2_arg_1 = b.block_arg_insert(block_2);
    b.op_call(block_2, entry_arg_1, &[block_2_arg_1]);

    let block_3_arg_1 = b.block_arg_insert(block_3);
    let block_3_arg_2 = b.block_arg_insert(block_3);
    let block_3_arg_3 = b.block_arg_insert(block_3);
    b.op_call(block_3, entry_arg_2, &[block_3_arg_1, block_3_arg_2, block_3_arg_3]);

    println!("{}", b.fun().to_text());

    let mut simplify_cfg_pass = SimplifyCfgPass::new();
    simplify_cfg_pass.run_function_pass(&mut b);

    println!("{}", b.fun().to_text());

    let mut out = Vec::new();
    b.fun().validate(&mut out);
    println!("{:?}", out);
    assert!(out.len() == 0);
}
