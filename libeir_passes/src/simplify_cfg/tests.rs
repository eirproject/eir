use crate::FunctionPass;
use super::SimplifyCfgPass;

use libeir_ir::parse_function_unwrap;

#[test]
fn primop_in_chain() {

    let mut fun = parse_function_unwrap("
foo:bar/1 {
    entry(%ret, %thr, %a):
        unpack <> arity 0 => b1;
    b1():
        b2(%a);
    b2(%b):
        %prim = {%b};
        b3(%prim);
    b3(%c):
        %ret(%c);
}
");
    let mut b = fun.builder();

    let mut simplify_cfg_pass = SimplifyCfgPass::new();
    simplify_cfg_pass.run_function_pass(&mut b);

    let mut out = Vec::new();
    b.fun().validate(&mut out);
    assert!(out.len() == 0);

    let after = parse_function_unwrap("
foo:bar/1 {
    entry(%ret, %thr, %a):
        unpack <> arity 0 => b1;
    b1():
        %ret({%a});
}
");

    assert!(b.fun().graph_eq(b.fun().block_entry(), &after, after.block_entry()).is_ok());

}

#[test]
#[ignore]
fn split_primop_in_chain() {

    let mut fun = parse_function_unwrap("
foo:bar/3 {
    entry(%ret, %thr, %a, %b, %c):
        if_bool %a b_true b_false;

    b_true():
        b_true1(%b);
      b_true1(%bb):
        b_join({%bb});

    b_false():
        b_false1(%c);
      b_false1(%cc):
        b_join({%cc});

    b_join(%d):
        b_join1({%d});
      b_join1(%dd):
        %ret(%dd);
}
");
    let mut b = fun.builder();

    let mut simplify_cfg_pass = SimplifyCfgPass::new();
    simplify_cfg_pass.run_function_pass(&mut b);

    println!("{}", b.fun().to_text());

    let mut out = Vec::new();
    b.fun().validate(&mut out);
    assert!(out.len() == 0);

    let after = parse_function_unwrap("
foo:bar/1 {
    entry(%ret, %thr, %a):
        unpack <> arity 0 => b1;
    b1():
        %ret({%a});
}
");

    assert!(b.fun().graph_eq(b.fun().block_entry(), &after, after.block_entry()).is_ok());

}

#[test]
fn tail_call_elimination() {

    let mut fun = parse_function_unwrap("
foo:bar/0 {
    entry(%ret, %thr):
        b1(a'foo':a'foo'/0);
    b1(%fun):
        %fun(b2, b3);
    b2(%ret_arg):
        %ret(%ret_arg);
    b3(%thr_a, %thr_b, %thr_c):
        %thr(%thr_a, %thr_b, %thr_c);
}
");
    let mut b = fun.builder();

    let mut simplify_cfg_pass = SimplifyCfgPass::new();
    simplify_cfg_pass.run_function_pass(&mut b);

    let mut out = Vec::new();
    b.fun().validate(&mut out);
    assert!(out.len() == 0);

    let after = parse_function_unwrap("
foo:bar/0 {
    entry(%ret, %thr):
        b1(a'foo':a'foo'/0);
    b1(%a):
        %a(%ret, %thr);
}
");

    assert!(b.fun().graph_eq(b.fun().block_entry(), &after, after.block_entry()).is_ok());

}
