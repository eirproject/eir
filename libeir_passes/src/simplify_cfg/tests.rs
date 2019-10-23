use crate::FunctionPass;
use super::SimplifyCfgPass;

use libeir_ir::parse_function_unwrap;

#[test]
fn primop_in_chain() {

    let mut fun = parse_function_unwrap("
foo:bar/1 {
    entry(%ret, %thr, %a):
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
        %ret({%a});
}
");

    assert!(b.fun().graph_eq(b.fun().block_entry(), &after, after.block_entry()).is_ok());

}

#[test]
fn double_primop_in_chain() {

    let mut fun = parse_function_unwrap("
foo:bar/1 {
    entry(%ret, %thr, %a):
        b2(%a);
    b2(%b):
        b3({%b});
    b3(%c):
        b4({%c});
    b4(%d):
        %ret(%d);
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
        %ret({{%a}});
}
");

    assert!(b.fun().graph_eq(b.fun().block_entry(), &after, after.block_entry()).is_ok());

}

#[test]
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
foo:bar/3 {
    entry(%ret, %thr, %a, %b, %c):
        if_bool %a b_true b_false;
    b_true():
        %ret({{%b}});
    b_false():
        %ret({{%c}});
}
");

    assert!(b.fun().graph_eq(b.fun().block_entry(), &after, after.block_entry()).is_ok());

}

#[test]
fn two_split_primop_in_chain() {

    let mut fun = parse_function_unwrap("
foo:bar/5 {
    entry(%ret, %thr, %a, %b, %c, %B, %C):
        if_bool %a b_true b_false;

    b_true():
        b_true1(%b, %B);
      b_true1(%bb, %BB):
        b_join({%bb}, {%BB});

    b_false():
        b_false1(%c, %C);
      b_false1(%cc, %CC):
        b_join({%cc}, {%CC});

    b_join(%d, %e):
        b_join1({%d}, {%e});
      b_join1(%dd, %ee):
        %ret({%dd, %ee});
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
foo:bar/5 {
    entry(%ret, %thr, %a, %b, %c, %B, %C):
        if_bool %a b_true b_false;
    b_true():
        %ret({{{%b}}, {{%B}}});
    b_false():
        %ret({{{%c}}, {{%C}}});
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
        a'foo':a'foo'/0(%ret, %thr);
}
");

    assert!(b.fun().graph_eq(b.fun().block_entry(), &after, after.block_entry()).is_ok());

}

#[test]
fn recursive_simplification() {

    let mut fun = parse_function_unwrap("
foo:bar/0 {
    entry(%ret, %thr):
        b(a'true');
    b(%sec):
        if_bool %sec b_true b_false;
    b_false():
        unreachable;
    b_true():
        %ret(a'yay');
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
        %ret(a'yay');
}
");

    assert!(b.fun().graph_eq(b.fun().block_entry(), &after, after.block_entry()).is_ok());
}

#[test]
fn value_list_removal() {

    let mut fun = parse_function_unwrap("
foo:bar/2 {
    entry(%ret, %thr, %a, %b):
        unpack <%a, %b> arity 2 => cont;
    cont(%aa, %bb):
        %ret(%aa);
}
");
    let mut b = fun.builder();

    let mut simplify_cfg_pass = SimplifyCfgPass::new();
    simplify_cfg_pass.run_function_pass(&mut b);

    let mut out = Vec::new();
    b.fun().validate(&mut out);
    assert!(out.len() == 0);

    let after = parse_function_unwrap("
foo:bar/2 {
    entry(%ret, %thr, %a, %b):
        %ret(%a);
}
");

    assert!(b.fun().graph_eq(b.fun().block_entry(), &after, after.block_entry()).is_ok());
}

#[test]
fn partial_loop() {

let mut fun = parse_function_unwrap("
foo:bar/1 {
    block1(%3, %4, %5):
        block8(%5, []);
    block8(%30, %31):
        match %30 {
            [] => block11;
        };
    block11(%36, %37):
        block28(%36);
    block28(%81):
        block17(%81);
    block17(%53):
        %54 = [%53 | %31];
        block8(%37, %54);
}
");
    let mut b = fun.builder();

    let mut out = Vec::new();
    b.fun().validate(&mut out);
    println!("{:?}", out);
    assert!(out.len() == 0);

    let mut simplify_cfg_pass = SimplifyCfgPass::new();
    simplify_cfg_pass.run_function_pass(&mut b);

    b.fun().live_values();

    let mut out = Vec::new();
    b.fun().validate(&mut out);
    assert!(out.len() == 0);

}

#[test]
fn tight_partial_loop() {
    let mut fun = parse_function_unwrap("
foo:perms/1 {
    block0(%1, %2, %3):
        block1(%3, []);
    block1(%5, %6):
        block2();
    block2():
        match %5 {
            [] => block3;
        };
    block3(%9, %10):
        %14 = [%9 | %6];
        block1(%10, %14);
}
");
    let mut b = fun.builder();

    let mut out = Vec::new();
    b.fun().validate(&mut out);
    println!("{:?}", out);
    assert!(out.len() == 0);

    let mut simplify_cfg_pass = SimplifyCfgPass::new();
    simplify_cfg_pass.run_function_pass(&mut b);

    b.fun().live_values();

    let mut out = Vec::new();
    b.fun().validate(&mut out);
    assert!(out.len() == 0);
}

#[test]
fn aa() {
    let mut fun = parse_function_unwrap("
foo:do_map_vars_used/1 {
    block0(%1, %2, %3):
        block1(%3);
    block1(%5):
        block2(%5);
    block2(%7):
        %12 = {%7};
        block3(%12);
    block3(%9):
        block4(%9);
    block4(%11):
        %13 = <>;
        match %11 {};
}
");
    let mut b = fun.builder();

    let mut out = Vec::new();
    b.fun().validate(&mut out);
    println!("{:?}", out);
    assert!(out.len() == 0);

    b.fun().live_values();

    let mut simplify_cfg_pass = SimplifyCfgPass::new();
    simplify_cfg_pass.run_function_pass(&mut b);

    b.fun().live_values();

    let mut out = Vec::new();
    b.fun().validate(&mut out);
    assert!(out.len() == 0);

}
