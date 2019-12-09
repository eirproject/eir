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

    let mut simplify_cfg_pass = SimplifyCfgPass::new();
    simplify_cfg_pass.run_function_pass(&mut b);

    let after = parse_function_unwrap("
foo:bar/1 {
    block1(%3, %4, %5):
        block8(%5, []);
    block8(%30, %31):
        match %30 {
            [] => block11;
        };
    block11(%36, %37):
        %54 = [%36 | %31];
        block8(%37, %54);
}
");
    assert!(b.fun().graph_eq(b.fun().block_entry(), &after, after.block_entry()).is_ok());
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

    let mut simplify_cfg_pass = SimplifyCfgPass::new();
    simplify_cfg_pass.run_function_pass(&mut b);

    let after = parse_function_unwrap("
foo:perms/1 {
    block0(%1, %2, %3):
        block1(%3, []);
    block1(%5, %6):
        match %5 {
            [] => block3;
        };
    block3(%9, %10):
        %14 = [%9 | %6];
        block1(%10, %14);
}
");
    assert!(b.fun().graph_eq(b.fun().block_entry(), &after, after.block_entry()).is_ok());
}

#[test]
fn deep_primop_rename_single_branch() {
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

    let mut simplify_cfg_pass = SimplifyCfgPass::new();
    simplify_cfg_pass.run_function_pass(&mut b);

    let after = parse_function_unwrap("
foo:do_map_vars_used/1 {
    entry(%1, %2, %3):
        %4 = {%3};
        match %4 {};
}
");
    assert!(b.fun().graph_eq(b.fun().block_entry(), &after, after.block_entry()).is_ok());

}

#[test]
fn deep_primop_rename_after_entry_single_branch() {
    let mut fun = parse_function_unwrap("
foo:do_map_vars_used/1 {
    block0(%1, %2, %3):
        block1(%3);
    block1(%5):
        block2(%5);
    block2(%7):
        %15 = {%7};
        block3(%15);
    block3(%9):
        block4(%9);
    block4(%11):
        %18 = <block5>;
        %16 = <>;
        match [] {
            type %{} => block5;
        };
    block5():
        %19 = <block6>;
        match [] {
            %{ %11} => block6;
        };
    block6(%14):
        unreachable;
}
");
    let mut b = fun.builder();

    let mut simplify_cfg_pass = SimplifyCfgPass::new();
    simplify_cfg_pass.run_function_pass(&mut b);

    let after = parse_function_unwrap("
foo:do_map_vars_used/1 {
    entry(%1, %2, %3):
        match [] {
            type %{} => block1;
        };
    block1():
        %4 = {%3};
        match [] {
            %{%4} => block2;
        };
    block2(%5):
        unreachable;
}
");
    assert!(b.fun().graph_eq(b.fun().block_entry(), &after, after.block_entry()).is_ok());
}

#[ignore]
#[test]
fn messy_cfg_block_captures() {
    let mut fun = parse_function_unwrap("
foo:grab_bag/0 {
    block0(%1, %2):
        block1(block2);
    block1(%4):
        block2(block3);
    block2(%6):
        match a'x' {
            _ => block4;
        };
    block3(%8):
        %1(a'ok');
    block4():
        %6(a'x');
}
");
    let mut b = fun.builder();

    let mut simplify_cfg_pass = SimplifyCfgPass::new();
    simplify_cfg_pass.run_function_pass(&mut b);

    b.fun().live_values();

    let mut out = Vec::new();
    b.fun().validate(&mut out);
    assert!(out.len() == 0);

    let after = parse_function_unwrap("
foo:grab_bag/0 {
    block0(%1, %2):
        match a'x' {
            _ => block1;
        };
    block1():
        block2(a'x');
    block2(%21):
        %1(a'ok');
}
");
    assert!(b.fun().graph_eq(b.fun().block_entry(), &after, after.block_entry()).is_ok());
}

#[test]
fn converging_from_single() {

    let mut fun = parse_function_unwrap("
fib:fib/1 {
    block21(%77, %78, %114):
        %64 = a'erlang':a'<'/2;
        %64(%114, 2) => block36 except block50;
    block50(%144, %145, %146):
        block36(a'false');
    block36(%116):
        if_bool %116 block38 block39;
    block38():
        unreachable;
    block39():
        unreachable;
}
");
    let mut b = fun.builder();

    let mut simplify_cfg_pass = SimplifyCfgPass::new();
    simplify_cfg_pass.run_function_pass(&mut b);

    let after = parse_function_unwrap("
fib:fib/1 {
    block8(%26, %27, %28):
        %15 = a'erlang':a'<'/2;
        %15(%28, 2) => block13 except block12;
    block13(%38):
        block11(%38);
    block12(%34, %35, %36):
        block11(a'false');
    block11(%32):
        if_bool %32 block9 block10;
    block9():
        unreachable;
    block10():
        unreachable;
}
");
    assert!(b.fun().graph_eq(b.fun().block_entry(), &after, after.block_entry()).is_ok());

}
