use libeir_ir::{parse_function_unwrap, StandardFormatConfig};

use crate::FunctionPass;

#[test]
fn inline_basic_function() {

    let mut fun = parse_function_unwrap("
a'foo':a'fun_shadowing'/1 {
    entry(%ret, %thr, %A):
        b1();
    b1():
        inner(ret, thr, %A);
    inner(%iret, %ithr, %B):
        %iret(%B);

    ret(%rv):
        %ret(%rv);
    thr(%rt1, %rt2, %rt3):
        %thr(%rt1, %rt2, %rt3);

}
");
    let mut b = fun.builder();

    let mut pass = super::NaiveInlineClosuresPass::new();
    pass.run_function_pass(&mut b);

    let after = parse_function_unwrap("
a'foo':a'fun_shadowing'/1 {
    entry(%ret, %thr, %A):
        b1();
    b1():
        b2();
    b2():
        ret(%A);
    ret(%rv):
        %ret(%rv);
}
");

    assert!(b.fun().graph_eq(b.fun().block_entry(), &after, after.block_entry()).is_ok());

}

#[test]
fn inline_nested_functions() {

    let mut fun = parse_function_unwrap("
a'foo':a'fun_shadowing'/1 {
    entry(%ret, %thr, %A):
        b1();
    b1():
        ainner(ret, thr, %A);

    ainner(%iret, %ithr, %B):
        anext();
    anext():
        binner(aret, athr, %B);
    aret(%aretv):
        %iret(%aretv);
    athr(%athr1, %athr2, %athr3):
        %ithr(%athr1, %athr2, %athr3);

    binner(%bret, %bthr, %C):
        bnext();
    bnext():
        %bret(%C);

    ret(%rv):
        %ret(%rv);
    thr(%rt1, %rt2, %rt3):
        %thr(%rt1, %rt2, %rt3);

}
");
    let mut b = fun.builder();

    let mut pass = super::NaiveInlineClosuresPass::new();
    pass.run_function_pass(&mut b);

    println!("{}", b.fun().to_text(&mut StandardFormatConfig::default()));

    let after = parse_function_unwrap("
a'foo':a'fun_shadowing'/1 {
    entry(%ret, %thr, %A):
        b1();
    b1():
        b2();
    b2():
        b3();
    b3():
        b4();
    b4():
        b5();
    b5():
        ret1(%A);
    ret1(%B):
        ret(%B);
    ret(%rv):
        %ret(%rv);
}
");

    assert!(b.fun().graph_eq(b.fun().block_entry(), &after, after.block_entry()).is_ok());

}
