use crate::FunctionPass;
use super::SimplifyCfgPass;

use libeir_ir::{parse_function_unwrap, parse_function_map_unwrap};

#[test]
fn primop_in_chain() {
    let _ = simple_logger::init();

    let mut fun = parse_function_unwrap("
a'foo':a'bar'/1 {
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
a'foo':a'bar'/1 {
    entry(%ret, %thr, %a):
        %ret({%a});
}
");

    assert!(b.fun().graph_eq(b.fun().block_entry(), &after, after.block_entry()).is_ok());

}

#[test]
fn double_primop_in_chain() {
    let _ = simple_logger::init();

    let mut fun = parse_function_unwrap("
a'foo':a'bar'/1 {
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
a'foo':a'bar'/1 {
    entry(%ret, %thr, %a):
        %ret({{%a}});
}
");

    assert!(b.fun().graph_eq(b.fun().block_entry(), &after, after.block_entry()).is_ok());

}

#[test]
fn split_primop_in_chain() {
    let _ = simple_logger::init();

    let mut fun = parse_function_unwrap("
a'foo':a'bar'/3 {
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
a'foo':a'bar'/3 {
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
    let _ = simple_logger::init();

    let mut fun = parse_function_unwrap("
a'foo':a'bar'/5 {
    entry(%ret, %thr, %a, %b, %c, %B, %C):
        if_bool %a b_true b_false;

    b_true():
        b_true1(%b, %B);
      b_true1(%bb, %BB):
        b_join({{%bb}, %BB}, {%BB});

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
a'foo':a'bar'/5 {
    entry(%ret, %thr, %a, %b, %c, %B, %C):
        if_bool %a b_true b_false;
    b_true():
        %ret({{{{%b}, %B}}, {{%B}}});
    b_false():
        %ret({{{%c}}, {{%C}}});
}
");

    assert!(b.fun().graph_eq(b.fun().block_entry(), &after, after.block_entry()).is_ok());

}

#[test]
fn simple_tail_call_elimination() {
    let _ = simple_logger::init();

    let mut fun = parse_function_unwrap("
a'foo':a'bar'/0 {
    entry(%ret, %thr):
        a'foo':a'foo'/0(b2, b3);
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
a'foo':a'bar'/0 {
    entry(%ret, %thr):
        a'foo':a'foo'/0(%ret, %thr);
}
");

    assert!(b.fun().graph_eq(b.fun().block_entry(), &after, after.block_entry()).is_ok());

}

#[test]
fn tail_call_elimination() {
    let _ = simple_logger::init();

    let mut fun = parse_function_unwrap("
a'foo':a'bar'/0 {
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
a'foo':a'bar'/0 {
    entry(%ret, %thr):
        a'foo':a'foo'/0(%ret, %thr);
}
");

    assert!(b.fun().graph_eq(b.fun().block_entry(), &after, after.block_entry()).is_ok());

}

#[test]
fn recursive_simplification() {
    let _ = simple_logger::init();

    let mut fun = parse_function_unwrap("
a'foo':a'bar'/0 {
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
a'foo':a'bar'/0 {
    entry(%ret, %thr):
        %ret(a'yay');
}
");

    assert!(b.fun().graph_eq(b.fun().block_entry(), &after, after.block_entry()).is_ok());
}

#[test]
fn value_list_removal() {
    let _ = simple_logger::init();

    let mut fun = parse_function_unwrap("
a'foo':a'bar'/2 {
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
a'foo':a'bar'/2 {
    entry(%ret, %thr, %a, %b):
        %ret(%a);
}
");

    assert!(b.fun().graph_eq(b.fun().block_entry(), &after, after.block_entry()).is_ok());
}

#[test]
fn partial_loop() {
    let _ = simple_logger::init();

let mut fun = parse_function_unwrap("
a'foo':a'bar'/1 {
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
a'foo':a'bar'/1 {
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
    let _ = simple_logger::init();

    let mut fun = parse_function_unwrap("
a'foo':a'perms'/1 {
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

//    let after = parse_function_unwrap("
//a'foo':a'perms'/1 {
//    block0(%1, %2, %3):
//        block1(%3, []);
//    block1(%5, %6):
//        match %5 {
//            [] => block3;
//        };
//    block3(%9, %10):
//        %14 = [%9 | %6];
//        block1(%10, %14);
//}
//");
    let after = parse_function_unwrap("
a'foo':a'perms'/1 {
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
    let _ = simple_logger::init();

    let mut fun = parse_function_unwrap("
a'foo':a'do_map_vars_used'/1 {
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
a'foo':a'do_map_vars_used'/1 {
    entry(%1, %2, %3):
        %4 = {%3};
        match %4 {};
}
");
    assert!(b.fun().graph_eq(b.fun().block_entry(), &after, after.block_entry()).is_ok());

}

#[test]
fn deep_primop_rename_after_entry_single_branch() {
    let _ = simple_logger::init();

    let mut fun = parse_function_unwrap("
a'foo':a'do_map_vars_used'/1 {
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
a'foo':a'do_map_vars_used'/1 {
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

#[test]
fn converging_from_single() {
    let _ = simple_logger::init();

    let mut fun = parse_function_unwrap("
a'fib':a'fib'/1 {
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
a'fib':a'fib'/1 {
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

// Fails because of https://github.com/eirproject/eir/issues/24
#[test]
fn block_capture_with_scope_in_chain() {
    let _ = simple_logger::init();

    let mut fun = parse_function_unwrap("
a'a':a'get_values'/1 {
    block1(%3, %4, %5):
        bn(%5);
    bn(%6):
        block7(%6);
    block7(%27):
        block58(block49);
    block58(%136):
        %3(%136);

    block49(%116, %117, %118):
        %47 = a'erlang':a'=:='/2;
        %47({%27, 1}, %118) => %116 except %117;
}
");
    let mut b = fun.builder();

    let mut simplify_cfg_pass = SimplifyCfgPass::new();
    simplify_cfg_pass.run_function_pass(&mut b);

    b.fun().live_values();

}

#[test]
fn aaaa() {
    use libeir_intern::Symbol;
    use libeir_ir::{Function, FunctionBuilder, FunctionIdent, StandardFormatConfig};
    use libeir_ir::AtomTerm;
    use libeir_ir::operation::binary_construct::{BinaryConstructStart, BinaryConstructFinish};

    let (mut fun, map) = parse_function_map_unwrap("
a'a':a'a'/1 {
  block1(%1, %2, %3):
    match %3 {
      value a'yay' => block2;
    };
  block2():
    block3();
  block3():
    block4(a'true');
  block4(%4):
    if_bool %4 bt bf be;
  bf():
    unreachable;
  be():
    unreachable;
  bt():
    block5();
  block5():
    unreachable;
  block6(%binref):
    unreachable;
  block7(%bin):
    unreachable;
}
");

    let mut b = FunctionBuilder::new(&mut fun);

    let block5 = map.get_block("block5");
    let block6 = map.get_block("block6");
    let block7 = map.get_block("block7");

    b.block_clear(block5);
    BinaryConstructStart::build_target(&mut b, block5, block6);
    let bin_ref = b.block_args(block6)[0];

    b.block_clear(block6);
    BinaryConstructFinish::build_target(&mut b, block6, bin_ref, block7);

    println!("{}", b.fun().to_text(&mut StandardFormatConfig::default()));

    let mut simplify_cfg_pass = SimplifyCfgPass::new();
    simplify_cfg_pass.run_function_pass(&mut b);

    println!("{}", b.fun().to_text(&mut StandardFormatConfig::default()));

    //@binary_construct_start(block6);
    //@binary_construct_finish(block7, %binref);


}


#[test]
fn bbbb() {
    let _ = simple_logger::init();

    let mut fun = parse_function_unwrap("
a'a':a'a'/1 {
  block1(%3, %4, %5):
    block33(%5);
  block33(%101):
    unpack %101 arity 1 => block34;
  block34(%103):
    %47 = <>;
    %133 = <block35, block46>;
    match %103 {
      [] => block35;
      _ => block46;
    };
  block46():
    block71();
  block71():
    block47(a'true');
  block47(%125):
    if_bool %125 block49 block50 block51;
  block51():
    unreachable;
  block50():
    block3();
  block3():
    trace_capture_raw block4;
  block4(%12):
    %4(a'error', a'function_clause', %12);
  block49():
    block10(%103);
  block10(%37):
    block11(%37, []);
  block11(%39, %40):
    %41 = [] == %39;
    if_bool %41 block12 block13;
  block13():
    %47 = <>;
    %49 = <block14, block15>;
    match %39 {
      [] => block14;
      _ => block15;
    };
  block15():
    trace_capture_raw block16;
  block16(%51):
    %4(a'error', a'function_clause', %51);
  block14(%45, %46):
    block52(%45);
  block52(%135):
    unpack %135 arity 1 => block53;
  block53(%137):
    block69();
  block69():
    block54(a'true');
  block54(%139):
    if_bool %139 block56 block57 block58;
  block58():
    unreachable;
  block57():
    block17();
  block17():
    block11(%46, []);
  block56():
    block20(%137);
  block20(%62):
    block21(%37, %40);
  block21(%64, %65):
    %66 = [] == %64;
    if_bool %66 block22 block23;
  block23():
    %47 = <>;
    %73 = <block24, block25>;
    match %64 {
      [] => block24;
      _ => block25;
    };
  block25():
    trace_capture_raw block26;
  block26(%75):
    %4(a'error', a'function_clause', %75);
  block24(%70, %71):
    block59(%70);
  block59(%148):
    unpack %148 arity 1 => block60;
  block60(%150):
    block67();
  block67():
    block61(a'true');
  block61(%152):
    if_bool %152 block63 block64 block65;
  block65():
    unreachable;
  block64():
    block27();
  block27():
    block21(%71, %40);
  block63():
    block30(%150);
  block30(%86):
    %87 = [%86 | []];
    %88 = [%62 | %87];
    %89 = [%88 | %65];
    block21(%71, %89);
  block22():
    block11(%46, %65);
  block12():
    %93 = a'lists':a'reverse'/1;
    %93(%40) => block31 except block32;
  block32(%97, %98, %99):
    %4(%97, %98, %99);
  block31(%95):
    block2(%95);
  block2(%7):
    %3(%7);
  block35():
    block75();
  block75():
    block36(a'true');
  block36(%106):
    if_bool %106 block38 block39 block40;
  block40():
    unreachable;
  block39():
    block73();
  block73():
    block41(a'true');
  block41(%115):
    if_bool %115 block43 block44 block45;
  block45():
    unreachable;
  block44():
    block3();
  block43():
    block10(%103);
  block38():
    block7();
  block7():
    block2([[] | []]);
}
");

    let mut b = fun.builder();

    let dot = libeir_ir::text::function_to_dot(b.fun());
    println!("{}", dot);

    let mut simplify_cfg_pass = SimplifyCfgPass::new();
    simplify_cfg_pass.run_function_pass(&mut b);


}


#[test]
fn cccc() {
    env_logger::init();

    let mut fun = parse_function_unwrap("
a'a':a'underscore'/1 {
  block55(%154, %155, %156):
    block83(%156);
  block83(%214):
    unpack %214 arity 1 => block84;
  block84(%216):
    block138();
  block138():
    %27 = a'erlang':a'is_list'/1;
    %27(%216) => block85 except block137;
  block137(%307, %308, %309):
    block85(a'false');
  block85(%218):
    if_bool %218 block87 block88 block89;
  block89():
    unreachable;
  block88():
    block56();
  block56():
    trace_capture_raw block82;
  block82(%208):
    %155(a'error', a'function_clause', %208);
  block87():
    block58(%216);
  block58(%163):
    block90(%163);
  block90(%227):
    unpack %227 arity 1 => block91;
  block91(%229):
    %233 = <>;
    %255 = <block92, block98, block104>;
    match %229 {
      [] => block92;
      value [] => block98;
      _ => block104;
    };
  block104():
    block59();
  block59():
    trace_capture_raw block80;
  block80(%202):
    %209 = {a'case_clause', %163};
    %155(a'error', %209, %202);
  block98():
    block131();
  block131():
    block99(a'true');
  block99(%246):
    if_bool %246 block101 block102 block103;
  block103():
    unreachable;
  block102():
    block59();
  block101():
    block60();
  block60():
    %51 = a'erlang':a'length'/1;
    %51(%163) => block61 except %155;
  block61(%167):
    block119(%167);
  block119(%283):
    unpack %283 arity 1 => block120;
  block120(%285):
    block129();
  block129():
    block121(a'true');
  block121(%287):
    if_bool %287 block123 block124 block125;
  block125():
    unreachable;
  block124():
    block62();
  block62():
    trace_capture_raw block78;
  block78(%197):
    %210 = {a'badmatch', %167};
    %155(a'error', %210, %197);
  block123():
    block72();
  block72():
    block71();
  block71():
    %27 = a'erlang':a'is_list'/1;
    %27(%163) => block66 except %155;
  block66(%174):
    block112(%174);
  block112(%270):
    unpack %270 arity 1 => block113;
  block113(%272):
    block127();
  block127():
    block114(a'true');
  block114(%274):
    if_bool %274 block116 block117 block118;
  block118():
    unreachable;
  block117():
    block67();
  block67():
    trace_capture_raw block70;
  block70(%181):
    %212 = {a'badmatch', %174};
    %155(a'error', %212, %181);
  block116():
    block68();
  block68():
    %154(a'ok');
  block92(%231, %232):
    block135();
  block135():
    block93(a'true');
  block93(%235):
    if_bool %235 block95 block96 block97;
  block97():
    unreachable;
  block96():
    block59();
  block95():
    block63();
  block63():
    %80 = a'erlang':a'list_to_tuple'/1;
    %80(%163) => block64 except %155;
  block64(%171):
    block105(%171);
  block105(%257):
    unpack %257 arity 1 => block106;
  block106(%259):
    block133();
  block133():
    block107(a'true');
  block107(%261):
    if_bool %261 block109 block110 block111;
  block111():
    unreachable;
  block110():
    block65();
  block65():
    trace_capture_raw block75;
  block75(%189):
    %211 = {a'badmatch', %171};
    %155(a'error', %211, %189);
  block109():
    block73();
  block73():
    block71();
}
");

    let mut b = fun.builder();

    let mut simplify_cfg_pass = SimplifyCfgPass::new();
    simplify_cfg_pass.run_function_pass(&mut b);

    let mut errs = Vec::new();
    b.fun().validate(&mut errs);
    println!("{:?}", errs);

    panic!()
}
