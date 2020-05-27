use crate::{NilTerm, StandardFormatConfig};

use super::Mangler;
use super::ToT;

#[test]
fn simple_mangle() {
    let (mut ir, map) = crate::parse_function_map_unwrap(
        "
a'foo':a'bar'/1 {
    entry(%ret, %thr, %a):
        b1();
    b1():
        %ret(%a);
}
",
    );

    let mut b = ir.builder();

    let mut mangler = Mangler::new();

    let b1 = map.get_block("entry");
    let b1_ret = map.get_value("ret");
    let b1_arg = map.get_value("a");

    let new_entry = b.block_insert();
    let ret_narg = b.block_arg_insert(new_entry);
    let nil_term = b.value(NilTerm);
    b.block_copy_body_map(b1, new_entry, |v| Some(v));

    mangler.start(ToT(new_entry));
    mangler.add_rename(ToT(b1_ret), ToT(ret_narg));
    mangler.add_rename(ToT(b1_arg), ToT(nil_term));
    let new_b1 = mangler.run(&mut b);
    b.block_set_entry(new_b1);

    let after = crate::parse_function_unwrap(
        "
a'foo':a'bar'/1 {
    entry(%ret):
        b1();
    b1():
        %ret([]);
}
",
    );

    assert!(b
        .fun()
        .graph_eq(new_b1, &after, after.block_entry())
        .is_ok());
}

#[test]
fn mangle_primop() {
    let (mut ir, map) = crate::parse_function_map_unwrap(
        "
a'foo':a'bar'/1 {
    entry(%ret, %thr, %a):
        %ret({%a});
}
",
    );

    let mut b = ir.builder();

    let mut mangler = Mangler::new();

    let b1 = map.get_block("entry");
    let b1_arg = map.get_value("a");

    let nil_term = b.value(NilTerm);
    mangler.start(ToT(b1));
    mangler.add_rename(ToT(b1_arg), ToT(nil_term));
    let new_b1 = mangler.run(&mut b);

    let after = crate::parse_function_unwrap(
        "
a'foo':a'bar'/1 {
    entry(%ret, %thr, %a):
        %ret({[]});
}
",
    );

    assert!(b
        .fun()
        .graph_eq(new_b1, &after, after.block_entry())
        .is_ok());
}

#[test]
fn mangle_recursive() {
    let (mut ir, map) = crate::parse_function_map_unwrap(
        "
a'foo':a'bar'/2 {
    entry(%ret, %thr, %a, %b):
        b2(%a);
    b1(%m):
        %ret(%t);
    b2(%p):
        %ret(%p);

    ! This just exists to have a dummy variable available
    dummy(%t):
        %t();
}
",
    );

    let mut b = ir.builder();
    println!("{}", b.fun().to_text(&mut StandardFormatConfig::default()));

    let mut mangler = Mangler::new();

    let entry = map.get_block("entry");
    let b1 = map.get_block("b1");
    let b1_val = b.fun().block_value(b1);
    let b2 = map.get_block("b2");
    let b2_val = b.fun().block_value(b2);
    let vb = map.get_value("b");
    let vt = map.get_value("t");

    mangler.start(ToT(entry));
    mangler.add_rename(ToT(b2_val), ToT(b1_val));
    mangler.add_rename(ToT(vt), ToT(vb));
    let new_entry = mangler.run(&mut b);

    b.block_set_entry(new_entry);
    println!("{}", b.fun().to_text(&mut StandardFormatConfig::default()));

    let after = crate::parse_function_unwrap(
        "
a'foo':a'bar'/2 {
    entry(%ret, %thr, %a, %b):
        b1(%a);
    b1(%m):
        %ret(%b);
}
",
    );

    assert!(b
        .fun()
        .graph_eq(new_entry, &after, after.block_entry())
        .is_ok());
}

#[test]
fn mangle_entry() {
    let (mut ir, map) = crate::parse_function_map_unwrap(
        "
a'foo':a'bar'/0 {
    block0(%1, %2):
        unreachable;
    block1(%4, %5):
        block2(%5);
    block2(%7):
        %4();
}
",
    );
    let entry = map.get_block("block0");

    let mut b = ir.builder();

    let mut mangler = Mangler::new();
    mangler.start(ToT(entry));

    mangler.add_rename(
        ToT(b.fun().block_value(map.get_block("block0"))),
        ToT(b.fun().block_value(map.get_block("block1"))),
    );

    let new_entry = mangler.run(&mut b);

    b.block_set_entry(new_entry);
    println!("{}", b.fun().to_text(&mut StandardFormatConfig::default()));

    let mut errors = Vec::new();
    b.fun().validate(&mut errors);
    assert_eq!(errors.len(), 0, "{:#?}", errors);

    let after = crate::parse_function_unwrap(
        "
a'foo':a'bar'/0 {
    block3(%9, %10):
        block4(%10);
    block4(%12):
        %9();
}
",
    );
    assert!(b
        .fun()
        .graph_eq(new_entry, &after, after.block_entry())
        .is_ok());

    //{
    //    value0#block0: (
    //        value17#block5,
    //        true,
    //    ),
    //#    value1#block0[0]: (
    //#        value26#block7[0],
    //#        true,
    //#    ),
    //#    value2#block0[1]: (
    //#        value27#block7[1],
    //#        true,
    //#    ),
    //#    value3#block0[2]: (
    //#        value28#block7[2],
    //#        true,
    //#    ),
    //    value5#block1[0]: (
    //        value15#block4[0],
    //        true,
    //    ),
    //    value6#block1[1]: (
    //        value16#block4[1],
    //        true,
    //    ),
    //    value8#block3: (
    //        value21#block6,
    //        true,
    //    ),
    //}
}
