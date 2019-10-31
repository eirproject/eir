use libeir_ir::parse_function_unwrap;

#[test]
fn simple_function() {

    let fun = parse_function_unwrap("
foo:bar/1 {
    entry(%ret, %thr, %a):
        if_bool %a one two;
    one():
        %ret(a'true');
    two():
        %ret(a'foo');
}
");

    let analyzed = super::analyze(&fun);
    dbg!(analyzed);
}

#[test]
fn nested_functions() {
    let fun = parse_function_unwrap("
foo:bar/1 {
    entry(%ret, %thr, %a):
        %ret(inner);
    inner(%iret, %ithr):
        %iret(%a);
}
");

    let analyzed = super::analyze(&fun);
    dbg!(analyzed);
}
