use std::rc::Rc;

use crate::lower;

use libeir_ir::{ FunctionIdent };
use libeir_syntax_erl::{ ParseConfig };
use libeir_intern::Ident;
use libeir_passes::PassManager;

use libeir_interpreter::{ VMState, Term, ErlEq };

#[test]
fn test_list_comprehension_single_filter() {
    let mut eir_mod = lower("
-module(woo).

woo(N) -> [1 || erlang:is_integer(N)].
", ParseConfig::default()).unwrap();

    for fun_def in eir_mod.function_iter() {
        let fun = fun_def.function();
        let mut out = Vec::new();
        fun.validate(&mut out);
        assert!(out.len() == 0);
    }

    let mut pass_manager = PassManager::default();
    pass_manager.run(&mut eir_mod);

    for fun_def in eir_mod.function_iter() {
        let fun = fun_def.function();
        let mut out = Vec::new();
        fun.validate(&mut out);
        assert!(out.len() == 0);
    }

    let fun = FunctionIdent {
        module: Ident::from_str("woo"),
        name: Ident::from_str("woo"),
        arity: 1,
    };

    println!("{}", eir_mod.to_text());

    let mut vm = VMState::new();
    vm.add_builtin_modules();
    vm.add_erlang_module(eir_mod);

    assert!(vm.call(&fun, &[Term::Nil]).unwrap().erl_eq(&Term::Nil));

    {
        let out = Term::slice_to_list(&[Term::Integer(1.into()).into()], Term::Nil.into());
        assert!(vm.call(&fun, &[Term::Integer(1.into())]).unwrap().erl_eq(&*out));
    }

}

#[test]
fn test_list_comprehension_single_list_generator() {
    let mut eir_mod = lower("
-module(woo).

woo(L) -> [X*2 || X <- L].
", ParseConfig::default()).unwrap();

    for fun_def in eir_mod.function_iter() {
        let fun = fun_def.function();
        let mut out = Vec::new();
        fun.validate(&mut out);
        assert!(out.len() == 0);
    }

    let mut pass_manager = PassManager::default();
    pass_manager.run(&mut eir_mod);

    for fun_def in eir_mod.function_iter() {
        let fun = fun_def.function();
        let mut out = Vec::new();
        fun.validate(&mut out);
        assert!(out.len() == 0);
    }

    let fun = FunctionIdent {
        module: Ident::from_str("woo"),
        name: Ident::from_str("woo"),
        arity: 1,
    };

    println!("{}", eir_mod.to_text());

    let mut vm = VMState::new();
    vm.add_builtin_modules();
    vm.add_erlang_module(eir_mod);

    assert!(vm.call(&fun, &[Term::Nil]).unwrap().erl_eq(&Term::Nil));

    {
        let in_list = Term::slice_to_list(&[Term::Integer(1.into()).into()], Term::Nil.into());
        let out_list = Term::slice_to_list(&[Term::Integer(2.into()).into()], Term::Nil.into());
        assert!(vm.call(&fun, &[Rc::try_unwrap(in_list).unwrap()]).unwrap().erl_eq(&*out_list));
    }

    {
        let in_list = Term::slice_to_list(&[
            Term::Integer(1.into()).into(),
            Term::Integer(2.into()).into(),
            Term::Integer(3.into()).into(),
        ], Term::Nil.into());
        let out_list = Term::slice_to_list(&[
            Term::Integer(2.into()).into(),
            Term::Integer(4.into()).into(),
            Term::Integer(6.into()).into(),
        ], Term::Nil.into());
        assert!(vm.call(&fun, &[Rc::try_unwrap(in_list).unwrap()]).unwrap().erl_eq(&*out_list));
    }

}

#[test]
fn test_list_comprehension_combinations() {
    let mut eir_mod = lower("
-module(woo).

comb([]) -> [[]];
comb(L) -> [[A, B] || A <- L, B <- L].
", ParseConfig::default()).unwrap();

    for fun_def in eir_mod.function_iter() {
        let fun = fun_def.function();
        let mut out = Vec::new();
        fun.validate(&mut out);
        assert!(out.len() == 0);
    }

    let mut pass_manager = PassManager::default();
    pass_manager.run(&mut eir_mod);

    for fun_def in eir_mod.function_iter() {
        let fun = fun_def.function();
        let mut out = Vec::new();
        fun.validate(&mut out);
        println!("{:?}", out);
        assert!(out.len() == 0);
    }

    let fun = FunctionIdent {
        module: Ident::from_str("woo"),
        name: Ident::from_str("comb"),
        arity: 1,
    };

    let mut vm = VMState::new();
    vm.add_builtin_modules();
    vm.add_erlang_module(eir_mod);

    {
        let arg = Term::slice_to_list(&[
            Term::new_atom("b").into(),
            Term::new_atom("u").into(),
        ], Term::Nil.into());

        let out = Term::slice_to_list(&[
            Term::slice_to_list(&[Term::new_atom("b").into(), Term::new_atom("b").into()], Term::Nil.into()),
            Term::slice_to_list(&[Term::new_atom("b").into(), Term::new_atom("u").into()], Term::Nil.into()),
            Term::slice_to_list(&[Term::new_atom("u").into(), Term::new_atom("b").into()], Term::Nil.into()),
            Term::slice_to_list(&[Term::new_atom("u").into(), Term::new_atom("u").into()], Term::Nil.into()),
        ], Term::Nil.into());

        let res = vm.call(&fun, &[Rc::try_unwrap(arg).unwrap()]).unwrap();
        assert!(res.erl_eq(&out));
    }

}

#[test]
fn test_basic_comprehension() {
    let mut eir_mod = lower("
-module(woo).

perms(L) -> [H || H <- L].
", ParseConfig::default()).unwrap();

    for fun_def in eir_mod.function_iter() {
        let fun = fun_def.function();
        let mut out = Vec::new();
        fun.validate(&mut out);
        assert!(out.len() == 0);
    }

    let mut pass_manager = PassManager::default();
    pass_manager.run(&mut eir_mod);

    for fun_def in eir_mod.function_iter() {
        let fun = fun_def.function();
        let mut out = Vec::new();
        fun.validate(&mut out);
        println!("{:?}", out);
        assert!(out.len() == 0);

        libeir_lowerutils::analyze(fun);
    }

    println!("{}", eir_mod.to_text());
}

#[test]
fn test_list_comprehension_permutations() {
    let mut eir_mod = lower("
-module(woo).

perms([]) -> [[]];
perms(L) -> [[H|T] || H <- L, T <- perms(L--[H])].
", ParseConfig::default()).unwrap();

    for fun_def in eir_mod.function_iter() {
        let fun = fun_def.function();
        let mut out = Vec::new();
        fun.validate(&mut out);
        assert!(out.len() == 0);
    }

    let mut pass_manager = PassManager::default();
    pass_manager.run(&mut eir_mod);

    for fun_def in eir_mod.function_iter() {
        let fun = fun_def.function();
        let mut out = Vec::new();
        fun.validate(&mut out);
        assert!(out.len() == 0);

        let analysis = libeir_lowerutils::analyze(fun);
        dbg!(analysis);
    }

    println!("{}", eir_mod.to_text());

    let fun = FunctionIdent {
        module: Ident::from_str("woo"),
        name: Ident::from_str("perms"),
        arity: 1,
    };

    let mut vm = VMState::new();
    vm.add_builtin_modules();
    vm.add_erlang_module(eir_mod);

    {
        let b: Rc<Term> = Term::new_atom("b").into();
        let u: Rc<Term> = Term::new_atom("u").into();
        let g: Rc<Term> = Term::new_atom("g").into();

        let arg = Term::slice_to_list(&[
            b.clone(), u.clone(), g.clone(),
        ], Term::Nil.into());

        let out = Term::slice_to_list(&[
            Term::slice_to_list(&[b.clone(), u.clone(), g.clone()], Term::Nil.into()),
            Term::slice_to_list(&[b.clone(), g.clone(), u.clone()], Term::Nil.into()),
            Term::slice_to_list(&[u.clone(), b.clone(), g.clone()], Term::Nil.into()),
            Term::slice_to_list(&[u.clone(), g.clone(), b.clone()], Term::Nil.into()),
            Term::slice_to_list(&[g.clone(), b.clone(), u.clone()], Term::Nil.into()),
            Term::slice_to_list(&[g.clone(), u.clone(), b.clone()], Term::Nil.into()),
        ], Term::Nil.into());

        let res = vm.call(&fun, &[Rc::try_unwrap(arg).unwrap()]).unwrap();

        println!("{:?}", res);
        println!("{:?}", out);

        assert!(res.erl_eq(&out));
    }

}
