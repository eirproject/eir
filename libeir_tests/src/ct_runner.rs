use std::collections::HashMap;
use std::rc::Rc;

use libeir_ir::FunctionIdent;
use libeir_intern::{ Ident, Symbol };

use libeir_interpreter::{ VMState, Term };

#[derive(Debug, Clone)]
struct SuiteSpec {
    entries: Vec<SuiteEntrySpec>,
}

#[derive(Debug, Clone)]
enum SuiteEntrySpec {
    Test(Symbol),
    Group {
        name: Symbol,
        tests: Vec<Symbol>,
    }
}

fn get_suite_spec(vm: &mut VMState, module: Ident) -> SuiteSpec {
    let groups = {
        let fun = FunctionIdent {
            module,
            name: Ident::from_str("groups"),
            arity: 0,
        };
        let ret = vm.call(&fun, &[]).unwrap();

        let mut groups = HashMap::new();
        for elem in Term::as_list(&ret).unwrap() {
            let tup = elem.as_tuple().unwrap();
            assert!(tup.len() == 3);

            let name = tup[0].as_atom().unwrap();

            let tests = Term::as_list(&tup[2])
                .unwrap().iter()
                .map(|v| v.as_atom().unwrap())
                .collect();

            let group = SuiteEntrySpec::Group {
                name,
                tests,
            };
            groups.insert(name, group);
        }

        groups
    };

    let all = {
        let fun = FunctionIdent {
            module,
            name: Ident::from_str("all"),
            arity: 0,
        };
        let ret = vm.call(&fun, &[]).unwrap();

        let mut entries = Vec::new();
        for elem in Term::as_list(&ret).unwrap() {
            if let Some(name) = elem.as_atom() {
                entries.push(SuiteEntrySpec::Test(name));
            } else if let Some(tup) = elem.as_tuple() {
                match &tup {
                    &[group_atom, group] if group_atom.as_atom() ==
                        Some(Symbol::intern("group")) =>
                    {
                        let group_name = group.as_atom().unwrap();
                        entries.push(groups[&group_name].clone());
                    }
                    _ => panic!(),
                }
            } else {
                panic!()
            }
        }

        entries
    };

    SuiteSpec {
        entries: all,
    }
}

pub fn run_ct_suite(vm: &mut VMState, module: Ident) {
    let spec = get_suite_spec(vm, module);
    println!("{:?}", spec);

    let config = Term::Nil;

    for entry in spec.entries.iter() {
        match entry {
            SuiteEntrySpec::Group { tests, .. } => {
                for test in tests {
                    let fun = FunctionIdent {
                        module,
                        name: Ident::with_empty_span(*test),
                        arity: 1,
                    };
                    let res = vm.call(&fun, &[config.clone()]);
                    res.unwrap();
                }
            }
            SuiteEntrySpec::Test(sym) => {
                let fun = FunctionIdent {
                    module,
                    name: Ident::with_empty_span(*sym),
                    arity: 1,
                };
                let res = vm.call(&fun, &[config.clone()]);
                res.unwrap();
            }
        }
    }
}
