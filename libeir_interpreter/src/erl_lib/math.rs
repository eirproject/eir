use std::rc::Rc;

use crate::term::{ Term, ErlEq, ErlExactEq };
use crate::process::{ ProcessContext };
use crate::vm::VMState;
use crate::module::{ NativeModule, NativeReturn };

use libeir_intern::Symbol;

use rug::ops::Pow;
use num_traits::{ ToPrimitive };

fn pow(_vm: &VMState, _proc: &mut ProcessContext, args: &[Rc<Term>]) -> NativeReturn {
    assert!(args.len() == 2);

    let a1 = &*args[0];
    let a2 = &*args[1];

    let ret = match (a1, a2) {
        (Term::Integer(ref i1), Term::Integer(ref i2)) =>
            Term::Integer(i1.clone().pow(i2.to_u32().unwrap())),
            //Term::Integer(num::pow::pow(i1.clone(), i2.to_usize().unwrap())),
        _ => unimplemented!(),
    };

    NativeReturn::Return { term: ret.into(), }
}

pub fn make_math() -> NativeModule {
    let mut module = NativeModule::new(Symbol::intern("math"));
    module.add_fun(Symbol::intern("pow"), 2, Box::new(pow));
    module
}
