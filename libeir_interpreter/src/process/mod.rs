use std::cell::RefCell;
use std::rc::Rc;
use std::collections::HashMap;

use num_bigint::BigInt;

use libeir_ir::{ FunctionIdent, Block, Value, OpKind, BinOp, ValueType };
use libeir_ir::constant::{ ConstValue, ConstValueKind, AtomicTerm };
use libeir_intern::{ Symbol, Ident };

use crate::term::{ Term, Pid, ErlEq };
use crate::module::{ ModuleType, ErlangModule, ErlangFunction, NativeModule, NativeReturn };
use crate::vm::VMState;

pub struct TermCall {
    pub fun: Rc<Term>,
    pub args: Vec<Rc<Term>>,
}

pub enum Continuation {
    Term(TermCall),
    ReturnOk(Rc<Term>),
    ReturnThrow(Rc<Term>, Rc<Term>, Rc<Term>),
}

pub struct CallExecutor {
    binds: HashMap<Value, Rc<Term>>,
}

impl CallExecutor {

    pub fn new() -> Self {
        CallExecutor {
            binds: HashMap::new(),
        }
    }

    pub fn run(&mut self, vm: &VMState, proc: &mut ProcessContext, call: TermCall) -> Continuation {
        self.binds.clear();
        match &*call.fun {
            Term::BoundLambda { ident, block, environment } => {
                let module = &vm.modules[&ident.module.name];
                match module {
                    ModuleType::Erlang(erl, _overlay) => {
                        Continuation::Term(
                            self.run_erlang(vm, erl, ident, Some((*block, &*environment)), &call.args).unwrap()
                        )
                    }
                    ModuleType::Native(_native) => {
                        unreachable!()
                    }
                }
            }
            Term::CapturedFunction { ident } => {
                let module = &vm.modules[&ident.module.name];
                match module {
                    ModuleType::Erlang(erl, overlay) => {
                        if let Some(native) = overlay {
                            if let Some(res) = self.run_native(vm, proc, native, ident, &call.args) {
                                return Continuation::Term(res);
                            }
                        }
                        Continuation::Term(
                            self.run_erlang(vm, erl, ident, None, &call.args).unwrap()
                        )
                    }
                    ModuleType::Native(native) => {
                        Continuation::Term(
                            if let Some(res) = self.run_native(vm, proc, native, ident, &call.args) {
                                res
                            } else {
                                panic!("Could not find native function {}", ident);
                            }
                        )
                    }
                }
            }
            Term::ReturnOk => {
                assert!(call.args.len() == 1);
                Continuation::ReturnOk(call.args[0].clone())
            }
            Term::ReturnThrow => {
                assert!(call.args.len() == 3);
                Continuation::ReturnThrow(call.args[0].clone(), call.args[1].clone(), call.args[2].clone())
            }
            // TODO can't call term type, throw exception
            _ => unimplemented!(),
        }
    }

    pub fn run_native(&mut self, vm: &VMState, proc: &mut ProcessContext, native: &NativeModule, ident: &FunctionIdent,
                      args: &[Rc<Term>]) -> Option<TermCall> {
        if let Some(n_fun) = native.functions.get(&(ident.name.name, ident.arity)) {
            match n_fun(vm, proc, &args[2..]) {
                NativeReturn::Return { term } => {
                    Some(TermCall {
                        fun: args[0].clone(),
                        args: vec![term],
                    })
                }
                NativeReturn::Throw => {
                    Some(TermCall {
                        fun: args[1].clone(),
                        args: vec![Term::Nil.into(), Term::Nil.into(), Term::Nil.into()],
                    })
                }
            }
        } else {
            None
        }
    }

    pub fn run_erlang(&mut self, vm: &VMState, module: &ErlangModule, ident: &FunctionIdent,
                      state: Option<(Block, &[Rc<Term>])>, args: &[Rc<Term>]) -> Option<TermCall> {
        if let Some(fun) = module.functions.get(&ident) {
            // Environment
            let block = if let Some((block, env)) = state {
                let live = &fun.live.live[&block];

                for (v, t) in live.iter(&fun.live.pool).zip(env.iter()) {
                    self.binds.insert(v, t.clone());
                }
                assert!(live.size(&fun.live.pool) == env.len());

                block
            } else {
                fun.fun.block_entry()
            };

            // Insert arguments
            let block_arg_vals = fun.fun.block_args(block);
            assert!(block_arg_vals.len() == args.len());
            for (v, t) in block_arg_vals.iter().zip(args.iter()) {
                self.binds.insert(*v, t.clone());
            }

            // Execute operation
            Some(self.run_erlang_op(vm, fun, block))
        } else {
            None
        }
    }

    fn make_const_term(&self, fun: &ErlangFunction, const_val: ConstValue) -> Rc<Term> {
        match fun.fun.cons().const_value_kind(const_val) {
            ConstValueKind::Atomic(AtomicTerm::Atom(atom)) => {
                Term::Atom(atom.0).into()
            }
            ConstValueKind::Atomic(AtomicTerm::Int(int)) => {
                Term::Integer(int.0.into()).into()
            }
            ConstValueKind::Atomic(AtomicTerm::Nil) => {
                Term::Nil.into()
            }
            kind => unimplemented!("{:?}", kind),
        }
    }

    fn make_term(&self, fun: &ErlangFunction, value: Value) -> Rc<Term> {
        match fun.fun.value(value) {
            ValueType::Block(block) => {
                let live = &fun.live.live[block];
                let mut env = Vec::new();
                for v in live.iter(&fun.live.pool) {
                    assert!(fun.fun.value_is_arg(v));
                    env.push(self.make_term(fun, v));
                }
                Term::BoundLambda { ident: fun.fun.ident().clone(), block: *block, environment: env }.into()
            }
            ValueType::Arg(_) => {
                self.binds[&value].clone()
            }
            ValueType::Constant(cons) => {
                let const_val = fun.fun.cons().const_value(*cons);
                self.make_const_term(fun, const_val)
            }
            _ => unreachable!()
        }
    }

    pub fn run_erlang_op(&mut self, vm: &VMState, fun: &ErlangFunction, block: Block) -> TermCall {
        let reads = fun.fun.block_reads(block);
        println!("{} {}", fun.fun.ident(), block);
        match fun.fun.block_kind(block).unwrap() {
            OpKind::PackValueList => {
                let val_list = Term::ValueList(
                    reads.iter().skip(1).map(|r| self.make_term(fun, *r)).collect()
                );
                TermCall {
                    fun: self.make_term(fun, reads[0]),
                    args: vec![val_list.into()],
                }
            }
            OpKind::Call => {
                TermCall {
                    fun: self.make_term(fun, reads[0]),
                    args: reads.iter().skip(1).map(|r| self.make_term(fun, *r)).collect(),
                }
            }
            OpKind::UnpackValueList(num) => {
                assert!(reads.len() == 2);
                let term = self.make_term(fun, reads[1]);
                match &*term {
                    Term::ValueList(items) => {
                        assert!(items.len() == *num);
                        TermCall {
                            fun: self.make_term(fun, reads[0]),
                            args: items.clone(),
                        }
                    }
                    _ => {
                        TermCall {
                            fun: self.make_term(fun, reads[0]),
                            args: vec![term],
                        }
                    }
                    //term => unreachable!("{:?}", term),
                }
            }
            OpKind::UnpackListCell => {
                assert!(reads.len() == 3);
                let val = self.make_term(fun, reads[2]);
                match &*val {
                    Term::ListCell(head, tail) => {
                        TermCall {
                            fun: self.make_term(fun, reads[0]),
                            args: vec![head.clone(), tail.clone()],
                        }
                    }
                    _ => {
                        TermCall {
                            fun: self.make_term(fun, reads[1]),
                            args: vec![],
                        }
                    }
                }
            }
            OpKind::CaptureFunction => {
                let module = self.make_term(fun, reads[1]).as_atom().unwrap();
                let name = self.make_term(fun, reads[2]).as_atom().unwrap();
                let arity = self.make_term(fun, reads[3]).as_usize().unwrap();

                let ident = FunctionIdent {
                    module: Ident::with_empty_span(module),
                    name: Ident::with_empty_span(name),
                    arity,
                };

                TermCall {
                    fun: self.make_term(fun, reads[0]),
                    args: vec![Term::CapturedFunction { ident }.into()],
                }
            }
            OpKind::Intrinsic(name) if *name == Symbol::intern("bool_and") => {
                let mut res = true;
                for val in reads[1..].iter() {
                    let term = self.make_term(fun, *val);
                    let b = term.as_boolean().expect("non boolean argument to bool_and");
                    res = res & b;
                }

                TermCall {
                    fun: self.make_term(fun, reads[0]),
                    args: vec![Term::new_bool(res).into()],
                }
            }
            OpKind::Intrinsic(name) if *name == Symbol::intern("bool_or") => {
                let mut res = false;
                for val in reads[1..].iter() {
                    let term = self.make_term(fun, *val);
                    let b = term.as_boolean().expect("non boolean argument to bool_or");
                    res = res | b;
                }

                TermCall {
                    fun: self.make_term(fun, reads[0]),
                    args: vec![Term::new_bool(res).into()],
                }
            }
            OpKind::IfBool => {
                let bool_term = self.make_term(fun, reads[3]);
                let call_n = match bool_term.as_boolean() {
                    Some(true) => 0,
                    Some(false) => 1,
                    None => 2,
                };

                TermCall {
                    fun: self.make_term(fun, reads[call_n]),
                    args: vec![],
                }
            }
            OpKind::UnpackTuple(num) => {
                let unpack_term = self.make_term(fun, reads[2]);
                match &*unpack_term {
                    Term::Tuple(elems) if elems.len() == *num => {
                        TermCall {
                            fun: self.make_term(fun, reads[0]),
                            args: elems.clone(),
                        }
                    }
                    _ => {
                        TermCall {
                            fun: self.make_term(fun, reads[1]),
                            args: vec![],
                        }
                    }
                }
            }
            OpKind::BinOp(BinOp::Equal) => {
                assert!(reads.len() == 4);
                let lhs = self.make_term(fun, reads[2]);
                let rhs = self.make_term(fun, reads[3]);
                if lhs.erl_eq(&rhs) {
                    TermCall {
                        fun: self.make_term(fun, reads[0]),
                        args: vec![],
                    }
                } else {
                    TermCall {
                        fun: self.make_term(fun, reads[1]),
                        args: vec![],
                    }
                }
            }
            OpKind::MakeList => {
                let heads: Vec<_> = reads.iter().skip(2).map(|v| self.make_term(fun, *v)).collect();
                let term = Term::slice_to_list(&heads, self.make_term(fun, reads[1]));
                TermCall {
                    fun: self.make_term(fun, reads[0]),
                    args: vec![term],
                }
            }
            kind => unimplemented!("{:?}", kind),
        }
    }

}

pub struct ProcessContext {
    pub pid: Pid,
}

impl ProcessContext {

    pub fn new(pid: Pid) -> Self {
        ProcessContext {
            pid,
        }
    }

}
