use std::rc::Rc;
use std::collections::HashMap;

use libeir_ir::{ FunctionIdent, Block, Value, OpKind, BinOp, ValueKind, PrimOpKind };
use libeir_ir::constant::{ Const, ConstKind, AtomicTerm };
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

    fn make_const_term(&self, fun: &ErlangFunction, const_val: Const) -> Rc<Term> {
        match fun.fun.cons().const_kind(const_val) {
            ConstKind::Atomic(AtomicTerm::Atom(atom)) => {
                Term::Atom(atom.0).into()
            }
            ConstKind::Atomic(AtomicTerm::Int(int)) => {
                Term::Integer(int.0.into()).into()
            }
            ConstKind::Atomic(AtomicTerm::Float(flt)) => {
                Term::Float(flt.0).into()
            }
            ConstKind::Atomic(AtomicTerm::Nil) => {
                Term::Nil.into()
            }
            ConstKind::ListCell { head, tail } => {
                Term::ListCell(
                    self.make_const_term(fun, *head),
                    self.make_const_term(fun, *tail),
                ).into()
            }
            kind => unimplemented!("{:?}", kind),
        }
    }

    fn make_term(&self, fun: &ErlangFunction, value: Value) -> Rc<Term> {
        match fun.fun.value_kind(value) {
            ValueKind::Block(block) => {
                let live = &fun.live.live[&block];
                let mut env = Vec::new();
                for v in live.iter(&fun.live.pool) {
                    assert!(fun.fun.value_argument(v).is_some());
                    env.push(self.make_term(fun, v));
                }
                Term::BoundLambda { ident: fun.fun.ident().clone(), block, environment: env }.into()
            }
            ValueKind::Argument(_, _) => {
                self.binds[&value].clone()
            }
            ValueKind::Const(cons) => {
                self.make_const_term(fun, cons)
            }
            ValueKind::PrimOp(prim) => {
                let reads = fun.fun.primop_reads(prim);
                match fun.fun.primop_kind(prim) {
                    PrimOpKind::ValueList => {
                        let terms: Vec<_> = reads.iter()
                            .map(|r| self.make_term(fun, *r)).collect();
                        Term::ValueList(terms).into()
                    }
                    PrimOpKind::Tuple => {
                        let terms: Vec<_> = reads.iter()
                            .map(|r| self.make_term(fun, *r)).collect();
                        Term::Tuple(terms).into()
                    }
                    PrimOpKind::ListCell => {
                        assert!(reads.len() == 2);
                        let head = self.make_term(fun, reads[0]);
                        let tail = self.make_term(fun, reads[1]);
                        Term::ListCell(head, tail).into()
                    }
                    PrimOpKind::BinOp(BinOp::Equal) => {
                        assert!(reads.len() == 2);
                        let lhs = self.make_term(fun, reads[0]);
                        let rhs = self.make_term(fun, reads[1]);
                        Term::new_bool(lhs.erl_eq(&*rhs)).into()
                    }
                    kind => unimplemented!("{:?}", kind),
                }
            }
        }
    }

    pub fn run_erlang_op(&mut self, _vm: &VMState, fun: &ErlangFunction, block: Block) -> TermCall {
        let reads = fun.fun.block_reads(block);
        println!("{} {}", fun.fun.ident(), block);
        match fun.fun.block_kind(block).unwrap() {
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
                let call_n = if reads.len() == 4 {
                    let bool_term = self.make_term(fun, reads[3]);
                    match bool_term.as_boolean() {
                        Some(true) => 0,
                        Some(false) => 1,
                        None => 2,
                    }
                } else if reads.len() == 3 {
                    let bool_term = self.make_term(fun, reads[2]);
                    match bool_term.as_boolean() {
                        Some(true) => 0,
                        Some(false) => 1,
                        None => unreachable!(),
                    }
                } else {
                    unreachable!()
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
