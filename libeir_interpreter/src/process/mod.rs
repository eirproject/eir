use std::rc::Rc;
use std::collections::HashMap;



use libeir_ir::{ FunctionIdent, Block, Value, OpKind, BinOp, ValueKind, PrimOpKind };
use libeir_ir::{ MapPutUpdate };
use libeir_ir::{ BinaryEntrySpecifier, Endianness };
use libeir_ir::constant::{ Const, ConstKind, AtomicTerm };
use libeir_intern::{ Symbol, Ident };

use libeir_util::binary::{ BitVec, BitSlice, integer_to_carrier, Endian };

use crate::term::{ Term, Pid, ErlEq, MapTerm };
use crate::module::{ ModuleType, ErlangModule, ErlangFunction, NativeModule, NativeReturn };
use crate::vm::VMState;

mod r#match;

#[derive(Debug)]
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
                        println!("{}", ident);
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
                NativeReturn::Throw { typ, reason } => {
                    Some(TermCall {
                        fun: args[1].clone(),
                        args: vec![typ, reason, Term::Nil.into()],
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
            ConstKind::Atomic(AtomicTerm::BigInt(int)) => {
                Term::Integer(int.0.clone()).into()
            }
            ConstKind::Atomic(AtomicTerm::Float(flt)) => {
                Term::Float(flt.0.into()).into()
            }
            ConstKind::Atomic(AtomicTerm::Binary(bin)) => {
                Term::Binary(Rc::new(bin.0.clone().into())).into()
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
            ConstKind::Tuple { entries } => {
                let vec = entries.as_slice(&fun.fun.cons().const_pool)
                    .iter()
                    .map(|e| self.make_const_term(fun, *e))
                    .collect::<Vec<_>>();
                Term::Tuple(vec).into()
            }
            ConstKind::Map { keys, values } => {
                assert!(keys.len(&fun.fun.cons().const_pool)
                        == values.len(&fun.fun.cons().const_pool));

                let mut map = MapTerm::new();
                for (key, val) in keys.as_slice(&fun.fun.cons().const_pool).iter()
                    .zip(values.as_slice(&fun.fun.cons().const_pool).iter())
                {
                    let key_v = self.make_const_term(fun, *key);
                    let val_v = self.make_const_term(fun, *val);
                    map.insert(key_v, val_v);
                }

                Term::Map(map).into()
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
            OpKind::TraceCaptureRaw => {
                TermCall {
                    fun: self.make_term(fun, reads[0]),
                    args: vec![Term::Nil.into()],
                }
            }
            OpKind::Match { branches } => {
                self::r#match::match_op(self, fun, branches, block)
                //let branches_elems = Term::as_value_list(
                //    &self.make_term(fun, reads[0]));

                //let unpack_term = self.make_term(fun, reads[1]);

                ////println!("MATCH START {:?}", unpack_term);
                //for (idx, kind) in branches.iter().enumerate() {
                //    let branch_args = Term::as_value_list(
                //        &self.make_term(fun, reads[idx + 2]));
                //    //println!("MATCH KIND {:?} {:?}", kind, branch_args);
                //    match kind {
                //        MatchKind::Value => {
                //            assert!(branch_args.len() == 1);
                //            if unpack_term.erl_exact_eq(&*branch_args[0]) {
                //                return TermCall {
                //                    fun: branches_elems[idx].clone(),
                //                    args: vec![],
                //                };
                //            }
                //        }
                //        MatchKind::ListCell => {
                //            assert!(branch_args.len() == 0);
                //            match &*unpack_term {
                //                Term::ListCell(head, tail) => {
                //                    return TermCall {
                //                        fun: branches_elems[idx].clone(),
                //                        args: vec![head.clone(), tail.clone()],
                //                    };
                //                }
                //                _ => (),
                //            }
                //        }
                //        MatchKind::Tuple(len) => {
                //            assert!(branch_args.len() == 0);
                //            match &*unpack_term {
                //                Term::Tuple(elems) if elems.len() == *len => {
                //                    return TermCall {
                //                        fun: branches_elems[idx].clone(),
                //                        args: elems.clone(),
                //                    };
                //                }
                //                _ => (),
                //            }
                //        }
                //        MatchKind::Type(BasicType::Map) => {
                //            assert!(branch_args.len() == 0);
                //            match &*unpack_term {
                //                Term::Map(_) => {
                //                    return TermCall {
                //                        fun: branches_elems[idx].clone(),
                //                        args: vec![],
                //                    };
                //                }
                //                _ => (),
                //            }
                //        }
                //        MatchKind::MapItem => {
                //            assert!(branch_args.len() == 1);
                //            match &*unpack_term {
                //                Term::Map(map) => {
                //                    if let Some(v) = map.get(&branch_args[0]) {
                //                        return TermCall {
                //                            fun: branches_elems[idx].clone(),
                //                            args: vec![v.clone()],
                //                        };
                //                    }
                //                }
                //                _ => unreachable!(),
                //            }
                //        }
                //        MatchKind::Binary(BinaryEntrySpecifier::Integer {
                //            signed: false, unit: 1, ..
                //        }) => {
                //            match &*unpack_term {
                //                Term::Binary(bin) => {
                //                    if bin.len() >= 1 {
                //                        return TermCall {
                //                            fun: branches_elems[idx].clone(),
                //                            args: vec![
                //                                Term::Integer(bin[0].into()).into(),
                //                                Term::Binary(bin[1..].to_owned()).into(),
                //                            ],
                //                        };
                //                    }
                //                }
                //                _ => (),
                //            }
                //        }
                //        MatchKind::Binary(BinaryEntrySpecifier::Bytes { unit: 8 }) => {
                //            match &*unpack_term {
                //                Term::Binary(bin) => {
                //                    if bin.len() >= 1 {
                //                        return TermCall {
                //                            fun: branches_elems[idx].clone(),
                //                            args: vec![
                //                                unpack_term.clone(),
                //                                Term::Binary(vec![]).into(),
                //                            ],
                //                        };
                //                    }
                //                }
                //                _ => (),
                //            }
                //        }
                //        MatchKind::Wildcard => {
                //            assert!(branch_args.len() == 0);
                //            return TermCall {
                //                fun: branches_elems[idx].clone(),
                //                args: vec![],
                //            };
                //        }
                //        kind => unimplemented!("{:?}", kind),
                //    }
                //}
                //panic!()
            }
            OpKind::BinaryPush { specifier } => {
                let bin_term = self.make_term(fun, reads[2]);
                let mut bin = match &*bin_term {
                    Term::Binary(bin) => (**bin).clone(),
                    Term::BinarySlice { buf, bit_offset, bit_length } => {
                        let slice = BitSlice::with_offset_length(
                            &**buf, *bit_offset, *bit_length);
                        let mut new = BitVec::new();
                        new.push(slice);
                        new
                    }
                    _ => panic!(),
                };

                let val_term = self.make_term(fun, reads[3]);

                assert!(reads.len() == 4 || reads.len() == 5);
                let size_term = reads.get(4).map(|r| self.make_term(fun, *r));

                match specifier {
                    BinaryEntrySpecifier::Integer {
                        signed: _, unit, endianness } =>
                    {
                        let size = size_term.unwrap().as_usize().unwrap();
                        let bit_size = *unit as usize * size;

                        let endian = match *endianness {
                            Endianness::Big => Endian::Big,
                            Endianness::Little => Endian::Little,
                            Endianness::Native => Endian::Big,
                        };

                        let val = val_term.as_integer().unwrap().clone();
                        let carrier = integer_to_carrier(
                            val, bit_size, endian);

                        bin.push(carrier);

                        //let bit_size = size * (*unit as usize);
                        //assert!(bit_size % 8 == 0);
                        //let byte_size = (bit_size + 7) / 8;

                        //let mut val = val_term.as_integer().unwrap().clone();
                        //let sign = val >= 0;
                        //let sign_xor = if sign { 0x00 } else { 0xff };

                        //if *signed {
                        //    if !sign {
                        //        val += 1;
                        //    }
                        //} else {
                        //    assert!(&val >= &0);
                        //}

                        //let digits = val.to_digits::<u8>(match endianness {
                        //    Endianness::Big => Order::Msf,
                        //    Endianness::Little => unimplemented!(),
                        //    Endianness::Native => Order::Msf,
                        //});

                        //let bef = bin.len();

                        //let start;
                        //if byte_size < digits.len() {
                        //    start = 0;
                        //    // zero padding
                        //    for _ in 0..(digits.len() - byte_size) {
                        //        bin.push(sign_xor);
                        //    }
                        //} else {
                        //    start = digits.len() - byte_size;
                        //}

                        //// actual data
                        //for n in start..digits.len() {
                        //    bin.push(digits[n] ^ sign_xor);
                        //}

                        //assert!(bin.len() == bef + byte_size);

                        //let val = if *signed {
                        //    let val = val_term.as_i64().unwrap();
                        //    assert!(val < 128 && val >= -128);
                        //    val as u8
                        //} else {
                        //    println!("{:?}", val_term);
                        //    let val = val_term.as_i64().unwrap();
                        //    assert!(val < 256 && val >= 0);
                        //    val as u8
                        //};

                        //bin.push(val as u8);
                    }
                    BinaryEntrySpecifier::Bytes { unit: 1 } => {
                        let binary = val_term.as_binary().unwrap();

                        if let Some(size_term) = size_term {
                            dbg!(&size_term, &binary);
                            assert!(size_term.as_usize().unwrap() == binary.len());
                        }

                        bin.push(binary);
                    }
                    k => unimplemented!("{:?}", k),
                }

                return TermCall {
                    fun: self.make_term(fun, reads[0]),
                    args: vec![Term::Binary(bin.into()).into()],
                };
            }
            OpKind::MapPut { action } => {
                let map_term = self.make_term(fun, reads[2]);
                let mut map = map_term.as_map().unwrap().clone();

                let mut idx = 3;
                for action in action.iter() {
                    let key = self.make_term(fun, reads[idx]);
                    let val = self.make_term(fun, reads[idx+1]);
                    idx += 2;

                    let replaced = map.insert(key, val);
                    if *action == MapPutUpdate::Update {
                        assert!(replaced)
                    }
                }

                TermCall {
                    fun: self.make_term(fun, reads[0]),
                    args: vec![Term::Map(map).into()],
                }
            }
            OpKind::Unreachable => {
                println!("==== Reached OpKind::Unreachable! ====");
                println!("Fun: {} Block: {}", fun.fun.ident(), block);
                unreachable!();
            }
            kind => unimplemented!("{:?}", kind),
        }
    }

}

pub struct ProcessContext {
    pub pid: Pid,
    pub dict: Vec<(Rc<Term>, Rc<Term>)>,
}

impl ProcessContext {

    pub fn new(pid: Pid) -> Self {
        ProcessContext {
            pid,
            dict: Vec::new(),
        }
    }

}
