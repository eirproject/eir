use ::cranelift_entity::{ EntityList };

use crate::{ Value, ValueKind, LogicOp, IntoValue };
use crate::ConstKind;
use super::{ FunctionBuilder, PrimOpData, PrimOpKind, BinOp };

/// PrimOp constructors
impl<'a> FunctionBuilder<'a> {

    pub fn prim_binop(&mut self, op: BinOp, lhs: Value, rhs: Value) -> Value {
        let mut reads = EntityList::new();
        if op.symmetric() && lhs >= rhs {
            reads.extend([rhs, lhs].iter().cloned(), &mut self.fun.pool.value);
        } else {
            reads.extend([lhs, rhs].iter().cloned(), &mut self.fun.pool.value);
        }

        let primop = self.fun.primops.push(PrimOpData {
            op: PrimOpKind::BinOp(op),
            reads,
        }, &self.fun.pool);
        self.fun.values.push(ValueKind::PrimOp(primop))
    }

    /// This will construct a tuple.
    /// If all values are constants, a constant tuple is created.
    /// Otherwise, a tuple PrimOp is created.
    pub fn prim_tuple(&mut self, values: &[Value]) -> Value {
        if values.iter().all(|v| self.fun.value_const(*v).is_some()) {
            let mut entries = EntityList::new();
            for val in values {
                let cons = self.fun.value_const(*val).unwrap();
                entries.push(cons, &mut self.fun.constant_container.const_pool);
            }

            let cons = self.cons_mut().from(ConstKind::Tuple {
                entries,
            });
            self.value(cons)
        } else {
            let mut reads = EntityList::new();
            for val in values {
                reads.push(*val, &mut self.fun.pool.value);
            }

            let primop = self.fun.primops.push(PrimOpData {
                op: PrimOpKind::Tuple,
                reads,
            }, &self.fun.pool);
            self.fun.values.push(ValueKind::PrimOp(primop))
        }
    }

    /// This will construct a new map.
    /// If all values are constants, a constant map is created.
    /// Otherwise, a map PrimOp is created.
    pub fn prim_map(&mut self, keys: &[Value], values: &[Value]) -> Value {
        if keys.iter().all(|v| self.fun.value_const(*v).is_some())
            && values.iter().all(|v| self.fun.value_const(*v).is_some())
        {
            let mut const_pair = self.const_pair_buf.take().unwrap();
            assert!(const_pair.is_empty());

            const_pair.extend(
                keys.iter().map(|v| self.fun.value_const(*v).unwrap())
                    .zip(values.iter().map(|v| self.fun.value_const(*v).unwrap()))
                    .map(|(k, v)| [k, v])
            );
            const_pair.sort_by(|[k1, _], [k2, _]| k1.cmp(k2));

            let mut key_list = EntityList::new();
            key_list.extend(const_pair.iter().map(|[k, _]| *k), &mut self.cons_mut().const_pool);

            let mut val_list = EntityList::new();
            val_list.extend(const_pair.iter().map(|[_, v]| *v), &mut self.cons_mut().const_pool);

            const_pair.clear();
            self.const_pair_buf = Some(const_pair);

            let cons = self.cons_mut().from(ConstKind::Map { keys: key_list, values: val_list });
            self.value(cons)
        } else {
            let mut value_pair = self.value_pair_buf.take().unwrap();
            assert!(value_pair.is_empty());

            value_pair.extend(
                keys.iter().cloned()
                    .zip(values.iter().cloned())
                    .map(|(k, v)| [k, v])
            );
            value_pair.sort_by(|[k1, _], [k2, _]| k1.cmp(k2));

            let mut entries_list = EntityList::new();
            entries_list.extend(value_pair.iter().flatten().cloned(), &mut self.fun.pool.value);

            value_pair.clear();
            self.value_pair_buf = Some(value_pair);

            let primop = self.fun.primops.push(PrimOpData {
                op: PrimOpKind::Map,
                reads: entries_list,
            }, &self.fun.pool);
            self.fun.values.push(ValueKind::PrimOp(primop))
        }
    }

    pub fn prim_list_cell(&mut self, head: Value, tail: Value) -> Value {
        if let (Some(head_v), Some(tail_v)) = (self.fun.value_const(head), self.fun.value_const(tail)) {
            let cons = self.cons_mut().from(ConstKind::ListCell {
                head: head_v,
                tail: tail_v,
            });
            self.value(cons)
        } else {
            let mut entries_list = EntityList::new();
            entries_list.push(head, &mut self.fun.pool.value);
            entries_list.push(tail, &mut self.fun.pool.value);

            let primop = self.fun.primops.push(PrimOpData {
                op: PrimOpKind::ListCell,
                reads: entries_list,
            }, &self.fun.pool);
            self.fun.values.push(ValueKind::PrimOp(primop))
        }
    }

    pub fn prim_value_list(&mut self, values: &[Value]) -> Value {
        assert!(values.iter().all(|v| {
            self.fun.value_primop(*v)
                .map(|p| self.fun.primop_kind(p))
                != Some(&PrimOpKind::ValueList)
        }));

        let mut entries_list = EntityList::new();
        entries_list.extend(values.iter().cloned(), &mut self.fun.pool.value);

        let primop = self.fun.primops.push(PrimOpData {
            op: PrimOpKind::ValueList,
            reads: entries_list,
        }, &self.fun.pool);
        self.fun.values.push(ValueKind::PrimOp(primop))
    }

    pub(crate) fn prim_value_list_from_entity_list(
        &mut self, values: EntityList<Value>) -> Value {
        let num = values.len(&self.fun.pool.value);
        for n in 0..num {
            let val = values.get(n, &self.fun.pool.value).unwrap();
            if let Some(prim) = self.fun.value_primop(val) {
                assert!(self.fun.primop_kind(prim) != &PrimOpKind::ValueList);
            }
        }

        let primop = self.fun.primops.push(PrimOpData {
            op: PrimOpKind::ValueList,
            reads: values,
        }, &self.fun.pool);
        self.fun.values.push(ValueKind::PrimOp(primop))
    }

    pub fn prim_logic_op(&mut self, op: LogicOp, values: &[Value]) -> Value {
        match (op, values.len()) {
            (LogicOp::And, 0) => return self.value(true),
            (LogicOp::And, 1) => return values[0],
            (LogicOp::Or, 0) => return self.value(false),
            (LogicOp::Or, 1) => return values[0],
            (LogicOp::Eq, 0) => return self.value(true),
            (LogicOp::Eq, 1) => return self.value(true),
            _ => (),
        }

        if values.iter().all(|v| self.fun.value_const(*v).is_some()) {
            let true_const = self.value(true);
            let false_const = self.value(false);
            match op {
                LogicOp::And => {
                    let mut acc = true;
                    for val in values {
                        if *val == false_const {
                            acc = false;
                        } else {
                            assert!(*val == true_const);
                        }
                    }
                    self.value(acc)
                }
                LogicOp::Or => {
                    let mut acc = false;
                    for val in values {
                        if *val == true_const {
                            acc = true;
                        } else {
                            assert!(*val == false_const);
                        }
                    }
                    self.value(acc)
                }
                LogicOp::Eq => {
                    unimplemented!()
                }
            }
        } else {
            let mut entries_list = EntityList::new();
            entries_list.extend(values.iter().cloned(), &mut self.fun.pool.value);

            let primop = self.fun.primops.push(PrimOpData {
                op: PrimOpKind::LogicOp(op),
                reads: entries_list,
            }, &self.fun.pool);
            self.fun.values.push(ValueKind::PrimOp(primop))
        }
    }

    pub fn prim_capture_function<M, F, A>(
        &mut self,
        m: M, f: F, a: A) -> Value
    where
        M: IntoValue, F: IntoValue, A: IntoValue,
    {
        let m_val = self.value(m);
        let f_val = self.value(f);
        let a_val = self.value(a);

        let mut entries_list = EntityList::new();
        entries_list.push(m_val, &mut self.fun.pool.value);
        entries_list.push(f_val, &mut self.fun.pool.value);
        entries_list.push(a_val, &mut self.fun.pool.value);

        let primop = self.fun.primops.push(PrimOpData {
            op: PrimOpKind::CaptureFunction,
            reads: entries_list,
        }, &self.fun.pool);
        self.fun.values.push(ValueKind::PrimOp(primop))
    }

    pub fn prim_from_kind(&mut self, op: PrimOpKind, vals: &[Value]) -> Value {
        match op {
            PrimOpKind::ValueList => {
                self.prim_value_list(vals)
            }
            PrimOpKind::Tuple => {
                self.prim_tuple(vals)
            }
            PrimOpKind::CaptureFunction => {
                assert!(vals.len() == 3);
                self.prim_capture_function(vals[0], vals[1], vals[2])
            }
            PrimOpKind::LogicOp(op) => {
                self.prim_logic_op(op, vals)
            }
            PrimOpKind::BinOp(op) => {
                assert!(vals.len() == 2);
                self.prim_binop(op, vals[0], vals[1])
            }
            PrimOpKind::ListCell => {
                assert!(vals.len() == 2);
                self.prim_list_cell(vals[0], vals[1])
            }
            p => unimplemented!("{:?}", p),
        }
    }

}
