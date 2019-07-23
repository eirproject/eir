use ::cranelift_entity::{ EntityList };

use crate::{ Value, ValueKind };
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

}
