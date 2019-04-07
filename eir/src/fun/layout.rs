use std::collections::HashSet;
use ::cranelift_entity::{ PrimaryMap, SecondaryMap, ListPool, EntityList,
                          EntitySet, entity_impl };
use super::{ Ebb, Op };

#[derive(Clone, Debug, Default)]
pub struct EbbNode {
    pub prev: Option<Ebb>,
    pub next: Option<Ebb>,
    pub first_op: Option<Op>,
    pub last_op: Option<Op>,
}

#[derive(Clone, Debug, Default)]
pub struct OpNode {
    pub ebb: Option<Ebb>,
    pub prev: Option<Op>,
    pub next: Option<Op>,
}

use std::sync::Mutex;
lazy_static::lazy_static! {
    static ref A: Mutex<usize> = Mutex::new(0);
}

#[derive(Debug)]
pub struct Layout {
    pub ebbs: SecondaryMap<Ebb, EbbNode>,
    pub ops: SecondaryMap<Op, OpNode>,
    pub first_ebb: Option<Ebb>,
    pub last_ebb: Option<Ebb>,
    pub idx: usize,
}
impl Layout {

    pub fn new() -> Self {
        let mut next_idx_l = A.lock().unwrap();
        let idx = *next_idx_l;
        *next_idx_l += 1;
        Layout {
            ebbs: SecondaryMap::new(),
            ops: SecondaryMap::new(),
            first_ebb: None,
            last_ebb: None,
            idx: idx,
        }
    }

    pub fn insert_ebb_first(&mut self, ebb: Ebb) {
        //println!("insert_ebb_first {} {:?}", self.idx, ebb);
        assert!(self.first_ebb.is_none());
        assert!(self.last_ebb.is_none());
        self.first_ebb = Some(ebb);
        self.last_ebb = Some(ebb);

        //self.validate();
    }

    pub fn remove_ebb(&mut self, ebb: Ebb) {
        //println!("remove_ebb {} {:?}", self.idx, ebb);
        let next = self.ebbs[ebb].next;
        let prev = self.ebbs[ebb].prev;

        if let Some(next) = next {
            assert!(self.last_ebb != Some(ebb));
            assert!(self.ebbs[next].prev == Some(ebb));
            self.ebbs[next].prev = prev;
        } else {
            assert!(self.last_ebb == Some(ebb));
            self.last_ebb = prev;
        }

        if let Some(prev) = prev {
            assert!(self.first_ebb != Some(ebb));
            assert!(self.ebbs[prev].next == Some(ebb));
            self.ebbs[prev].next = next;
        } else {
            assert!(self.first_ebb == Some(ebb));
            self.first_ebb = next;
        }

        //self.validate();
    }

    pub fn concat_ebb(&mut self, ebb1: Ebb, ebb2: Ebb) {
        assert!(ebb1 != ebb2);
        //println!("concat_ebb {} {:?} {:?}", self.idx, ebb1, ebb2);
        let mut last_op = self.ebbs[ebb1].last_op;
        let mut curr_op = self.ebbs[ebb2].first_op;

        // Update last op in ebb1 to point to first in ebb2
        if let Some(inner) = last_op {
            self.ops[inner].next = curr_op;
        } else {
            assert!(self.ebbs[ebb1].first_op.is_none());
            self.ebbs[ebb1].first_op = curr_op;
        }

        // Update ebb for each op in ebb2
        while curr_op.is_some() {
            let curr_op_i = curr_op.unwrap();
            assert!(self.ops[curr_op_i].ebb == Some(ebb2));
            self.ops[curr_op_i].ebb = Some(ebb1);
            self.ops[curr_op_i].prev = last_op;
            last_op = curr_op;
            curr_op = self.ops[curr_op_i].next;
        }

        // Update last_op in ebb1
        self.ebbs[ebb1].last_op = last_op;

        // Zero out ebb2 for good karma
        self.ebbs[ebb2].first_op = None;
        self.ebbs[ebb2].last_op = None;

        //self.validate();

        // Remove ebb2 from layout
        self.remove_ebb(ebb2);
    }

    pub fn split_ebb_into(&mut self, op: Op, ebb2: Ebb) {
        let ebb1 = self.ops[op].ebb.unwrap();
        assert!(ebb1 != ebb2);

        let next = self.ops[op].next;

        self.ops[op].next = None;
        self.ebbs[ebb1].last_op = Some(op);

        let mut last_op = None;
        let mut curr_op = next;

        self.ebbs[ebb2].first_op = curr_op;

        while curr_op.is_some() {
            let curr_op_i = curr_op.unwrap();
            assert!(self.ops[curr_op_i].ebb == Some(ebb1));
            self.ops[curr_op_i].ebb = Some(ebb2);
            self.ops[curr_op_i].prev = last_op;
            last_op = curr_op;
            curr_op = self.ops[curr_op_i].next;
        }

        self.ebbs[ebb2].last_op = last_op;
    }

    pub fn remove_op(&mut self, op: Op) {
        //println!("remove_op {} {:?}", self.idx, op);
        let prev = self.ops[op].prev;
        let next = self.ops[op].next;
        let ebb = self.ops[op].ebb.unwrap();

        if let Some(next) = next {
            assert!(self.ops[next].prev == Some(op));
            self.ops[next].prev = prev;
        } else {
            assert!(self.ebbs[ebb].last_op == Some(op));
            self.ebbs[ebb].last_op = prev;
        }

        if let Some(prev) = prev {
            assert!(self.ops[prev].next == Some(op));
            self.ops[prev].next = next;
        } else {
            assert!(self.ebbs[ebb].first_op == Some(op));
            self.ebbs[ebb].first_op = next;
        }

        //self.validate();
    }

    pub fn insert_ebb_after(&mut self, prev: Ebb, ebb: Ebb) {
        //println!("insert_ebb_after {} {:?} {:?}", self.idx, prev, ebb);
        // TODO: Validate not inserted
        let next = self.ebbs[prev].next;
        self.ebbs[prev].next = Some(ebb);
        self.ebbs[ebb].prev = Some(prev);
        self.ebbs[ebb].next = next;
        if let Some(next) = next {
            self.ebbs[next].prev = Some(ebb);
        }
        if self.last_ebb == Some(prev) {
            self.last_ebb = Some(ebb);
        }
        //self.validate();
    }

    pub fn insert_op_after(&mut self, ebb: Ebb, prev_op: Option<Op>, op: Op) {
        //println!("insert_op_after {} {:?} {:?} {:?}", self.idx, ebb, prev_op, op);
        assert!(self.ops[op].ebb == None);
        self.ops[op].ebb = Some(ebb);

        if let Some(prev_op) = prev_op {
            // If a previous operation is selected,

            let next = self.ops[prev_op].next;
            // Update previous and next of current
            self.ops[op].prev = Some(prev_op);
            self.ops[op].next = next;

            // Set the next of previous to current
            self.ops[prev_op].next = Some(op);

            // If there is a next operation,
            // set it's previous to current
            // else,
            // set the last Ebb Op to current
            if let Some(next) = next {
                assert!(self.ops[next].prev == Some(prev_op));
                self.ops[next].prev = Some(op);
            } else {
                assert!(self.ebbs[ebb].last_op == Some(prev_op));
                self.ebbs[ebb].last_op = Some(op);
            }
        } else {
            // No previous operation selected, insert at beginning

            // Get the Op that is at block start
            let next = self.ebbs[ebb].first_op;

            // Set the next of the current Op to that
            self.ops[op].next = next;

            // Set the first Op of the Ebb to the current Op
            self.ebbs[ebb].first_op = Some(op);

            if let Some(next) = next {
                // If there was an Op after this one, set its previous to current
                assert!(self.ops[next].prev == None);
                self.ops[next].prev = Some(op);
            } else {
                // If there was no Op after this one, set the last Op to current
                assert!(self.ebbs[ebb].last_op == None);
                self.ebbs[ebb].last_op = Some(op);
            }

        }

        //self.validate();
    }

    fn validate(&self) {
        let mut seen_ebbs = Vec::new();
        let mut seen_ops = Vec::new();

        // Traverse Ebbs forward
        let mut last_ebb = None;
        let mut curr_ebb = self.first_ebb;
        while curr_ebb.is_some() {
            let curr_ebb_i = curr_ebb.unwrap();
            assert!(self.ebbs[curr_ebb_i].prev == last_ebb);
            seen_ebbs.push(curr_ebb_i);

            seen_ops.clear();

            // Traverse Ops forward
            let mut last_op = None;
            let mut curr_op = self.ebbs[curr_ebb_i].first_op;
            while curr_op.is_some() {
                let curr_op_i = curr_op.unwrap();
                assert!(self.ops[curr_op_i].prev == last_op);
                assert!(self.ops[curr_op_i].ebb == Some(curr_ebb_i));

                seen_ops.push(curr_op_i);

                last_op = curr_op;
                curr_op = self.ops[curr_op_i].next;
            }
            //println!("{:?} {:?}", self.ebbs[curr_ebb_i].last_op, last_op);
            assert!(self.ebbs[curr_ebb_i].last_op == last_op);

            // Traverse Ops backward
            let mut last_op = None;
            let mut curr_op = self.ebbs[curr_ebb_i].last_op;
            let mut i = 0;
            while curr_op.is_some() {
                let curr_op_i = curr_op.unwrap();
                assert!(self.ops[curr_op_i].next == last_op);
                assert!(self.ops[curr_op_i].ebb == Some(curr_ebb_i));

                assert!(seen_ops[seen_ops.len() - 1 - i] == curr_op_i);

                last_op = curr_op;
                curr_op = self.ops[curr_op_i].prev;
                i += 1;
            }
            assert!(self.ebbs[curr_ebb_i].first_op == last_op);


            last_ebb = curr_ebb;
            curr_ebb = self.ebbs[curr_ebb_i].next;
        }
        assert!(self.last_ebb == last_ebb);

        // Traverse Ebbs backward
        let mut last_ebb = None;
        let mut curr_ebb = self.last_ebb;
        let mut i = 0;
        while curr_ebb.is_some() {
            let curr_ebb_i = curr_ebb.unwrap();
            assert!(self.ebbs[curr_ebb_i].next == last_ebb);
            //seen_ebbs_backward.insert(curr_ebb_i);

            assert!(seen_ebbs[seen_ebbs.len() - 1 - i] == curr_ebb_i);

            last_ebb = curr_ebb;
            curr_ebb = self.ebbs[curr_ebb_i].prev;
            i += 1;
        }
        assert!(self.first_ebb == last_ebb);

    }

}

#[cfg(test)]
mod test {
    use super::Layout;
    use super::super::{ Ebb, Op };

    #[test]
    fn test_basic() {
        let mut layout = Layout::new();

        // Insert Ebb
        let first_ebb = Ebb(0);
        layout.insert_ebb_first(first_ebb);
        assert!(layout.ebbs[first_ebb].next == None);
        assert!(layout.ebbs[first_ebb].prev == None);
        assert!(layout.ebbs[first_ebb].first_op == None);
        assert!(layout.ebbs[first_ebb].last_op == None);

        // E0:

        // Insert single Op
        let first_op = Op(0);
        layout.insert_op_after(first_ebb, None, first_op);
        assert!(layout.ebbs[first_ebb].next == None);
        assert!(layout.ebbs[first_ebb].prev == None);
        assert!(layout.ebbs[first_ebb].first_op == Some(first_op));
        assert!(layout.ebbs[first_ebb].last_op == Some(first_op));

        // E0:
        //     O0

        // Insert second Op
        let second_op = Op(1);
        layout.insert_op_after(first_ebb, Some(first_op), second_op);
        assert!(layout.ebbs[first_ebb].first_op == Some(first_op));
        assert!(layout.ebbs[first_ebb].last_op == Some(second_op));
        assert!(layout.ops[first_op].prev == None);
        assert!(layout.ops[first_op].next == Some(second_op));
        assert!(layout.ops[second_op].prev == Some(first_op));
        assert!(layout.ops[second_op].next == None);

        // E0:
        //     O0
        //     O1

        // Second Ebb
        let second_ebb = Ebb(1);
        layout.insert_ebb_after(first_ebb, second_ebb);
        assert!(layout.ebbs[first_ebb].prev == None);
        assert!(layout.ebbs[first_ebb].next == Some(second_ebb));
        assert!(layout.ebbs[second_ebb].prev == Some(first_ebb));
        assert!(layout.ebbs[second_ebb].next == None);

        // E0:
        //     O1
        //     O2
        // E1:

        // Insert Op between the two
        let new_op = Op(2);
        layout.insert_op_after(first_ebb, Some(first_op), new_op);
        assert!(layout.ebbs[first_ebb].first_op == Some(first_op));
        assert!(layout.ebbs[first_ebb].last_op == Some(second_op));
        assert!(layout.ops[first_op].next == Some(new_op));
        assert!(layout.ops[new_op].prev == Some(first_op));
        assert!(layout.ops[new_op].next == Some(second_op));
        assert!(layout.ops[second_op].prev == Some(new_op));

        // E0:
        //     O0
        //     O2
        //     O1
        // E1:

        // Remove Op between
        layout.remove_op(new_op);
        assert!(layout.ebbs[first_ebb].prev == None);
        assert!(layout.ebbs[first_ebb].next == Some(second_ebb));
        assert!(layout.ebbs[second_ebb].prev == Some(first_ebb));
        assert!(layout.ebbs[second_ebb].next == None);

        // E0:
        //     O0
        //     O1
        // E1:

        // Remove second Op
        layout.remove_op(second_op);
        assert!(layout.ops[first_op].next == None);
        assert!(layout.ops[first_op].prev == None);
        assert!(layout.ebbs[first_ebb].first_op == Some(first_op));
        assert!(layout.ebbs[first_ebb].last_op == Some(first_op));

        // E0:
        //     O0
        // E1:

        // Insert Op in second Ebb
        let second_ebb_op = Op(3);
        layout.insert_op_after(second_ebb, None, second_ebb_op);
        assert!(layout.ops[second_ebb_op].next == None);
        assert!(layout.ops[second_ebb_op].prev == None);
        assert!(layout.ebbs[second_ebb].first_op == Some(second_ebb_op));
        assert!(layout.ebbs[second_ebb].last_op == Some(second_ebb_op));

        // E0:
        //     O0
        //     O3

        layout.concat_ebb(first_ebb, second_ebb);
        assert!(layout.ebbs[first_ebb].last_op == Some(second_ebb_op));
        assert!(layout.ops[second_ebb_op].ebb == Some(first_ebb));
        assert!(layout.ops[second_ebb_op].next == None);
        assert!(layout.ops[second_ebb_op].prev == Some(first_op));
        assert!(layout.ops[first_op].next == Some(second_ebb_op));
        assert!(layout.ops[first_op].prev == None);

        // E0:
        //     O3

        layout.remove_op(first_op);
        assert!(layout.ops[second_ebb_op].prev == None);
        assert!(layout.ops[second_ebb_op].next == None);

    }

    #[test]
    fn test_concat() {
        let mut layout = Layout::new();
        let e0 = Ebb(0);
        let e1 = Ebb(1);
        let o0 = Op(0);
        let o1 = Op(1);
        layout.insert_ebb_first(e0);
        layout.insert_op_after(e0, None, o0);
        layout.insert_ebb_after(e0, e1);
        layout.insert_op_after(e1, None, o1);
        layout.concat_ebb(e0, e1);
    }

    #[test]
    fn test_assertion_fail_trace_1() {
        let mut layout = Layout::new();

        layout.insert_ebb_first(Ebb(0));
        layout.insert_ebb_after(Ebb(0), Ebb(1));
        layout.insert_op_after(Ebb(1), None, Op(0));
        layout.insert_ebb_after(Ebb(1), Ebb(2));
        layout.insert_op_after(Ebb(2), None, Op(1));
        layout.insert_op_after(Ebb(0), None, Op(2));
        layout.insert_ebb_after(Ebb(0), Ebb(3));
        layout.insert_ebb_after(Ebb(0), Ebb(4));
        layout.insert_op_after(Ebb(0), Some(Op(2)), Op(3));
        layout.insert_ebb_after(Ebb(0), Ebb(5));
        layout.insert_op_after(Ebb(5), None, Op(4));
        layout.insert_ebb_after(Ebb(5), Ebb(6));
        layout.insert_ebb_after(Ebb(5), Ebb(7));
        layout.insert_op_after(Ebb(3), None, Op(5));
        layout.insert_op_after(Ebb(6), None, Op(6));
        layout.insert_ebb_after(Ebb(6), Ebb(8));
        layout.insert_op_after(Ebb(6), Some(Op(6)), Op(7));
        layout.insert_op_after(Ebb(6), Some(Op(7)), Op(8));
        layout.insert_op_after(Ebb(6), Some(Op(8)), Op(9));
        layout.insert_op_after(Ebb(6), Some(Op(9)), Op(10));
        layout.insert_op_after(Ebb(6), Some(Op(10)), Op(11));
        layout.insert_op_after(Ebb(6), Some(Op(11)), Op(12));
        layout.insert_op_after(Ebb(6), Some(Op(12)), Op(13));
        layout.insert_op_after(Ebb(6), Some(Op(13)), Op(14));
        layout.insert_op_after(Ebb(6), Some(Op(14)), Op(15));
        layout.insert_op_after(Ebb(6), Some(Op(15)), Op(16));
        layout.insert_op_after(Ebb(6), Some(Op(16)), Op(17));
        layout.insert_op_after(Ebb(8), None, Op(18));
        layout.insert_op_after(Ebb(7), None, Op(19));
        layout.insert_ebb_after(Ebb(7), Ebb(9));
        layout.insert_op_after(Ebb(7), Some(Op(19)), Op(20));
        layout.insert_op_after(Ebb(7), Some(Op(20)), Op(21));
        layout.insert_op_after(Ebb(7), Some(Op(21)), Op(22));
        layout.insert_op_after(Ebb(7), Some(Op(22)), Op(23));
        layout.insert_op_after(Ebb(7), Some(Op(23)), Op(24));
        layout.insert_op_after(Ebb(7), Some(Op(24)), Op(25));
        layout.insert_op_after(Ebb(7), Some(Op(25)), Op(26));
        layout.insert_op_after(Ebb(7), Some(Op(26)), Op(27));
        layout.insert_op_after(Ebb(7), Some(Op(27)), Op(28));
        layout.insert_op_after(Ebb(7), Some(Op(28)), Op(29));
        layout.insert_op_after(Ebb(9), None, Op(30));
        layout.insert_op_after(Ebb(4), None, Op(31));
        layout.insert_ebb_after(Ebb(3), Ebb(10));
        layout.insert_op_after(Ebb(10), None, Op(32));
        layout.insert_ebb_after(Ebb(10), Ebb(11));
        layout.insert_ebb_after(Ebb(10), Ebb(12));
        layout.insert_ebb_after(Ebb(10), Ebb(13));
        layout.insert_ebb_after(Ebb(10), Ebb(14));
        layout.insert_ebb_after(Ebb(10), Ebb(15));
        layout.insert_op_after(Ebb(10), Some(Op(32)), Op(33));
        layout.insert_op_after(Ebb(14), None, Op(34));
        layout.insert_op_after(Ebb(15), None, Op(35));
        layout.insert_op_after(Ebb(11), None, Op(36));
        layout.remove_op(Op(3));
        layout.insert_op_after(Ebb(0), Some(Op(2)), Op(37));
        layout.insert_op_after(Ebb(12), None, Op(38));
        layout.remove_op(Op(6));
        layout.insert_op_after(Ebb(6), None, Op(39));
        layout.insert_op_after(Ebb(6), Some(Op(39)), Op(40));
        layout.insert_op_after(Ebb(13), None, Op(41));
        layout.remove_op(Op(19));
        layout.insert_op_after(Ebb(7), None, Op(42));
        layout.insert_op_after(Ebb(7), Some(Op(42)), Op(43));
        layout.remove_op(Op(18));
        layout.insert_op_after(Ebb(8), None, Op(44));
        layout.remove_op(Op(30));
        layout.insert_op_after(Ebb(9), None, Op(45));
        layout.remove_ebb(Ebb(3));
        layout.remove_op(Op(22));
        layout.remove_op(Op(10));
        layout.remove_op(Op(37));
        layout.concat_ebb(Ebb(0), Ebb(10));
        //layout.remove_ebb(Ebb(10));
        layout.remove_op(Op(45));
        layout.concat_ebb(Ebb(9), Ebb(15));
        //layout.remove_ebb(Ebb(15));
        layout.remove_op(Op(44));
        layout.concat_ebb(Ebb(8), Ebb(14));
        //layout.remove_ebb(Ebb(14));
        layout.remove_op(Op(31));
        layout.insert_op_after(Ebb(4), None, Op(46));
        layout.concat_ebb(Ebb(4), Ebb(2));
        //layout.remove_ebb(Ebb(2));
        layout.remove_op(Op(33));
        layout.concat_ebb(Ebb(0), Ebb(12));
        //layout.remove_ebb(Ebb(12));
        layout.remove_op(Op(35));
    }


}





