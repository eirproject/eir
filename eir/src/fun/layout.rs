use std::collections::HashSet;
use ::cranelift_entity::{ PrimaryMap, SecondaryMap, ListPool, EntityList,
                          EntitySet, entity_impl };
use super::{ Block, Op };

#[derive(Clone, Debug, Default)]
pub struct BlockNode {
    pub prev: Option<Block>,
    pub next: Option<Block>,
    pub first_op: Option<Op>,
    pub last_op: Option<Op>,
}

#[derive(Clone, Debug, Default)]
pub struct OpNode {
    pub block: Option<Block>,
    pub prev: Option<Op>,
    pub next: Option<Op>,
}

use std::sync::Mutex;
lazy_static::lazy_static! {
    static ref A: Mutex<usize> = Mutex::new(0);
}

#[derive(Debug)]
pub struct Layout {
    pub blocks: SecondaryMap<Block, BlockNode>,
    pub ops: SecondaryMap<Op, OpNode>,
    pub first_block: Option<Block>,
    pub last_block: Option<Block>,
    pub idx: usize,
}
impl Layout {

    pub fn new() -> Self {
        let mut next_idx_l = A.lock().unwrap();
        let idx = *next_idx_l;
        *next_idx_l += 1;
        Layout {
            blocks: SecondaryMap::new(),
            ops: SecondaryMap::new(),
            first_block: None,
            last_block: None,
            idx: idx,
        }
    }

    pub fn insert_block_first(&mut self, block: Block) {
        //println!("insert_block_first {} {:?}", self.idx, block);
        assert!(self.first_block.is_none());
        assert!(self.last_block.is_none());
        self.first_block = Some(block);
        self.last_block = Some(block);

        //self.validate();
    }

    pub fn remove_block(&mut self, block: Block) {
        //println!("remove_block {} {:?}", self.idx, block);
        let next = self.blocks[block].next;
        let prev = self.blocks[block].prev;

        if let Some(next) = next {
            assert!(self.last_block != Some(block));
            assert!(self.blocks[next].prev == Some(block));
            self.blocks[next].prev = prev;
        } else {
            assert!(self.last_block == Some(block));
            self.last_block = prev;
        }

        if let Some(prev) = prev {
            assert!(self.first_block != Some(block));
            assert!(self.blocks[prev].next == Some(block));
            self.blocks[prev].next = next;
        } else {
            assert!(self.first_block == Some(block));
            self.first_block = next;
        }

        //self.validate();
    }

    pub fn concat_block(&mut self, block1: Block, block2: Block) {
        assert!(block1 != block2);
        //println!("concat_block {} {:?} {:?}", self.idx, block1, block2);
        let mut last_op = self.blocks[block1].last_op;
        let mut curr_op = self.blocks[block2].first_op;

        // Update last op in block1 to point to first in block2
        if let Some(inner) = last_op {
            self.ops[inner].next = curr_op;
        } else {
            assert!(self.blocks[block1].first_op.is_none());
            self.blocks[block1].first_op = curr_op;
        }

        // Update block for each op in block2
        while curr_op.is_some() {
            let curr_op_i = curr_op.unwrap();
            assert!(self.ops[curr_op_i].block == Some(block2));
            self.ops[curr_op_i].block = Some(block1);
            self.ops[curr_op_i].prev = last_op;
            last_op = curr_op;
            curr_op = self.ops[curr_op_i].next;
        }

        // Update last_op in block1
        self.blocks[block1].last_op = last_op;

        // Zero out block2 for good karma
        self.blocks[block2].first_op = None;
        self.blocks[block2].last_op = None;

        //self.validate();

        // Remove block2 from layout
        self.remove_block(block2);
    }

    pub fn split_block_into(&mut self, op: Op, block2: Block) {
        let block1 = self.ops[op].block.unwrap();
        assert!(block1 != block2);

        let next = self.ops[op].next;

        self.ops[op].next = None;
        self.blocks[block1].last_op = Some(op);

        let mut last_op = None;
        let mut curr_op = next;

        self.blocks[block2].first_op = curr_op;

        while curr_op.is_some() {
            let curr_op_i = curr_op.unwrap();
            assert!(self.ops[curr_op_i].block == Some(block1));
            self.ops[curr_op_i].block = Some(block2);
            self.ops[curr_op_i].prev = last_op;
            last_op = curr_op;
            curr_op = self.ops[curr_op_i].next;
        }

        self.blocks[block2].last_op = last_op;
    }

    pub fn remove_op(&mut self, op: Op) {
        //println!("remove_op {} {:?}", self.idx, op);
        let prev = self.ops[op].prev;
        let next = self.ops[op].next;
        let block = self.ops[op].block.unwrap();

        if let Some(next) = next {
            assert!(self.ops[next].prev == Some(op));
            self.ops[next].prev = prev;
        } else {
            assert!(self.blocks[block].last_op == Some(op));
            self.blocks[block].last_op = prev;
        }

        if let Some(prev) = prev {
            assert!(self.ops[prev].next == Some(op));
            self.ops[prev].next = next;
        } else {
            assert!(self.blocks[block].first_op == Some(op));
            self.blocks[block].first_op = next;
        }

        //self.validate();
    }

    pub fn insert_block_after(&mut self, prev: Block, block: Block) {
        //println!("insert_block_after {} {:?} {:?}", self.idx, prev, block);
        // TODO: Validate not inserted
        let next = self.blocks[prev].next;
        self.blocks[prev].next = Some(block);
        self.blocks[block].prev = Some(prev);
        self.blocks[block].next = next;
        if let Some(next) = next {
            self.blocks[next].prev = Some(block);
        }
        if self.last_block == Some(prev) {
            self.last_block = Some(block);
        }
        //self.validate();
    }

    pub fn insert_op_after(&mut self, block: Block, prev_op: Option<Op>, op: Op) {
        //println!("insert_op_after {} {:?} {:?} {:?}", self.idx, block, prev_op, op);
        assert!(self.ops[op].block == None);
        self.ops[op].block = Some(block);

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
            // set the last Block Op to current
            if let Some(next) = next {
                assert!(self.ops[next].prev == Some(prev_op));
                self.ops[next].prev = Some(op);
            } else {
                assert!(self.blocks[block].last_op == Some(prev_op));
                self.blocks[block].last_op = Some(op);
            }
        } else {
            // No previous operation selected, insert at beginning

            // Get the Op that is at block start
            let next = self.blocks[block].first_op;

            // Set the next of the current Op to that
            self.ops[op].next = next;

            // Set the first Op of the Block to the current Op
            self.blocks[block].first_op = Some(op);

            if let Some(next) = next {
                // If there was an Op after this one, set its previous to current
                assert!(self.ops[next].prev == None);
                self.ops[next].prev = Some(op);
            } else {
                // If there was no Op after this one, set the last Op to current
                assert!(self.blocks[block].last_op == None);
                self.blocks[block].last_op = Some(op);
            }

        }

        //self.validate();
    }

    fn validate(&self) {
        let mut seen_blocks = Vec::new();
        let mut seen_ops = Vec::new();

        // Traverse Blocks forward
        let mut last_block = None;
        let mut curr_block = self.first_block;
        while curr_block.is_some() {
            let curr_block_i = curr_block.unwrap();
            assert!(self.blocks[curr_block_i].prev == last_block);
            seen_blocks.push(curr_block_i);

            seen_ops.clear();

            // Traverse Ops forward
            let mut last_op = None;
            let mut curr_op = self.blocks[curr_block_i].first_op;
            while curr_op.is_some() {
                let curr_op_i = curr_op.unwrap();
                assert!(self.ops[curr_op_i].prev == last_op);
                assert!(self.ops[curr_op_i].block == Some(curr_block_i));

                seen_ops.push(curr_op_i);

                last_op = curr_op;
                curr_op = self.ops[curr_op_i].next;
            }
            //println!("{:?} {:?}", self.blocks[curr_block_i].last_op, last_op);
            assert!(self.blocks[curr_block_i].last_op == last_op);

            // Traverse Ops backward
            let mut last_op = None;
            let mut curr_op = self.blocks[curr_block_i].last_op;
            let mut i = 0;
            while curr_op.is_some() {
                let curr_op_i = curr_op.unwrap();
                assert!(self.ops[curr_op_i].next == last_op);
                assert!(self.ops[curr_op_i].block == Some(curr_block_i));

                assert!(seen_ops[seen_ops.len() - 1 - i] == curr_op_i);

                last_op = curr_op;
                curr_op = self.ops[curr_op_i].prev;
                i += 1;
            }
            assert!(self.blocks[curr_block_i].first_op == last_op);


            last_block = curr_block;
            curr_block = self.blocks[curr_block_i].next;
        }
        assert!(self.last_block == last_block);

        // Traverse Blocks backward
        let mut last_block = None;
        let mut curr_block = self.last_block;
        let mut i = 0;
        while curr_block.is_some() {
            let curr_block_i = curr_block.unwrap();
            assert!(self.blocks[curr_block_i].next == last_block);
            //seen_blocks_backward.insert(curr_block_i);

            assert!(seen_blocks[seen_blocks.len() - 1 - i] == curr_block_i);

            last_block = curr_block;
            curr_block = self.blocks[curr_block_i].prev;
            i += 1;
        }
        assert!(self.first_block == last_block);

    }

}

#[cfg(test)]
mod test {
    use super::Layout;
    use super::super::{ Block, Op };

    #[test]
    fn test_basic() {
        let mut layout = Layout::new();

        // Insert Block
        let first_block = Block(0);
        layout.insert_block_first(first_block);
        assert!(layout.blocks[first_block].next == None);
        assert!(layout.blocks[first_block].prev == None);
        assert!(layout.blocks[first_block].first_op == None);
        assert!(layout.blocks[first_block].last_op == None);

        // E0:

        // Insert single Op
        let first_op = Op(0);
        layout.insert_op_after(first_block, None, first_op);
        assert!(layout.blocks[first_block].next == None);
        assert!(layout.blocks[first_block].prev == None);
        assert!(layout.blocks[first_block].first_op == Some(first_op));
        assert!(layout.blocks[first_block].last_op == Some(first_op));

        // E0:
        //     O0

        // Insert second Op
        let second_op = Op(1);
        layout.insert_op_after(first_block, Some(first_op), second_op);
        assert!(layout.blocks[first_block].first_op == Some(first_op));
        assert!(layout.blocks[first_block].last_op == Some(second_op));
        assert!(layout.ops[first_op].prev == None);
        assert!(layout.ops[first_op].next == Some(second_op));
        assert!(layout.ops[second_op].prev == Some(first_op));
        assert!(layout.ops[second_op].next == None);

        // E0:
        //     O0
        //     O1

        // Second Block
        let second_block = Block(1);
        layout.insert_block_after(first_block, second_block);
        assert!(layout.blocks[first_block].prev == None);
        assert!(layout.blocks[first_block].next == Some(second_block));
        assert!(layout.blocks[second_block].prev == Some(first_block));
        assert!(layout.blocks[second_block].next == None);

        // E0:
        //     O1
        //     O2
        // E1:

        // Insert Op between the two
        let new_op = Op(2);
        layout.insert_op_after(first_block, Some(first_op), new_op);
        assert!(layout.blocks[first_block].first_op == Some(first_op));
        assert!(layout.blocks[first_block].last_op == Some(second_op));
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
        assert!(layout.blocks[first_block].prev == None);
        assert!(layout.blocks[first_block].next == Some(second_block));
        assert!(layout.blocks[second_block].prev == Some(first_block));
        assert!(layout.blocks[second_block].next == None);

        // E0:
        //     O0
        //     O1
        // E1:

        // Remove second Op
        layout.remove_op(second_op);
        assert!(layout.ops[first_op].next == None);
        assert!(layout.ops[first_op].prev == None);
        assert!(layout.blocks[first_block].first_op == Some(first_op));
        assert!(layout.blocks[first_block].last_op == Some(first_op));

        // E0:
        //     O0
        // E1:

        // Insert Op in second Block
        let second_block_op = Op(3);
        layout.insert_op_after(second_block, None, second_block_op);
        assert!(layout.ops[second_block_op].next == None);
        assert!(layout.ops[second_block_op].prev == None);
        assert!(layout.blocks[second_block].first_op == Some(second_block_op));
        assert!(layout.blocks[second_block].last_op == Some(second_block_op));

        // E0:
        //     O0
        //     O3

        layout.concat_block(first_block, second_block);
        assert!(layout.blocks[first_block].last_op == Some(second_block_op));
        assert!(layout.ops[second_block_op].block == Some(first_block));
        assert!(layout.ops[second_block_op].next == None);
        assert!(layout.ops[second_block_op].prev == Some(first_op));
        assert!(layout.ops[first_op].next == Some(second_block_op));
        assert!(layout.ops[first_op].prev == None);

        // E0:
        //     O3

        layout.remove_op(first_op);
        assert!(layout.ops[second_block_op].prev == None);
        assert!(layout.ops[second_block_op].next == None);

    }

    #[test]
    fn test_concat() {
        let mut layout = Layout::new();
        let e0 = Block(0);
        let e1 = Block(1);
        let o0 = Op(0);
        let o1 = Op(1);
        layout.insert_block_first(e0);
        layout.insert_op_after(e0, None, o0);
        layout.insert_block_after(e0, e1);
        layout.insert_op_after(e1, None, o1);
        layout.concat_block(e0, e1);
    }

    #[test]
    fn test_assertion_fail_trace_1() {
        let mut layout = Layout::new();

        layout.insert_block_first(Block(0));
        layout.insert_block_after(Block(0), Block(1));
        layout.insert_op_after(Block(1), None, Op(0));
        layout.insert_block_after(Block(1), Block(2));
        layout.insert_op_after(Block(2), None, Op(1));
        layout.insert_op_after(Block(0), None, Op(2));
        layout.insert_block_after(Block(0), Block(3));
        layout.insert_block_after(Block(0), Block(4));
        layout.insert_op_after(Block(0), Some(Op(2)), Op(3));
        layout.insert_block_after(Block(0), Block(5));
        layout.insert_op_after(Block(5), None, Op(4));
        layout.insert_block_after(Block(5), Block(6));
        layout.insert_block_after(Block(5), Block(7));
        layout.insert_op_after(Block(3), None, Op(5));
        layout.insert_op_after(Block(6), None, Op(6));
        layout.insert_block_after(Block(6), Block(8));
        layout.insert_op_after(Block(6), Some(Op(6)), Op(7));
        layout.insert_op_after(Block(6), Some(Op(7)), Op(8));
        layout.insert_op_after(Block(6), Some(Op(8)), Op(9));
        layout.insert_op_after(Block(6), Some(Op(9)), Op(10));
        layout.insert_op_after(Block(6), Some(Op(10)), Op(11));
        layout.insert_op_after(Block(6), Some(Op(11)), Op(12));
        layout.insert_op_after(Block(6), Some(Op(12)), Op(13));
        layout.insert_op_after(Block(6), Some(Op(13)), Op(14));
        layout.insert_op_after(Block(6), Some(Op(14)), Op(15));
        layout.insert_op_after(Block(6), Some(Op(15)), Op(16));
        layout.insert_op_after(Block(6), Some(Op(16)), Op(17));
        layout.insert_op_after(Block(8), None, Op(18));
        layout.insert_op_after(Block(7), None, Op(19));
        layout.insert_block_after(Block(7), Block(9));
        layout.insert_op_after(Block(7), Some(Op(19)), Op(20));
        layout.insert_op_after(Block(7), Some(Op(20)), Op(21));
        layout.insert_op_after(Block(7), Some(Op(21)), Op(22));
        layout.insert_op_after(Block(7), Some(Op(22)), Op(23));
        layout.insert_op_after(Block(7), Some(Op(23)), Op(24));
        layout.insert_op_after(Block(7), Some(Op(24)), Op(25));
        layout.insert_op_after(Block(7), Some(Op(25)), Op(26));
        layout.insert_op_after(Block(7), Some(Op(26)), Op(27));
        layout.insert_op_after(Block(7), Some(Op(27)), Op(28));
        layout.insert_op_after(Block(7), Some(Op(28)), Op(29));
        layout.insert_op_after(Block(9), None, Op(30));
        layout.insert_op_after(Block(4), None, Op(31));
        layout.insert_block_after(Block(3), Block(10));
        layout.insert_op_after(Block(10), None, Op(32));
        layout.insert_block_after(Block(10), Block(11));
        layout.insert_block_after(Block(10), Block(12));
        layout.insert_block_after(Block(10), Block(13));
        layout.insert_block_after(Block(10), Block(14));
        layout.insert_block_after(Block(10), Block(15));
        layout.insert_op_after(Block(10), Some(Op(32)), Op(33));
        layout.insert_op_after(Block(14), None, Op(34));
        layout.insert_op_after(Block(15), None, Op(35));
        layout.insert_op_after(Block(11), None, Op(36));
        layout.remove_op(Op(3));
        layout.insert_op_after(Block(0), Some(Op(2)), Op(37));
        layout.insert_op_after(Block(12), None, Op(38));
        layout.remove_op(Op(6));
        layout.insert_op_after(Block(6), None, Op(39));
        layout.insert_op_after(Block(6), Some(Op(39)), Op(40));
        layout.insert_op_after(Block(13), None, Op(41));
        layout.remove_op(Op(19));
        layout.insert_op_after(Block(7), None, Op(42));
        layout.insert_op_after(Block(7), Some(Op(42)), Op(43));
        layout.remove_op(Op(18));
        layout.insert_op_after(Block(8), None, Op(44));
        layout.remove_op(Op(30));
        layout.insert_op_after(Block(9), None, Op(45));
        layout.remove_block(Block(3));
        layout.remove_op(Op(22));
        layout.remove_op(Op(10));
        layout.remove_op(Op(37));
        layout.concat_block(Block(0), Block(10));
        //layout.remove_block(Block(10));
        layout.remove_op(Op(45));
        layout.concat_block(Block(9), Block(15));
        //layout.remove_block(Block(15));
        layout.remove_op(Op(44));
        layout.concat_block(Block(8), Block(14));
        //layout.remove_block(Block(14));
        layout.remove_op(Op(31));
        layout.insert_op_after(Block(4), None, Op(46));
        layout.concat_block(Block(4), Block(2));
        //layout.remove_block(Block(2));
        layout.remove_op(Op(33));
        layout.concat_block(Block(0), Block(12));
        //layout.remove_block(Block(12));
        layout.remove_op(Op(35));
    }


}





