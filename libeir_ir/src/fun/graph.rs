//use super::{ Ebb, Op, EbbCall };
//
//use std::collections::HashMap;
//
//use petgraph::graph::{ Graph, NodeIndex };
//
//#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
//pub enum CfgNode {
//    Ebb(Ebb),
//    Op(Op),
//}
//
//#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
//pub enum CfgEdge {
//    Flow,
//    Call(EbbCall),
//}
//
//#[derive(Debug, Clone)]
//pub struct FunctionCfg {
//    pub graph: Graph<CfgNode, CfgEdge>,
//    pub ops: HashMap<Op, NodeIndex>,
//    pub ebbs: HashMap<Ebb, NodeIndex>,
//}

use petgraph::Direction;
use petgraph::visit::{ GraphBase, IntoNeighbors, IntoNeighborsDirected,
                       Visitable, VisitMap };
use petgraph::visit::{ Dfs, DfsPostOrder };

use cranelift_entity::{ EntityRef, EntitySet };

use itertools::Either;

use libeir_util::pooled_entity_set::PooledEntitySetIter;

use super::Function;
use super::{ Block };

/// This is a newtype that contains implementations of petgraphs graph traits.
pub struct BlockGraph<'a> {
    fun: &'a Function,
}
impl<'a> BlockGraph<'a> {

    pub fn new(fun: &'a Function) -> Self {
        BlockGraph {
            fun: fun,
        }
    }

    pub fn dfs(&self) -> Dfs<Block, EntityVisitMap<Block>> {
        Dfs::new(self, self.fun.block_entry())
    }

    pub fn dfs_iter(&'a self) -> impl Iterator<Item = Block> + 'a {
        struct BlocksIterator<'b>(&'b BlockGraph<'b>, Dfs<Block, EntityVisitMap<Block>>);
        impl<'b> Iterator for BlocksIterator<'b> {
            type Item = Block;
            fn next(&mut self) -> Option<Block> {
                self.1.next(self.0)
            }
        }
        BlocksIterator(self, self.dfs())
    }

    pub fn dfs_post_order(&self) -> DfsPostOrder<Block, EntityVisitMap<Block>> {
        DfsPostOrder::new(self, self.fun.block_entry())
    }

    pub fn outgoing(&'a self, block: Block) -> impl Iterator<Item = Block> + 'a {
        self.fun.blocks[block].successors.iter(&self.fun.block_set_pool)
    }

}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct BlockEdge(Block, usize);

pub struct BlockSuccessors<'a> {
    fun: &'a Function,
    block: Block,
    pos: usize,
}
impl<'a> Iterator for BlockSuccessors<'a> {
    type Item = Block;
    fn next(&mut self) -> Option<Block> {
        loop {
            if let Some(val) = self.fun.blocks[self.block].reads.get(
                self.pos, &self.fun.value_pool)
            {
                self.pos += 1;
                if let Some(block) = self.fun.value_block(val) {
                    return Some(block);
                }
            } else {
                return None;
            }
        }
    }
}

pub struct BlockPredecessors<'a> {
    iter: PooledEntitySetIter<'a, Block>,
}
impl<'a> Iterator for BlockPredecessors<'a> {
    type Item = Block;
    fn next(&mut self) -> Option<Block> {
        self.iter.next()
    }
}

impl<'a> GraphBase for BlockGraph<'a> {
    type NodeId = Block;
    type EdgeId = BlockEdge;
}
impl<'a> IntoNeighbors for &'a BlockGraph<'a> {
    type Neighbors = BlockSuccessors<'a>;
    fn neighbors(self, block: Block) -> Self::Neighbors {
        BlockSuccessors {
            fun: &self.fun,
            block,
            pos: 0,
        }
    }
}
impl<'a> IntoNeighborsDirected for &'a BlockGraph<'a> {
    type NeighborsDirected = Either<BlockSuccessors<'a>, BlockPredecessors<'a>>;
    fn neighbors_directed(self, block: Block, dir: Direction) -> Self::NeighborsDirected {
        match dir {
            Direction::Outgoing => {
                Either::Left(BlockSuccessors {
                    fun: &self.fun,
                    block,
                    pos: 0,
                })
            }
            Direction::Incoming => {
                Either::Right(BlockPredecessors {
                    iter: self.fun.blocks[block].predecessors.iter(&self.fun.block_set_pool)
                })
            }
        }
    }
}

pub struct EntityVisitMap<E> where E: EntityRef {
    set: EntitySet<E>,
}
impl<E> VisitMap<E> for EntityVisitMap<E> where E: EntityRef {
    fn visit(&mut self, a: E) -> bool {
        self.set.insert(a)
    }
    fn is_visited(&self, a: &E) -> bool {
        self.set.contains(*a)
    }
}

impl<'a> Visitable for &'a BlockGraph<'a> {
    type Map = EntityVisitMap<Block>;
    fn visit_map(&self) -> EntityVisitMap<Block> {
        let mut set = EntitySet::new();
        set.resize(self.fun.blocks.len());
        EntityVisitMap {
            set,
        }
    }
    fn reset_map(&self, map: &mut EntityVisitMap<Block>) {
        map.set.clear();
        map.set.resize(self.fun.blocks.len());
    }
}
