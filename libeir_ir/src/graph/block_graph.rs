use petgraph::Direction;
use petgraph::visit::{ GraphBase, IntoNeighbors, IntoNeighborsDirected,
                       Visitable, VisitMap, Walker };
use petgraph::visit::{ Dfs, DfsPostOrder };

use cranelift_entity::{ EntityRef, EntitySet };

use itertools::Either;

use libeir_util_datastructures::pooled_entity_set::EntitySetIter;

use crate::Function;
use crate::{ Block };

impl Function {

    pub fn block_graph(&self) -> BlockGraph<'_> {
        BlockGraph::new(self)
    }

}

/// This is a newtype that contains implementations of petgraphs graph traits.
///
/// The semantics of the below graph are as follows:
/// - Nodes are blocks
/// - Block capture values in blocks are edges
/// - Back edges exist to non-live blocks
///
/// The last point may cause some graph algorithms to produce undesirable results.
/// `LiveBlockGraph` does not have this feature, but is slightly more expensive to
/// construct.
pub struct BlockGraph<'a> {
    pub(crate) fun: &'a Function,
}

impl<'a> BlockGraph<'a> {

    pub fn new(fun: &'a Function) -> Self {
        BlockGraph {
            fun,
        }
    }

    pub fn dfs(&self) -> Dfs<Block, EntityVisitMap<Block>> {
        Dfs::new(self, self.fun.block_entry())
    }

    pub fn dfs_iter(&'a self) -> impl Iterator<Item = Block> + 'a {
        self.dfs().iter(self)
    }

    pub fn dfs_post_order(&self) -> DfsPostOrder<Block, EntityVisitMap<Block>> {
        DfsPostOrder::new(self, self.fun.block_entry())
    }

    pub fn dfs_post_order_iter(&'a self) -> impl Iterator<Item = Block> + 'a {
        self.dfs_post_order().iter(self)
    }

    pub fn outgoing(&'a self, block: Block) -> impl Iterator<Item = Block> + 'a {
        self.fun.blocks[block].successors.iter(&self.fun.pool.block_set)
    }

}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct BlockEdge(Block, usize);

pub struct BlockSuccessors<'a> {
    iter: EntitySetIter<'a, Block>,
}
impl<'a> Iterator for BlockSuccessors<'a> {
    type Item = Block;
    fn next(&mut self) -> Option<Block> {
        self.iter.next()
    }
}

pub struct BlockPredecessors<'a> {
    iter: EntitySetIter<'a, Block>,
}
impl<'a> BlockPredecessors<'a> {
    fn new(graph: &'a BlockGraph, block: Block) -> Self {
        BlockPredecessors {
            iter: graph.fun.blocks[block].predecessors
                .iter(&graph.fun.pool.block_set),
        }
    }
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
            iter: self.fun.blocks[block].successors
                .iter(&self.fun.pool.block_set),
        }
    }
}
impl<'a> IntoNeighborsDirected for &'a BlockGraph<'a> {
    type NeighborsDirected = Either<BlockSuccessors<'a>, BlockPredecessors<'a>>;
    fn neighbors_directed(self, block: Block, dir: Direction) -> Self::NeighborsDirected {
        match dir {
            Direction::Outgoing => Either::Left(self.neighbors(block)),
            Direction::Incoming => Either::Right(BlockPredecessors::new(self, block)),
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

impl<'a> Visitable for BlockGraph<'a> {
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

#[cfg(test)]
mod tests {

    use crate::{ FunctionIdent, Function, FunctionBuilder };
    use libeir_intern::Ident;
    use libeir_diagnostics::DUMMY_SPAN;

    use petgraph::Direction;
    use petgraph::visit::IntoNeighborsDirected;

    #[test]
    fn test_edge() {
        let ident = FunctionIdent {
            module: Ident::from_str("woo"),
            name: Ident::from_str("woo"),
            arity: 1,
        };
        let mut fun = Function::new(DUMMY_SPAN, ident);
        let mut b = FunctionBuilder::new(&mut fun);

        let b1 = b.block_insert();
        b.block_set_entry(b1);
        let b1_ret = b.block_arg_insert(b1);

        let b2 = b.block_insert();

        let b3 = b.block_insert();

        b.op_call_flow(b1, b2, &[]);
        b.op_call_flow(b2, b1_ret, &[]);
        b.op_call_flow(b3, b2, &[]);

        let graph = b.fun().block_graph();

        assert!(&graph.neighbors_directed(b1, Direction::Outgoing).collect::<Vec<_>>() == &[b2]);
        assert!(&graph.neighbors_directed(b2, Direction::Outgoing).collect::<Vec<_>>() == &[]);
        assert!(&graph.neighbors_directed(b3, Direction::Outgoing).collect::<Vec<_>>() == &[b2]);
        assert!(&graph.neighbors_directed(b1, Direction::Incoming).collect::<Vec<_>>() == &[]);
        assert!(&graph.neighbors_directed(b2, Direction::Incoming).collect::<Vec<_>>() == &[b1, b3]);
        assert!(&graph.neighbors_directed(b3, Direction::Incoming).collect::<Vec<_>>() == &[]);

    }

}
