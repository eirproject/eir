use std::collections::HashSet;

use petgraph::Direction;
use petgraph::visit::{ GraphBase, IntoNeighbors, IntoNeighborsDirected, Visitable };
use petgraph::visit::{ Dfs, DfsPostOrder };

use itertools::Either;

use libeir_util_datastructures::pooled_entity_set::EntitySetIter;

use crate::Function;
use crate::{ Block };

use super::BlockGraph;
use super::block_graph::{ BlockEdge, BlockSuccessors };
use super::block_graph::EntityVisitMap;

impl Function {

    pub fn live_block_graph(&self) -> LiveBlockGraph<'_> {
        LiveBlockGraph::new(self)
    }

}

/// This is a newtype that contains implementations of petgraphs graph traits.
///
/// This has identical semantics to `BlockGraph`, with the following difference:
/// - Back edges do not exist to non-live blocks
///
/// If back edges to non-live blocks are acceptable, it is recommended to use
/// `BlockGraph` instead.
pub struct LiveBlockGraph<'a> {
    pub graph: BlockGraph<'a>,
    pub live: HashSet<Block>,
}

impl<'a> LiveBlockGraph<'a> {

    pub fn new(fun: &'a Function) -> Self {
        let graph = fun.block_graph();

        let mut live = HashSet::new();
        for block in graph.dfs_iter() {
            live.insert(block);
        }

        LiveBlockGraph {
            graph,
            live,
        }
    }

    pub fn dfs(&self) -> Dfs<Block, EntityVisitMap<Block>> {
        self.graph.dfs()
    }
    pub fn dfs_iter(&'a self) -> impl Iterator<Item = Block> + 'a {
        self.graph.dfs_iter()
    }

    pub fn dfs_post_order(&self) -> DfsPostOrder<Block, EntityVisitMap<Block>> {
        self.graph.dfs_post_order()
    }
    pub fn dfs_post_order_iter(&'a self) -> impl Iterator<Item = Block> + 'a {
        self.graph.dfs_post_order_iter()
    }

    pub fn outgoing(&'a self, block: Block) -> impl Iterator<Item = Block> + 'a {
        self.graph.outgoing(block)
    }

    pub fn incoming(&'a self, block: Block) -> impl Iterator<Item = Block> + 'a {
        self.graph.fun.blocks[block].predecessors
            .iter(&self.graph.fun.pool.block_set)
            .filter(move |b| self.live.contains(b))
    }

}

pub struct LiveBlockPredecessors<'a> {
    graph: &'a LiveBlockGraph<'a>,
    iter: EntitySetIter<'a, Block>,
}
impl<'a> LiveBlockPredecessors<'a> {
    fn new(graph: &'a LiveBlockGraph, block: Block) -> Self {
        LiveBlockPredecessors {
            graph,
            iter: graph.graph.fun.blocks[block].predecessors
                .iter(&graph.graph.fun.pool.block_set),
        }
    }
}
impl<'a> Iterator for LiveBlockPredecessors<'a> {
    type Item = Block;
    fn next(&mut self) -> Option<Block> {
        while let Some(block) = self.iter.next() {
            if self.graph.live.contains(&block) {
                return Some(block);
            }
        }
        None
    }
}

impl<'a> GraphBase for LiveBlockGraph<'a> {
    type NodeId = Block;
    type EdgeId = BlockEdge;
}

impl<'a> IntoNeighbors for &'a LiveBlockGraph<'a> {
    type Neighbors = BlockSuccessors<'a>;
    fn neighbors(self, block: Block) -> Self::Neighbors {
        self.graph.neighbors(block)
    }
}

impl<'a> IntoNeighborsDirected for &'a LiveBlockGraph<'a> {
    type NeighborsDirected = Either<BlockSuccessors<'a>, LiveBlockPredecessors<'a>>;
    fn neighbors_directed(self, block: Block, dir: Direction) -> Self::NeighborsDirected {
        match dir {
            Direction::Outgoing => Either::Left(self.graph.neighbors(block)),
            Direction::Incoming => Either::Right(LiveBlockPredecessors::new(self, block)),
        }
    }
}

impl<'a> Visitable for &'a LiveBlockGraph<'a> {
    type Map = EntityVisitMap<Block>;
    fn visit_map(&self) -> EntityVisitMap<Block> {
        Visitable::visit_map(&self.graph)
    }
    fn reset_map(&self, map: &mut EntityVisitMap<Block>) {
        Visitable::reset_map(&self.graph, map)
    }
}

#[cfg(test)]
mod tests {

    use crate::{ FunctionIdent, Function, FunctionBuilder };
    use libeir_intern::Ident;

    use petgraph::Direction;
    use petgraph::visit::IntoNeighborsDirected;

    #[test]
    fn test_back_edge() {
        let ident = FunctionIdent {
            module: Ident::from_str("woo"),
            name: Ident::from_str("woo"),
            arity: 1,
        };
        let mut fun = Function::new(ident);
        let mut b = FunctionBuilder::new(&mut fun);

        let b1 = b.block_insert();
        b.block_set_entry(b1);
        let b1_ret = b.block_arg_insert(b1);

        let b2 = b.block_insert();

        let b3 = b.block_insert();

        b.op_call_flow(b1, b2, &[]);
        b.op_call_flow(b2, b1_ret, &[]);
        b.op_call_flow(b3, b2, &[]);

        let graph = b.fun().live_block_graph();

        assert!(&graph.neighbors_directed(b1, Direction::Outgoing).collect::<Vec<_>>() == &[b2]);
        assert!(&graph.neighbors_directed(b2, Direction::Outgoing).collect::<Vec<_>>() == &[]);
        assert!(&graph.neighbors_directed(b3, Direction::Outgoing).collect::<Vec<_>>() == &[b2]);
        assert!(&graph.neighbors_directed(b1, Direction::Incoming).collect::<Vec<_>>() == &[]);
        assert!(&graph.neighbors_directed(b2, Direction::Incoming).collect::<Vec<_>>() == &[b1]);
        assert!(&graph.neighbors_directed(b3, Direction::Incoming).collect::<Vec<_>>() == &[]);

    }

}






