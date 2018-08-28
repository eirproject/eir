use ::petgraph::{ Graph, Direction };
use ::petgraph::graph::NodeIndex;
use ::petgraph::algo::dominators::simple_fast;

use std::collections::{ HashMap, HashSet };

pub fn dominance_frontiers<NodeTyp, EdgeTyp>(
    graph: &Graph<NodeTyp, EdgeTyp>, root: NodeIndex<u32>)
    -> HashMap<NodeIndex<u32>, Vec<NodeIndex<u32>>>
{
    let dominators = simple_fast(graph, root);

    let mut frontiers = HashMap::new();
    for node_idx in graph.node_indices() {
        frontiers.insert(node_idx, vec![]);
    }

    for u in graph.node_indices() {
        let incoming: Vec<NodeIndex<u32>> =
            graph.neighbors_directed(u, Direction::Incoming).collect();
        let loops_self = incoming.contains(&u);

        if incoming.len() - if loops_self { 1 } else { 0 } >= 2 {
            let mut p = HashSet::new();
            for v_r in incoming.iter() {
                let mut v = *v_r;
                while v != dominators.immediate_dominator(u).unwrap() && !p.contains(&v) {
                    p.insert(v);
                    v = dominators.immediate_dominator(v).unwrap();
                }
            }
            p.remove(&u);
            for v in p.iter() {
                frontiers.get_mut(v).unwrap().push(u);
            }
        }
    }

    frontiers
}

#[cfg(test)]
mod test {
    use std::collections::HashMap;
    use ::petgraph::Graph;
    use super::dominance_frontiers;

    #[test]
    fn simple() {
        let mut graph: Graph<(), ()> = Graph::new();

        //   r
        //  / \__
        //  |    |
        //  |    a
        //  |   / \
        //  |  b1  b2
        //   \ |  /
        //    \| /
        //     \/
        //      c

        let r = graph.add_node(());
        let a = graph.add_node(());
        let b1 = graph.add_node(());
        let b2 = graph.add_node(());
        let c = graph.add_node(());

        graph.extend_with_edges(&[
            (r, a),
            (a, b1),
            (a, b2),
            (b1, c),
            (b2, c),
            (r, c),
        ]);

        let df = dominance_frontiers(&graph, r);

        let mut expected_df = HashMap::new();
        expected_df.insert(r, vec![]);
        expected_df.insert(a, vec![c]);
        expected_df.insert(b1, vec![c]);
        expected_df.insert(b2, vec![c]);
        expected_df.insert(c, vec![]);

        assert!(df == expected_df);
    }

}
