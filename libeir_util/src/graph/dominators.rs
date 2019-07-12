use super::*;

use std::collections::{ HashMap, HashSet };

impl<Node, Edge> Graph<Node, Edge> {

    pub fn dfs_walk(&self, node: NodeLabel, visited: &mut HashSet<NodeLabel>,
                order: &mut Vec<NodeLabel>) {
        visited.insert(node);
        for (_edge, succ) in self.node(node).outgoing.iter() {
            if !visited.contains(succ) {
                self.dfs_walk(*succ, visited, order);
            }
        }
        order.push(node);
    }

    pub fn post_order(&self, root: NodeLabel) -> Vec<NodeLabel> {
        let mut visited = HashSet::new();
        let mut order = Vec::new();
        self.dfs_walk(root, &mut visited, &mut order);
        order
    }

    pub fn reverse_post_order(&self, root: NodeLabel) -> Vec<NodeLabel> {
        let mut order = self.post_order(root);
        order.reverse();
        order
    }

    // for all nodes, b /* initialize the dominators array */
    //   doms[b] ← Undefined
    // doms[start node] ← start node
    // Changed ← true
    //
    // while (Changed)
    //   Changed ← false
    //   for all nodes, b, in reverse postorder (except start node)
    //     new idom ← first (processed) predecessor of b /* (pick one) */
    //     for all other predecessors, p, of b
    //       if doms[p] 6 = Undefined /* i.e., if doms[p] already calculated */
    //         new idom ← intersect(p, new idom)
    //     if doms[b] 6 = new idom
    //       doms[b] ← new idom
    //       Changed ← true
    //
    // function intersect(b1, b2) returns node
    //   finger1 ← b1
    //   finger2 ← b2
    //   while (finger1 6 = finger2)
    //     while (finger1 < finger2)
    //       finger1 = doms[finger1]
    //     while (finger2 < finger1)
    //       finger2 = doms[finger2]
    //   return finger1

    fn intersect(&self, doms: &HashMap<usize, usize>, b1: usize, b2: usize) -> usize {
        let mut finger1 = b1;
        let mut finger2 = b2;
        while finger1 != finger2 {
            while finger1 < finger2 {
                finger1 = doms[&finger1];
            }
            while finger2 < finger1 {
                finger2 = doms[&finger2];
            }
        }
        return finger1;
    }

    /// Dominators implementation specialized for our purposes, specifically,
    /// unreachable nodes from the entry node are just ignored.
    pub fn dominators(&self, entry: NodeLabel) -> HashMap<NodeLabel, NodeLabel> {
        // Implements https://www.cs.rice.edu/~keith/EMBED/dom.pdf

        let order = self.post_order(entry);
        let length = order.len();
        assert!(*order.last().unwrap() == entry);

        let mut doms: HashMap<usize, usize> = HashMap::new();
        doms.insert(length-1, length-1);

        let node_to_order_idx: HashMap<_, _> = {
            order.iter().enumerate()
                .map(|(idx, val)| (val, idx))
                .collect()
        };

        let mut changed = true;

        while changed {
            changed = false;
            for order_num in (0..(order.len()-1)).rev() {
                let node_label = order[order_num];
                let node = self.node(node_label);
                let new_idom = {
                    let mut preds = node.incoming.iter()
                        .filter_map(|n| node_to_order_idx.get(&n.1).map(|v| *v))
                        .filter(|n| doms.contains_key(n));
                    let mut new_idom = preds.next().unwrap();
                    for pred_idx in preds {
                        if doms.contains_key(&pred_idx) {
                            new_idom = self.intersect(&doms, pred_idx, new_idom);
                        }
                    }
                    new_idom
                };
                if !doms.contains_key(&order_num) || new_idom != doms[&order_num] {
                    doms.insert(order_num, new_idom);
                    changed = true;
                }
            }
        }

        doms.iter().map(|(k, v)| (order[*k], order[*v])).collect()
    }

}
