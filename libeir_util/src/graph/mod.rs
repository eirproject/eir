use std::collections::{ HashMap, HashSet };
use std::cell::RefCell;
use std::ops::{ Index, IndexMut };

pub mod dominators;

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct NodeLabel(usize);
impl std::fmt::Display for NodeLabel {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "N{}", self.0)
    }
}

#[derive(Debug, Copy, Clone, Hash, PartialEq, Eq)]
pub struct EdgeLabel(usize);
impl std::fmt::Display for EdgeLabel {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "E{}", self.0)
    }
}

#[derive(Debug, Clone)]
pub struct NodeData<Node> {
    pub label: NodeLabel,
    pub inner: RefCell<Node>,
    pub incoming: Vec<(EdgeLabel, NodeLabel)>,
    pub outgoing: Vec<(EdgeLabel, NodeLabel)>,
}

#[derive(Debug, Clone)]
pub struct EdgeData<Edge> {
    pub label: EdgeLabel,
    pub from: NodeLabel,
    pub to: NodeLabel,
    pub inner: RefCell<Edge>,
}

#[derive(Debug, Clone)]
pub struct Graph<Node, Edge> {
    node_label_counter: usize,
    edge_label_counter: usize,
    nodes: HashMap<NodeLabel, NodeData<Node>>,
    edges: HashMap<EdgeLabel, EdgeData<Edge>>,
}

impl<Node, Edge> Graph<Node, Edge> {

    pub fn new() -> Self {
        Graph {
            node_label_counter: 0,
            edge_label_counter: 0,
            nodes: HashMap::new(),
            edges: HashMap::new(),
        }
    }

    pub fn add_node(&mut self, node: Node) -> NodeLabel {
        let label = NodeLabel(self.node_label_counter);
        self.node_label_counter += 1;
        self.nodes.insert(label, NodeData {
            label: label,
            inner: RefCell::new(node),
            incoming: vec![],
            outgoing: vec![],
        });
        label
    }

    pub fn add_edge(&mut self, from: NodeLabel, to: NodeLabel,
                    edge: Edge) -> EdgeLabel {
        let label = EdgeLabel(self.edge_label_counter);
        self.edge_label_counter += 1;
        self.edges.insert(label, EdgeData {
            label: label,
            inner: RefCell::new(edge),
            from: from,
            to: to,
        });
        self.node_mut(from).outgoing.push((label, to));
        self.node_mut(to).incoming.push((label, from));
        label
    }

    pub fn node<'a>(&'a self, node: NodeLabel) -> &'a NodeData<Node> {
        &self.nodes[&node]
    }
    pub fn node_mut<'a>(&'a mut self, node: NodeLabel) -> &'a mut NodeData<Node> {
        self.nodes.get_mut(&node).unwrap()
    }

    pub fn nodes<'a>(&'a self) -> impl Iterator<Item = &'a NodeData<Node>> {
        self.nodes.values()
    }
    pub fn nodes_mut<'a>(&'a mut self) -> impl Iterator<Item = &'a mut NodeData<Node>> {
        self.nodes.values_mut()
    }

    pub fn node_labels<'a>(&'a self) -> impl Iterator<Item = NodeLabel> + 'a {
        self.nodes.keys().cloned()
    }

    pub fn has_edge<'a>(&'a self, src: NodeLabel, dst: NodeLabel) -> bool {
        self.nodes[&src].outgoing.iter().find(|(_e, d)| *d == dst).is_some()
    }

    pub fn remove_edge(&mut self, lbl: EdgeLabel) {
        let edge = self.edges.remove(&lbl).unwrap();
        {
            let from_node = &mut self.nodes.get_mut(&edge.from).unwrap();
            let o_pos = from_node.outgoing.iter()
                .position(|(edge, _node)| *edge == lbl)
                .unwrap();
            from_node.outgoing.remove(o_pos);
        }
        {
            let to_node = &mut self.nodes.get_mut(&edge.to).unwrap();
            let i_pos = to_node.incoming.iter()
                .position(|(edge, _node)| *edge == lbl)
                .unwrap();
            to_node.incoming.remove(i_pos);
        }
        //self.validate();
    }

    /// Not memory unsafe, but has a large potential of messing up our
    /// graph invariants.
    pub unsafe fn remove_node(&mut self, lbl: NodeLabel) -> Node {
        {
            let (incoming, outgoing) = {
                let node = &self.nodes[&lbl];
                (node.incoming.clone(), node.outgoing.clone())
            };
            for (edge, _node) in incoming.iter() {
                self.remove_edge(*edge);
            }
            for (edge, _node) in outgoing.iter() {
                self.remove_edge(*edge);
            }
        }
        let node = self.nodes.remove(&lbl).unwrap().inner.into_inner();
        //self.validate();
        node
    }

    pub fn edge_from(&self, edge: EdgeLabel) -> NodeLabel {
        self.edges[&edge].from
    }
    pub fn edge_to(&self, edge: EdgeLabel) -> NodeLabel {
        self.edges[&edge].to
    }

    pub fn reparent_edge(&mut self, edge: EdgeLabel, new_parent: NodeLabel) {
        let (from, to) = {
            let edge = self.edges.get_mut(&edge).unwrap();
            let from = edge.from;
            let to = edge.to;
            edge.from = new_parent;
            (from, to)
        };
        {
            let from_node = self.nodes.get_mut(&from).unwrap();
            let pos = from_node.outgoing.iter()
                .position(|(r_edge, _node)| *r_edge == edge).unwrap();
            from_node.outgoing.remove(pos);
        }
        {
            let new_from_node = self.nodes.get_mut(&new_parent).unwrap();
            new_from_node.outgoing.push((edge, to));
        }
        {
            let to_node = self.nodes.get_mut(&to).unwrap();
            let pos = to_node.incoming.iter()
                .position(|(r_edge, _node)| *r_edge == edge).unwrap();
            to_node.incoming[pos].1 = new_parent;
        }
        //self.validate();
    }

    pub fn rechild_edge(&mut self, edge: EdgeLabel, new_child: NodeLabel) {
        let (from, to) = {
            let edge = self.edges.get_mut(&edge).unwrap();
            let from = edge.from;
            let to = edge.to;
            edge.to = new_child;
            (from, to)
        };
        {
            let from_node = self.nodes.get_mut(&from).unwrap();
            let pos = from_node.outgoing.iter()
                .position(|(r_edge, _node)| *r_edge == edge).unwrap();
            from_node.outgoing[pos].1 = new_child;
        }
        {
            let new_to_node = self.nodes.get_mut(&new_child).unwrap();
            new_to_node.incoming.push((edge, from));
        }
        {
            let to_node = self.nodes.get_mut(&to).unwrap();
            let pos = to_node.incoming.iter()
                .position(|(r_edge, _node)| *r_edge == edge).unwrap();
            to_node.incoming.remove(pos);
        }
        //self.validate();
    }

    /// Validates that the graph itself is internally consistent.
    pub fn validate(&self) {
        let valid_nodes: HashSet<_> = self.nodes.keys().collect();
        let valid_edges: HashSet<_> = self.edges.keys().collect();
        let mut visited_in: HashSet<_> = HashSet::new();
        let mut visited_out: HashSet<_> = HashSet::new();
        for node in self.nodes.values() {
            for (i_edge, i_node) in node.incoming.iter() {
                assert!(valid_edges.contains(i_edge));
                assert!(valid_nodes.contains(i_node));
                assert!(self.edges[i_edge].to == node.label);
                assert!(self.edges[i_edge].from == *i_node);
                assert!(!visited_in.contains(i_edge));
                visited_in.insert(*i_edge);
            }
            for (o_edge, o_node) in node.outgoing.iter() {
                assert!(valid_edges.contains(o_edge));
                assert!(valid_nodes.contains(o_node));
                assert!(self.edges[o_edge].from == node.label);
                assert!(self.edges[o_edge].to == *o_node);
                assert!(!visited_out.contains(o_edge));
                visited_out.insert(*o_edge);
            }
        }
        for edge in self.edges.values() {
            assert!(valid_nodes.contains(&edge.from));
            assert!(valid_nodes.contains(&edge.to));
            assert!(visited_in.contains(&edge.label));
            assert!(visited_out.contains(&edge.label));
        }
    }

}

impl<Node, Edge> Index<NodeLabel> for Graph<Node, Edge> {
    type Output = NodeData<Node>;
    fn index(&self, label: NodeLabel) -> &NodeData<Node> {
        &self.nodes[&label]
    }
}
impl<Node, Edge> Index<EdgeLabel> for Graph<Node, Edge> {
    type Output = EdgeData<Edge>;
    fn index(&self, label: EdgeLabel) -> &EdgeData<Edge> {
        &self.edges[&label]
    }
}
impl<Node, Edge> IndexMut<NodeLabel> for Graph<Node, Edge> {
    fn index_mut(&mut self, label: NodeLabel) -> &mut NodeData<Node> {
        self.nodes.get_mut(&label).unwrap()
    }
}
impl<Node, Edge> IndexMut<EdgeLabel> for Graph<Node, Edge> {
    fn index_mut(&mut self, label: EdgeLabel) -> &mut EdgeData<Edge> {
        self.edges.get_mut(&label).unwrap()
    }
}
