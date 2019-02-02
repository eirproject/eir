use std::collections::HashMap;
use std::cell::RefCell;
use std::ops::{ Index, IndexMut };

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

#[derive(Debug)]
pub struct NodeData<Node> {
    pub label: NodeLabel,
    pub inner: RefCell<Node>,
    pub incoming: Vec<(EdgeLabel, NodeLabel)>,
    pub outgoing: Vec<(EdgeLabel, NodeLabel)>,
}

#[derive(Debug)]
pub struct EdgeData<Edge> {
    pub label: EdgeLabel,
    pub from: NodeLabel,
    pub to: NodeLabel,
    pub inner: RefCell<Edge>,
}

#[derive(Debug)]
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
    }

    pub fn remove_node(&mut self, lbl: NodeLabel) -> Node {
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
        self.nodes.remove(&lbl).unwrap().inner.into_inner()
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
