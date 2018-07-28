use ::petgraph::{ Graph, Direction };
use ::petgraph::graph::NodeIndex;

use ::ir::SSAVariable;
use ::ir::hir::Expression;
use ::ir::hir::Clause;

use ::ir::hir::PatternNode;
use ::parser::AtomicLiteral;

use ::ir::hir::pass::ssa::ScopeTracker;

use ::pattern_compiler::{ PatternProvider, ExpandedClauseNodes };

#[derive(Copy, Clone, Hash, PartialEq, Eq)]
struct CfgVar(usize);

/**
 * value_inputs: A set of constants provided as SSAVariables
 */
pub fn compile(roots: &[SSAVariable], clauses: &[Clause], value_inputs: &[SSAVariable], 
               env: &mut ScopeTracker) {

    println!("==================================");
    println!("Compile pattern:");
    println!("roots: {:?}", roots);
    //println!("clauses: {:#?}", clauses);

    for clause in clauses {
        println!("clause: {:#?}", clause.patterns);
        //let a: i32 = clause.patterns[0].node;
    }

    let mut pat = ErlPatternProvider::new(env, roots.iter().map(|i| *i).collect());

    for clause in clauses {
        for pattern in &clause.patterns {
            let node = pattern_node_to_node(&mut pat, &pattern.node);
            pat.add_root_clause(node);
        }
    }

    let res = ::pattern_compiler::to_decision_tree(&mut pat);

}

fn pattern_node_to_node(pat: &mut ErlPatternProvider, node: &PatternNode)
                        -> NodeIndex {
    match *node {
        PatternNode::Atomic(AtomicLiteral::Nil) => pat.add_node(NodeKind::Nil),
        PatternNode::Variable(_) => pat.add_node(NodeKind::Wildcard), // TODO
        PatternNode::Map(ref entries) => {
            let node = pat.add_node(NodeKind::Map);
            node
        },
        ref item => unimplemented!("{:?}", item),
    }
}

fn add_pattern_node(pat: &mut ErlPatternProvider, parent: NodeIndex,
                    node: &PatternNode) {
    println!("{:?}", node);
    let node_idx = pattern_node_to_node(pat, node);
    pat.add_edge(parent, node_idx);
}


#[derive(Debug, Copy, Clone, Hash, Eq, PartialEq)]
enum NodeKind {
    Tuple,
    List,
    Map,
    Nil,
    RootValues,
    Wildcard,
}

#[derive(Debug)]
struct ErlPatternProvider<'a> {
    pattern: Graph<NodeKind, ()>,
    root_clauses: Vec<NodeIndex>,
    root_vars: Vec<SSAVariable>,
    env: &'a mut ScopeTracker,
}

impl<'a> ErlPatternProvider<'a> {

    fn new(env: &'a mut ScopeTracker, root_vars: Vec<SSAVariable>)
           -> ErlPatternProvider<'a> {

        ErlPatternProvider {
            pattern: Graph::new(),
            root_clauses: Vec::new(),
            root_vars: root_vars,
            env: env,
        }
    }

    fn add_node(&mut self, kind: NodeKind) -> NodeIndex {
        self.pattern.add_node(kind)
    }

    fn add_edge(&mut self, parent: NodeIndex, child: NodeIndex) {
        self.pattern.add_edge(parent, child, ());
    }

    fn add_root_clause(&mut self, node: NodeIndex) {
        self.root_clauses.push(node);
    }

    fn add_clause(&mut self, kind: NodeKind) -> NodeIndex {
        let res = self.pattern.add_node(kind);
        self.root_clauses.push(res);
        res
    }

    fn add_child(&mut self, node: NodeIndex, kind: NodeKind) -> NodeIndex {
        let res = self.pattern.add_node(kind);
        self.pattern.add_edge(node, res, ());
        res
    }

}

impl<'a> PatternProvider for ErlPatternProvider<'a> {

    type PatternNodeKey = NodeIndex;
    type PatternNodeKind = NodeKind;
    type CfgVariable = SSAVariable;

    const WILDCARD: NodeKind = NodeKind::Wildcard;

    fn get_root(&self) -> ExpandedClauseNodes<
            Self::CfgVariable, Self::PatternNodeKey> {
        ExpandedClauseNodes {
            variables: self.root_vars.clone(),
            clauses: self.root_vars.len(),
            nodes: self.root_clauses.clone(),
        }
    }

    fn kind_includes(&self, kind: Self::PatternNodeKind,
                     key: Self::PatternNodeKey) -> bool {
        unimplemented!();
    }

    fn expand_clause_nodes(&mut self, clause_nodes: Vec<Self::PatternNodeKey>)
                           -> ExpandedClauseNodes<Self::CfgVariable,
                                                  Self::PatternNodeKey> {
        unimplemented!();
    }

    fn get_kind(&self, key: Self::PatternNodeKey) -> Self::PatternNodeKind {
        unimplemented!();
    }

}
