use ::{ PatternProvider, ExpandedClauseNodes };

use ::petgraph::{ Graph, Direction };
use ::petgraph::graph::NodeIndex;

use super::{ SimplePatternProvider, NodeKind };

#[test]
fn list_merge_pattern() {

    // fn ([], _)
    // fn (_, [])
    // fn ([_, _], [_, _])

    let mut pattern = SimplePatternProvider::new();

    {
        let clause = pattern.add_clause(NodeKind::RootValues);
        pattern.add_child(clause, NodeKind::Terminal);
        pattern.add_child(clause, NodeKind::Wildcard);
    }
    {
        let clause = pattern.add_clause(NodeKind::RootValues);
        pattern.add_child(clause, NodeKind::Wildcard);
        pattern.add_child(clause, NodeKind::Terminal);
    }
    {
        let clause = pattern.add_clause(NodeKind::RootValues);

        let list_cell_1 = pattern.add_child(clause, NodeKind::ListCell);
        pattern.add_child(list_cell_1, NodeKind::Wildcard);
        pattern.add_child(list_cell_1, NodeKind::Wildcard);

        let list_cell_2 = pattern.add_child(clause, NodeKind::ListCell);
        pattern.add_child(list_cell_2, NodeKind::Wildcard);
        pattern.add_child(list_cell_2, NodeKind::Wildcard);
    }

    let res = ::to_decision_tree(&mut pattern);

    let mut file = ::std::fs::File::create("cfg.dot").unwrap();
    res.to_dot(&mut file).unwrap();

    println!("{:?}", res);
    println!("{:#?}", res.leaf_bindings);

}
