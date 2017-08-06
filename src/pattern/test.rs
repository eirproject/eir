use super::pattern::{ Pattern, PatternNode };

#[test]
fn list_merge_pattern() {
    let mut pattern = Pattern::new(2);

    let sentinel = pattern.add_node(PatternNode::Sentinel);
    let wildcard = pattern.add_node(PatternNode::Wildcard);

    let list_cell = pattern.add_node(PatternNode::ListCell);
    pattern.add_child(list_cell, PatternNode::Wildcard);
    pattern.add_child(list_cell, PatternNode::Wildcard);

    pattern.add_clause(vec![sentinel, wildcard]);
    pattern.add_clause(vec![wildcard, sentinel]);
    pattern.add_clause(vec![list_cell, list_cell]);

    //super::to_decision_tree(&pattern);

}

#[test]
fn advanced_pattern() {
    let mut pattern = Pattern::new(2);
    let wildcard = pattern.add_node(PatternNode::Wildcard);

    let list_cell_1 = pattern.add_node(PatternNode::ListCell);
    pattern.add_child(list_cell_1, PatternNode::Wildcard);

    let list_cell_2 = pattern.add_child(list_cell_1, PatternNode::ListCell);
    pattern.add_child(list_cell_2, PatternNode::Wildcard);
    pattern.add_child(list_cell_2, PatternNode::Wildcard);

    pattern.add_clause(vec![wildcard, list_cell_1]);

    super::to_decision_tree(&pattern);
}
