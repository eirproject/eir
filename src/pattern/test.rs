use super::pattern::{ Pattern, PatternNodeO, PatternNodeKind };

#[test]
fn list_merge_pattern() {
    let mut pattern = Pattern::new(2);

    {
        let sentinel = pattern.add_node(PatternNodeKind::Sentinel);
        let wildcard = pattern.add_node(PatternNodeKind::Wildcard);
        pattern.add_clause(vec![sentinel, wildcard]);
    }
    {
        let wildcard = pattern.add_node(PatternNodeKind::Wildcard);
        let sentinel = pattern.add_node(PatternNodeKind::Sentinel);
        pattern.add_clause(vec![wildcard, sentinel]);
    }
    {
        let list_cell_1 = pattern.add_node(PatternNodeKind::ListCell);
        pattern.add_child(list_cell_1, PatternNodeKind::Wildcard);
        pattern.add_child(list_cell_1, PatternNodeKind::Wildcard);

        let list_cell_2 = pattern.add_node(PatternNodeKind::ListCell);
        pattern.add_child(list_cell_2, PatternNodeKind::Wildcard);
        pattern.add_child(list_cell_2, PatternNodeKind::Wildcard);

        pattern.add_clause(vec![list_cell_1, list_cell_2]);
    }

    super::to_decision_tree(&pattern);

}

#[test]
fn advanced_pattern() {
    let mut pattern = Pattern::new(2);
    let wildcard = pattern.add_node(PatternNodeKind::Wildcard);

    let list_cell_1 = pattern.add_node(PatternNodeKind::ListCell);
    pattern.add_child(list_cell_1, PatternNodeKind::Wildcard);

    let list_cell_2 = pattern.add_child(list_cell_1, PatternNodeKind::ListCell);
    pattern.add_child(list_cell_2, PatternNodeKind::Wildcard);
    pattern.add_child(list_cell_2, PatternNodeKind::Sentinel);

    pattern.add_clause(vec![wildcard, list_cell_1]);

    //super::to_decision_tree(&pattern);
}
