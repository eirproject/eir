use ::ir::lir;
//use ::pattern::pattern::{ Pattern, PatternNodeIndex, PatternNodeKind };
use ::pattern::cfg::PatternCfg;

pub fn compile_pattern(lir: &mut ::ir::lir::cfg::FunctionCfg) {
    for node_idx in lir.cfg.node_indices() {
        let node = &lir.cfg[node_idx];
        let last_op = node.ops.last().unwrap();
        if let lir::OpKind::Case { ref vars, ref clauses,
                                   ref value_vars } = last_op.kind {
            println!("{:?} {:?}", vars, clauses);
            //match_from_parsed(vars.len(), clauses.as_slice());
        }
    }
}

//pub fn match_from_parsed(
//    num_vals: usize,
//    clauses: &[lir::Clause]) -> PatternCfg {
//
//    let mut pattern = Pattern::new(num_vals);
//
//    for clause in clauses {
//        assert!(clause.patterns.len() == num_vals);
//        let nodes: Vec<_> = clause
//            .patterns.iter()
//            .map(|var| node(&mut pattern, var))
//            .collect();
//        pattern.add_clause(nodes);
//    }
//
//    let cfg = ::pattern::to_decision_tree(&pattern);
//
//    println!("{:?}", pattern);
//    println!("{:?}", cfg);
//
//    cfg
//}
//
//fn node(pat: &mut Pattern, orig: &::ir::hir::Pattern) -> PatternNodeIndex {
//    use ::ir::hir::PatternNode as PN;
//
//    match orig.node {
//        PN::Variable(_) => pat.wildcard(),
//        //PN::Map(ref kvs) => {
//        //    let curr = pat.add_node(PatternNodeKind::Map(kvs.len()));
//
//        //    for kv in kvs.iter() {
//        //        let val = node(pat, &(kv.1).0);
//        //        pat.add_edge(curr, val);
//        //    }
//
//        //    curr
//        //},
//        PN::Atomic(ref val) => {
//            pat.add_node(PatternNodeKind::Atomic(val.clone()))
//        },
//        ref n => panic!("unimplemented: {:?}", n),
//    }
//}
