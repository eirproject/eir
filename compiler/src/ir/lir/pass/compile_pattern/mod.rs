use ::ir::lir;

use ::petgraph::{ Graph, Direction };
use ::petgraph::graph::NodeIndex;

use ::matches::assert_matches;

use std::collections::{ HashSet, HashMap };

use ::ir::SSAVariable;
use ::ir::hir::Expression;
use ::ir::hir::Clause;

use ::ir::hir::PatternNode;

use ::eir::{ ConstantTerm, AtomicTerm };
//use ::eir::{ Source, FunctionIdent };
use ::eir::op::OpKind;
//use ::eir::cfg::{ LabelN, EdgeN };
use ::eir::{ Ebb, Op, EbbCall, Value };
use ::eir::{ Function, FunctionBuilder };
use ::eir::pattern::ValueAssign;

use ::pattern_compiler::{ PatternProvider, ExpandedClauseNodes, PatternCfg, CfgNodeIndex };

mod erlang_pattern_provider;
use self::erlang_pattern_provider::{ ErlangPatternProvider, PatternValueCollector, PatternVar };

struct CaseDef {
    start_op: Op,
    match_on: Value,
    match_values: Vec<Value>,
    body_ebb: Ebb,
    body_op: Op,
    guard_branches: Vec<EbbCall>,
    fail_branch: EbbCall,
    ok_ops: HashSet<Op>,
    fail_ops: HashSet<(usize, Op)>,
}

fn map_case_structure(fun: &Function, start: Op) -> CaseDef {

    // Start Op
    let start_kind = fun.op_kind(start);
    let clauses = if let OpKind::CaseStart { clauses } = start_kind { clauses }
    else { panic!() };
    let case_val = fun.op_writes(start)[0];
    let case_reads = fun.op_reads(start);
    let case_on = case_reads[0];
    let case_values = &case_reads[1..];

    // Start branch
    let branches = fun.op_branches(start);
    assert!(branches.len() == 1);
    assert!(fun.ebb_call_args(branches[0]).len() == 0);

    // Body Ebb
    let body_ebb = fun.ebb_call_target(branches[0]);
    assert!(fun.ebb_args(body_ebb).len() == 0);

    // Body Op
    let mut body_op_iter = fun.iter_op(body_ebb);
    let body_op = body_op_iter.next().unwrap();
    assert!(body_op_iter.next().is_none());
    assert_matches!(fun.op_kind(body_op), OpKind::Case(num) if *num == clauses.len());

    // Body branches
    let body_branches = fun.op_branches(body_op);
    assert!(body_branches.len() == clauses.len() + 1);

    let mut visited = HashSet::new();
    let mut to_visit = Vec::new();

    for (idx, branch) in body_branches[1..].iter().enumerate() {
        assert!(fun.ebb_call_args(*branch).len() == 0);
        to_visit.push((idx, fun.ebb_call_target(*branch)));
    }
    let guard_branches = body_branches.iter().skip(1).cloned().collect();

    let mut ok_ops = HashSet::new();
    let mut fail_ops = HashSet::new();

    while to_visit.len() > 0 {
        let (clause_num, block) = to_visit.pop().unwrap();
        if visited.contains(&(clause_num, block)) { continue; }

        'ops: for op in fun.iter_op(block) {
            match fun.op_kind(op) {
                OpKind::CaseGuardOk => {
                    if fun.op_reads(op)[0] != case_val { continue 'ops; }
                    ok_ops.insert(op);
                    break 'ops;
                }
                OpKind::CaseGuardFail { clause_num: cnum } => {
                    if fun.op_reads(op)[0] != case_val { continue 'ops; }
                    assert!(clause_num == *cnum);

                    let branches = fun.op_branches(op);
                    assert!(branches.len() == 1);
                    let branch = branches[0];
                    assert!(fun.ebb_call_target(branch) == body_ebb);

                    fail_ops.insert((clause_num, op));
                }
                OpKind::ReturnOk => panic!(),
                OpKind::ReturnThrow => panic!(),
                _ => {
                    for branch in fun.op_branches(op) {
                        let next = fun.ebb_call_target(*branch);
                        to_visit.push((clause_num, next));
                    }
                },
            }
        }
        visited.insert((clause_num, block));
    }

    CaseDef {
        start_op: start,
        match_on: case_on,
        match_values: case_values.to_vec(),
        body_ebb: body_ebb,
        body_op: body_op,
        guard_branches: guard_branches,
        fail_branch: body_branches[0],
        ok_ops: ok_ops,
        fail_ops: fail_ops,
    }
}


















//struct CaseClauseDef {
//    out_edge: EdgeN,
//    fail_edges: HashSet<EdgeN>,
//    success_edges: HashSet<EdgeN>,
//}
//
//struct CaseDef {
//    entry: LabelN,
//    main: LabelN,
//    fail_edge: EdgeN,
//    clauses: Vec<CaseClauseDef>,
//}
//
//fn find_exits(lir: &Function, case_body: Ebb, case_ssa: Value, initial: (EdgeN, LabelN)) -> Option<(HashSet<EdgeN>, HashSet<EdgeN>)> {
//    let mut visited: HashSet<LabelN> = HashSet::new();
//    let mut to_visit = vec![initial];
//
//    let mut ok_exits = HashSet::new();
//    let mut fail_exits = HashSet::new();
//
//    'outer: while to_visit.len() > 0 {
//        let (in_edge, in_label) = to_visit.pop().unwrap();
//        if visited.contains(&in_label) { continue };
//
//        let node = &lir.graph[in_label];
//        let node_inner = node.inner.borrow();
//
//        visited.insert(in_label);
//        for op in node_inner.ops.iter() {
//            let v = Source::Variable(case_ssa);
//            match op.kind {
//                OpKind::CaseGuardOk if op.reads[0] == v => {
//                    ok_exits.insert(in_edge);
//                    continue 'outer;
//                },
//                OpKind::CaseGuardFail { .. } if op.reads[0] == v => {
//                    assert!(node.outgoing.len() == 1);
//                    assert!(node.outgoing[0].1 == case_body);
//                    fail_exits.insert(node.outgoing[0].0);
//                    continue 'outer;
//                },
//                _ => ()
//            }
//        }
//        let op = node_inner.ops.last().unwrap();
//        match op.kind {
//            ref kind if kind.num_jumps() == Some(0) => {
//                return None;
//            }
//            OpKind::CaseStart { .. } if op.writes[0] == case_ssa => {
//                return None;
//            }
//            _ => {
//                for out in node.outgoing.iter() {
//                    to_visit.push(*out);
//                }
//            }
//        }
//    }
//
//    Some((ok_exits, fail_exits))
//}

struct DecisionTreeDestinations {
    fail: Ebb,
    guard_fails: Vec<Ebb>,
    leaves: Vec<Ebb>,
}

fn decision_tree_to_cfg_rec(dec: &PatternCfg<ErlangPatternProvider>,
                            collector: &PatternValueCollector,
                            b: &mut FunctionBuilder,
                            mappings: &mut HashMap<PatternVar, Value>,
                            destinations: &DecisionTreeDestinations,
                            value_bindings: &mut HashMap<ValueAssign, Value>,
                            case_map: &CaseDef,
                            cfg_node: CfgNodeIndex) {
    use ::pattern_compiler::CfgNodeKind;
    use self::erlang_pattern_provider::NodeKind as MatchKind;
    use ::pattern_compiler::EdgeRef;

    match dec.graph[cfg_node] {
        CfgNodeKind::Root => unreachable!(),
        CfgNodeKind::Match(var) => {
            let match_val = mappings[&var];
            let mut wildcard_node = None;
            for outgoing in dec.graph.edges(cfg_node) {
                let weight = outgoing.weight();
                let kind = weight.kind.unwrap();
                match kind {
                    MatchKind::Wildcard => {
                        assert!(wildcard_node.is_none());
                        wildcard_node = Some(outgoing);
                    },
                    MatchKind::TupleSize(size) => {
                        assert!(size == weight.variable_binds.len());

                        let nok_ebb = b.insert_ebb();

                        let mut values = Vec::new();
                        let call = b.create_ebb_call(nok_ebb, &[]);
                        b.op_unpack_tuple(match_val, size, &mut values, call);

                        assert!(values.len() == weight.variable_binds.len());

                        for (var, val) in weight.variable_binds.iter().zip(values.iter()) {
                            mappings.insert(*var, *val);
                        }

                        decision_tree_to_cfg_rec(
                            dec, collector, b, mappings, destinations,
                            value_bindings, case_map, outgoing.target()
                        );

                        b.position_at_end(nok_ebb);
                    },
                    MatchKind::Atomic(num) => {
                        assert!(weight.variable_binds.len() == 0);
                        let atomic_val = collector.atomic_terms[num].clone();

                        let nok_ebb = b.insert_ebb();
                        let call = b.create_ebb_call(nok_ebb, &[]);
                        let constant = b.create_atomic(atomic_val);
                        b.op_equal(match_val, constant, call);

                        decision_tree_to_cfg_rec(
                            dec, collector, b, mappings, destinations,
                            value_bindings, case_map, outgoing.target()
                        );

                        b.position_at_end(nok_ebb);
                    },
                    MatchKind::ListCell => {
                        assert!(weight.variable_binds.len() == 2);

                        let nok_ebb = b.insert_ebb();
                        let call = b.create_ebb_call(nok_ebb, &[]);
                        let (head, tail) = b.op_unpack_list_cell(match_val, call);

                        mappings.insert(weight.variable_binds[0], head);
                        mappings.insert(weight.variable_binds[1], tail);

                        decision_tree_to_cfg_rec(
                            dec, collector, b, mappings, destinations,
                            value_bindings, case_map, outgoing.target()
                        );

                        b.position_at_end(nok_ebb);
                    },
                    MatchKind::Map => {
                        if let Some(map_bind) = weight.variable_binds.get(0) {
                            for var_bind in weight.variable_binds.iter() {
                                assert!(map_bind == var_bind);
                            }
                            mappings.insert(*map_bind, match_val);
                        }

                        let nok_ebb = b.insert_ebb();
                        let call = b.create_ebb_call(nok_ebb, &[]);
                        b.op_is_map(match_val, call);

                        decision_tree_to_cfg_rec(
                            dec, collector, b, mappings, destinations,
                            value_bindings, case_map, outgoing.target()
                        );

                        b.position_at_end(nok_ebb);
                    },
                    MatchKind::MapItem(key_value_var) => {
                        let nok_ebb = b.insert_ebb();
                        let call = b.create_ebb_call(nok_ebb, &[]);
                        let value = b.op_map_get(
                            match_val, case_map.match_values[key_value_var.0], call);
                        mappings.insert(weight.variable_binds[0], value);

                        decision_tree_to_cfg_rec(
                            dec, collector, b, mappings, destinations,
                            value_bindings, case_map, outgoing.target()
                        );

                        b.position_at_end(nok_ebb);
                    },
                    _ => {
                        println!("{:?}", kind);
                        unimplemented!();
                    },
                }
            }

            let wildcard_edge = wildcard_node.unwrap();
            assert!(wildcard_edge.weight().variable_binds.len() == 0);
            decision_tree_to_cfg_rec(
                dec, collector, b, mappings, destinations,
                value_bindings, case_map, wildcard_edge.target()
            );
        },
        CfgNodeKind::Fail => {
            let call = b.create_ebb_call(destinations.fail, &[]);
            b.op_jump(call);
        },
        CfgNodeKind::Leaf(num) => {
            let dest_leaf = destinations.leaves[num];
            let dest_call = b.create_ebb_call(dest_leaf, &[]);
            b.op_jump(dest_call);

            let leaf_binding = &dec.leaf_bindings[&cfg_node];
            for (cfg_var, pattern_ref) in leaf_binding.iter() {
                if collector.node_bindings.contains_key(pattern_ref) {
                    let from_var = mappings[cfg_var];
                    let to_assign = collector.node_bindings[pattern_ref];
                    value_bindings.insert(to_assign, from_var);
                }
            }

            let edges: Vec<_> = dec.graph.edges(cfg_node).collect();
            assert!(edges.len() == 1);
            b.position_at_end(destinations.guard_fails[num]);
            decision_tree_to_cfg_rec(
                dec, collector, b, mappings, destinations,
                value_bindings, case_map, edges[0].target()
            );
        },
        CfgNodeKind::Guard => unreachable!(),
    }

}

fn decision_tree_to_cfg(dec: &PatternCfg<ErlangPatternProvider>,
                        collector: &PatternValueCollector,
                        b: &mut FunctionBuilder,
                        case_map: &CaseDef,
                        value_bindings: &mut HashMap<ValueAssign, Value>)
                        -> DecisionTreeDestinations {

    use ::pattern_compiler::CfgNodeKind;
    use self::erlang_pattern_provider::NodeKind as MatchKind;
    use ::pattern_compiler::EdgeRef;

    let entry_dec = &dec.graph[dec.entry];
    assert!(*entry_dec == CfgNodeKind::Root);

    let mut mappings: HashMap<PatternVar, Value> = HashMap::new();

    // First node is just a dummy node
    let value_list_node = {
        let outgoing: Vec<_> = dec.graph.edges(dec.entry).collect();
        assert!(outgoing.len() == 1);

        let o_edge = &outgoing[0];
        let weight = o_edge.weight();
        assert!(weight.kind == Some(MatchKind::Wildcard));
        assert!(weight.variable_binds.len() == 1);

        mappings.insert(weight.variable_binds[0], case_map.match_on);

        let outgoing_n: Vec<_> = dec.graph.neighbors(dec.entry).collect();
        assert!(outgoing_n.len() == 1);
        outgoing_n[0]
    };

    // Second node is always matching on value list in our case
    let start_node = {
        let outgoing: Vec<_> = dec.graph.edges(value_list_node).collect();
        assert!(outgoing.len() == 2);
        // Expect one branch going to fail
        assert!(outgoing.iter()
                .find(|o| o.weight().kind == Some(MatchKind::Wildcard)).is_some());
        let real_target = outgoing.iter()
            .find(|o| o.weight().kind == Some(MatchKind::ValueList))
            .unwrap();

        let mut unpacked = Vec::new();

        if let CfgNodeKind::Match(var) = dec.graph[value_list_node] {
            b.op_unpack_value_list(mappings[&var],
                                   real_target.weight().variable_binds.len(),
                                   &mut unpacked);
        } else {
            panic!();
        }

        for (var, val) in real_target.weight().variable_binds.iter().zip(unpacked.iter()) {
            mappings.insert(*var, *val);
        }

        real_target.target()
    };

    let num_clauses = case_map.guard_branches.len();
    let destinations = DecisionTreeDestinations {
        fail: b.insert_ebb(),
        leaves: (0..num_clauses).map(|_| b.insert_ebb()).collect(),
        guard_fails: (0..num_clauses).map(|_| b.insert_ebb()).collect(),
    };

    decision_tree_to_cfg_rec(dec, collector, b, &mut mappings,
                             &destinations, value_bindings, case_map, start_node);

    destinations
}



pub fn compile_pattern(b: &mut FunctionBuilder) {

    // Find all pattern matching constructs
    let case_starts = {
        let fun = b.function();
        let mut case_starts = Vec::new();

        for ebb in fun.iter_ebb() {
            for op in fun.iter_op(ebb) {
                if matches!(fun.op_kind(op), OpKind::CaseStart { .. }) {
                    case_starts.push(op);
                }
            }
        }

        case_starts
    };

    for start_op in case_starts.iter() {

        let (case_map, collector, provider, decision_tree) = {
            let fun = b.function();
            let case_map = map_case_structure(fun, *start_op);

            let start_kind = fun.op_kind(case_map.start_op);
            let clauses = if let OpKind::CaseStart { clauses } = start_kind { clauses }
            else { panic!() };

            let (collector, mut provider) =
                erlang_pattern_provider::pattern_to_provider(clauses);
            let decision_tree = ::pattern_compiler::to_decision_tree(&mut provider);

            (case_map, collector, provider, decision_tree)
        };

        b.position_at_end(case_map.body_ebb);

        let mut value_bindings = HashMap::new();

        let match_start = b.insert_ebb();
        b.position_at_end(match_start);
        let destinations = decision_tree_to_cfg(
            &decision_tree, &collector, b, &case_map, &mut value_bindings);

        // Graft new CFG into matching construct

        // Fail jump
        b.position_at_end(destinations.fail);
        let fail_node = b.function().ebb_call_target(case_map.fail_branch);
        let fail_call = b.create_ebb_call(fail_node, &[]);
        b.op_jump(fail_call);

        // Remove case_start op
        let start_block = b.function().op_ebb(case_map.start_op);
        b.function_mut().op_remove(case_map.start_op);
        // Insert jump to new control flow instead
        b.position_at_end(start_block);
        let start_call = b.create_ebb_call(match_start, &[]);
        b.op_jump(start_call);

        let mut write_tokens = Vec::new();

        // Change guard entry points
        for (idx, guard_branch) in case_map.guard_branches.iter().enumerate() {
            let leaf_ebb = destinations.leaves[idx];
            let guard_entry = b.function().ebb_call_target(*guard_branch);

            b.position_at_end(leaf_ebb);
            let call = b.create_ebb_call(guard_entry, &[]);
            b.op_jump(call);

            let case_values_op = b.function().iter_op(guard_entry).next().unwrap();
            assert_matches!(b.function().op_kind(case_values_op), OpKind::CaseValues);

            b.function_mut().op_remove_take_writes(case_values_op, &mut write_tokens);
            b.position_at_start(guard_entry);
            assert!(write_tokens.len() == collector.clause_assigns[idx].len());
            for (tok, assign) in write_tokens.drain(..).zip(collector.clause_assigns[idx].iter()) {
                b.op_move_write_token(value_bindings[assign], tok);
            }
        }

        // Change guard fails
        for (clause_num, fail_op) in case_map.fail_ops.iter() {
            let source_ebb = b.function().op_ebb(*fail_op);
            let target_ebb = destinations.guard_fails[*clause_num];
            b.function_mut().op_remove(*fail_op);
            b.position_at_end(source_ebb);
            let call = b.create_ebb_call(target_ebb, &[]);
            b.op_jump(call);
        }

        // Remove case body Ebb
        b.function_mut().ebb_remove(case_map.body_ebb);

        // Remove CaseGuardOk
        for op in case_map.ok_ops {
            b.function_mut().op_remove(op);
        }

    }

}
