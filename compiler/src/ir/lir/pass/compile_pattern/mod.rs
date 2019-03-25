use ::ir::lir;

use ::petgraph::{ Graph, Direction };
use ::petgraph::graph::NodeIndex;

use std::collections::{ HashSet, HashMap };

use ::ir::SSAVariable;
use ::ir::hir::Expression;
use ::ir::hir::Clause;

use ::ir::hir::PatternNode;

use ::eir::{ ConstantTerm, AtomicTerm };
use ::eir::{ Source, FunctionIdent };
use ::eir::op::OpKind;
use ::eir::cfg::{ LabelN, EdgeN };

use ::pattern_compiler::{ PatternProvider, ExpandedClauseNodes, PatternCfg, CfgNodeIndex };

mod erlang_pattern_provider;
use self::erlang_pattern_provider::{ ErlangPatternProvider, PatternValueCollector, PatternVar };

struct CaseClauseDef {
    out_edge: EdgeN,
    fail_edges: HashSet<EdgeN>,
    success_edges: HashSet<EdgeN>,
}

struct CaseDef {
    entry: LabelN,
    main: LabelN,
    fail_edge: EdgeN,
    clauses: Vec<CaseClauseDef>,
}

fn find_exits(lir: &::eir::cfg::FunctionCfg, case_body: LabelN, case_ssa: SSAVariable, initial: (EdgeN, LabelN)) -> Option<(HashSet<EdgeN>, HashSet<EdgeN>)> {
    let mut visited: HashSet<LabelN> = HashSet::new();
    let mut to_visit = vec![initial];

    let mut ok_exits = HashSet::new();
    let mut fail_exits = HashSet::new();

    'outer: while to_visit.len() > 0 {
        let (in_edge, in_label) = to_visit.pop().unwrap();
        if visited.contains(&in_label) { continue };

        let node = &lir.graph[in_label];
        let node_inner = node.inner.borrow();

        visited.insert(in_label);
        for op in node_inner.ops.iter() {
            let v = Source::Variable(case_ssa);
            match op.kind {
                OpKind::CaseGuardOk if op.reads[0] == v => {
                    ok_exits.insert(in_edge);
                    continue 'outer;
                },
                OpKind::CaseGuardFail { .. } if op.reads[0] == v => {
                    assert!(node.outgoing.len() == 1);
                    assert!(node.outgoing[0].1 == case_body);
                    fail_exits.insert(node.outgoing[0].0);
                    continue 'outer;
                },
                _ => ()
            }
        }
        let op = node_inner.ops.last().unwrap();
        match op.kind {
            ref kind if kind.num_jumps() == Some(0) => {
                return None;
            }
            OpKind::CaseStart { .. } if op.writes[0] == case_ssa => {
                return None;
            }
            _ => {
                for out in node.outgoing.iter() {
                    to_visit.push(*out);
                }
            }
        }
    }

    Some((ok_exits, fail_exits))
}

struct DecisionTreeDestinations {
    fail: LabelN,
    guard_fails: Vec<LabelN>,
    leaves: Vec<LabelN>,
}

fn decision_tree_to_cfg_rec(dec: &PatternCfg<ErlangPatternProvider>,
                            collector: &PatternValueCollector,
                            builder: &mut ::eir::cfg::FunctionCfgBuilder,
                            mappings: &mut HashMap<PatternVar, SSAVariable>,
                            value_vars: &[Source],
                            destinations: &DecisionTreeDestinations,
                            block: LabelN,
                            cfg_node: CfgNodeIndex) {
    use ::pattern_compiler::CfgNodeKind;
    use self::erlang_pattern_provider::NodeKind as MatchKind;
    use ::pattern_compiler::EdgeRef;

    match dec.graph[cfg_node] {
        CfgNodeKind::Root => unreachable!(),
        CfgNodeKind::Match(var) => {
            let match_ssa = mappings[&var];
            let mut block_cont = block;
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
                        let ssa_binds: Vec<_> = (0..size)
                            .map(|_| builder.new_ssa()).collect();
                        for (ssa, var) in ssa_binds.iter().zip(
                            weight.variable_binds.iter()) {
                            mappings.insert(*var, *ssa);
                        }
                        let ok_block = builder.add_block();
                        let nok_block = builder.add_block();
                        builder.op_unpack_tuple(
                            block_cont,
                            match_ssa.into(), ssa_binds,
                            ok_block, nok_block
                        );
                        block_cont = nok_block;
                        decision_tree_to_cfg_rec(
                            dec, collector, builder, mappings, value_vars,
                            destinations, ok_block, outgoing.target()
                        );
                    },
                    MatchKind::Atomic(num) => {
                        assert!(weight.variable_binds.len() == 0);
                        let atomic_val = collector.atomic_terms[num].clone();
                        let ok_block = builder.add_block();
                        let nok_block = builder.add_block();
                        builder.op_equal_atomic(
                            block_cont,
                            match_ssa.into(),
                            atomic_val,
                            ok_block, nok_block
                        );
                        block_cont = nok_block;
                        decision_tree_to_cfg_rec(
                            dec, collector, builder, mappings, value_vars,
                            destinations, ok_block, outgoing.target()
                        );
                    },
                    MatchKind::ListCell => {
                        assert!(weight.variable_binds.len() == 2);
                        let ok_block = builder.add_block();
                        let nok_block = builder.add_block();
                        let head_ssa = builder.new_ssa();
                        let tail_ssa = builder.new_ssa();
                        mappings.insert(weight.variable_binds[0], head_ssa);
                        mappings.insert(weight.variable_binds[1], tail_ssa);
                        builder.op_unpack_list_cell(
                            block_cont,
                            match_ssa.into(),
                            head_ssa, tail_ssa,
                            ok_block, nok_block
                        );
                        block_cont = nok_block;
                        decision_tree_to_cfg_rec(
                            dec, collector, builder, mappings, value_vars,
                            destinations, ok_block, outgoing.target()
                        );
                    },
                    MatchKind::Map => {
                        let map_bind = weight.variable_binds[0];
                        for var_bind in weight.variable_binds.iter() {
                            assert!(map_bind == *var_bind);
                        }

                        let ok_block = builder.add_block();
                        let nok_block = builder.add_block();
                        mappings.insert(map_bind, match_ssa);
                        builder.op_is_map(
                            block_cont,
                            match_ssa.into(),
                            ok_block, nok_block,
                        );
                        block_cont = nok_block;
                        decision_tree_to_cfg_rec(
                            dec, collector, builder, mappings, value_vars,
                            destinations, ok_block, outgoing.target(),
                        );
                    },
                    MatchKind::MapItem(key_value_var) => {
                        let ok_block = builder.add_block();
                        let nok_block = builder.add_block();
                        let value_ssa = builder.new_ssa();
                        mappings.insert(weight.variable_binds[0], value_ssa);
                        builder.op_map_get(
                            block_cont,
                            match_ssa.into(),
                            value_vars[key_value_var].clone(),
                            value_ssa,
                            ok_block, nok_block,
                        );
                        block_cont = nok_block;
                        decision_tree_to_cfg_rec(
                            dec, collector, builder, mappings, value_vars,
                            destinations, ok_block, outgoing.target(),
                        );
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
                dec, collector, builder, mappings, value_vars,
                destinations, block_cont, wildcard_edge.target()
            );
        },
        CfgNodeKind::Fail => {
            builder.op_jump(block, destinations.fail);
        },
        CfgNodeKind::Leaf(num) => {
            let dest_leaf = destinations.leaves[num];
            builder.op_jump(block, dest_leaf);
            let leaf_binding = &dec.leaf_bindings[&cfg_node];
            for (cfg_var, pattern_ref) in leaf_binding.iter() {
                if collector.node_bindings.contains_key(pattern_ref) {
                    let from_ssa = mappings[cfg_var];
                    let to_ssa = collector.node_bindings[pattern_ref];
                    builder.op_move(dest_leaf, from_ssa, to_ssa);
                }
            }

            let edges: Vec<_> = dec.graph.edges(cfg_node).collect();
            assert!(edges.len() == 1);
            decision_tree_to_cfg_rec(
                dec, collector, builder, mappings, value_vars,
                destinations, destinations.guard_fails[num], edges[0].target()
            );
        },
        CfgNodeKind::Guard => unreachable!(),
    }

}

fn decision_tree_to_cfg(dec: &PatternCfg<ErlangPatternProvider>,
                        collector: &PatternValueCollector,
                        builder: &mut ::eir::cfg::FunctionCfgBuilder,
                        value_vars: &[Source],
                        num_clauses: usize,
                        entry: LabelN,
                        val: SSAVariable)
                        -> DecisionTreeDestinations {

    use ::pattern_compiler::CfgNodeKind;
    use self::erlang_pattern_provider::NodeKind as MatchKind;
    use ::pattern_compiler::EdgeRef;

    let entry_dec = &dec.graph[dec.entry];
    assert!(*entry_dec == CfgNodeKind::Root);

    let mut mappings: HashMap<PatternVar, SSAVariable> = HashMap::new();

    // First node is just a dummy node
    let value_list_node = {
        let outgoing: Vec<_> = dec.graph.edges(dec.entry).collect();
        assert!(outgoing.len() == 1);

        let o_edge = &outgoing[0];
        let weight = o_edge.weight();
        assert!(weight.kind == Some(MatchKind::Wildcard));
        assert!(weight.variable_binds.len() == 1);

        mappings.insert(weight.variable_binds[0], val);

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
        for var in real_target.weight().variable_binds.iter() {
            let ssa = builder.new_ssa();
            mappings.insert(*var, ssa);
            unpacked.push(ssa);
        }

        if let CfgNodeKind::Match(var) = dec.graph[value_list_node] {
            builder.op_unpack_value_list(entry, mappings[&var], unpacked);
        } else {
            panic!();
        }

        real_target.target()
    };

    let destinations = DecisionTreeDestinations {
        fail: builder.add_block(),
        leaves: (0..num_clauses).map(|_| builder.add_block()).collect(),
        guard_fails: (0..num_clauses).map(|_| builder.add_block()).collect(),
    };

    decision_tree_to_cfg_rec(dec, collector, builder, &mut mappings, value_vars, &destinations, entry, start_node);

    destinations
}



pub fn compile_pattern(ident: &FunctionIdent, lir: &mut ::eir::cfg::FunctionCfg) {

    // Find all pattern matching constructs
    let mut case_starts = Vec::new();
    for node in lir.graph.nodes() {
        let inner = node.inner.borrow();
        if let OpKind::CaseStart { .. } = inner.ops.last().unwrap().kind {
            case_starts.push(node.label);
        }
    }

    for start_label in case_starts.iter() {
        // Start node
        let (clause_edges, collector, decision_tree, match_val, num_clauses, fail_edge, entry_edge, case_ssa, value_vars) = {
            let start_node = lir.graph.node(*start_label);
            let start_inner = start_node.inner.borrow();
            assert!(start_node.outgoing.len() == 1);
            let entry_edge = start_node.outgoing[0].0;
            let start_op = start_inner.ops.last().unwrap();
            let (vars, clauses, value_vars) =
                if let OpKind::CaseStart { vars, clauses, value_vars } =
                &start_op.kind { (vars, clauses, value_vars) } else { panic!() };
            assert!(start_op.writes.len() == 1);
            let case_ssa = start_op.writes[0];
            assert!(start_op.reads.len() == (1 + value_vars.len()));
            let match_val = start_op.reads[0].clone();

            // Body node
            let body_node = lir.graph.node(start_node.outgoing[0].1);
            let body_inner = body_node.inner.borrow();
            assert!(body_inner.ops.len() == 1);
            assert!(body_inner.phi_nodes.len() == 0);
            //assert!(body_node.)
            let body_op = &body_inner.ops[0];
            if let OpKind::Case(n) = body_op.kind { n } else { panic!() };
            assert!(body_op.reads[0] == Source::Variable(case_ssa));

            // Fail edge
            let (fail_edge, fail_node) = body_node.outgoing[0];

            let mut clause_edges: Vec<(EdgeN, HashSet<EdgeN>, HashSet<EdgeN>)> =
                Vec::new();
            // Clauses
            for (clause_edge, clause_start_label) in body_node.outgoing.iter().skip(1) {
                if let Some((ok_exits, fail_exits)) = find_exits(
                    lir, body_node.label, case_ssa, (*clause_edge, *clause_start_label)) {
                    clause_edges.push((*clause_edge, ok_exits, fail_exits));
                } else {
                    panic!("Exit inside of pattern guard");
                }
            }

            println!("{:?}", clauses);

            let (collector, mut provider) =
                erlang_pattern_provider::pattern_to_provider(clauses);
            let decision_tree = ::pattern_compiler::to_decision_tree(&mut provider);

            let value_vars_reads: Vec<_> = start_op.reads.iter().skip(1)
                .cloned().collect();

            (clause_edges, collector, decision_tree, match_val, clauses.len(), fail_edge, entry_edge, case_ssa, value_vars_reads)
        };

        let mut builder = ::eir::cfg::FunctionCfgBuilder::new(lir);
        let entry = builder.add_block();
        let match_ssa = builder.new_ssa();
        builder.op_move(entry, match_val, match_ssa);
        let destinations =
            decision_tree_to_cfg(&decision_tree, &collector, &mut builder,
                                 &value_vars, num_clauses, entry, match_ssa);

        // Graft new CFG into matching construct

        // Fail jump
        builder.basic_op(destinations.fail, OpKind::Jump, vec![], vec![]);
        builder.target.graph.reparent_edge(fail_edge, destinations.fail);

        // Entry
        builder.target.graph.rechild_edge(entry_edge, entry);
        {
            let entry_from = builder.target.graph.edge_from(entry_edge);
            let entry_node = builder.target.graph.node(entry_from);
            let mut entry_inner = entry_node.inner.borrow_mut();
            let mut last_op = entry_inner.ops.last_mut().unwrap();
            assert_matches!(last_op.kind, OpKind::CaseStart { .. });
            last_op.kind = OpKind::Jump;
            last_op.writes = vec![];
            last_op.reads = vec![];
        }

        // Clauses
        for ((clause_node, guard_fail_node), (entry_edge, _ok_edges, fail_edges)) in destinations.leaves.iter().zip(destinations.guard_fails.iter()).zip(clause_edges.iter()) {

            {
                let entry_label = builder.target.graph.edge_to(*entry_edge);
                let entry_node = builder.target.graph.node(entry_label);
                let mut entry_inner = entry_node.inner.borrow_mut();
                //println!("{:?}", entry_label);
                //println!("{:?}", entry_inner);
                assert_matches!(entry_inner.ops.first().unwrap().kind,
                                OpKind::CaseValues);
                entry_inner.ops.remove(0);
            }

            builder.basic_op(*clause_node, OpKind::Jump, vec![], vec![]);
            builder.target.graph.reparent_edge(*entry_edge, *clause_node);

            for fail_edge in fail_edges.iter() {
                builder.target.graph.rechild_edge(*fail_edge, *guard_fail_node);
            }

        }

        // Remove remaining operations referencing case SSA
        for node in builder.target.graph.nodes_mut() {
            let mut inner = node.inner.borrow_mut();
            inner.ops.retain(|op| {
                match op.kind {
                    OpKind::CaseGuardFail { .. } if
                        op.reads[0] == Source::Variable(case_ssa)
                        => false,
                    OpKind::CaseGuardOk { .. } if
                        op.reads[0] == Source::Variable(case_ssa)
                        => false,
                    OpKind::TombstoneSSA(ssa) if
                        ssa == case_ssa
                        => false,
                    _ => true,
                }
            });
        }

    }

}
