use std::collections::HashMap;

use libeir_ir::FunctionBuilder;
use libeir_ir::{ Block, Value };
use libeir_ir::pattern::{ PatternClause, PatternNode };

use libeir_intern::Ident;

use pattern_compiler::{ PatternCfg, CfgNodeKind, EdgeRef, NodeIndex };

use super::erlang_pattern_provider::{ ErlangPatternProvider, NodeKind, Var, Node };

struct DecisionTreeDestinations<'a> {
    fail: Value,
    guards: &'a [Value],
    bodies: &'a [Value],
}

pub fn lower_cfg(
    b: &mut FunctionBuilder,
    cfg: &PatternCfg<ErlangPatternProvider>,
    node_map: &HashMap<PatternNode, Node>,
    clauses: &[PatternClause],
    no_match: Value,
    guards: &[Value],
    bodies: &[Value],
) -> Block {
    assert!(guards.len() == bodies.len());

    let entry_kind = &cfg.graph[cfg.entry];
    assert!(*entry_kind == CfgNodeKind::Root);

    let mut mapping: HashMap<Var, Value> = HashMap::new();

    let entry_block = b.block_insert();
    let entry_arg = b.block_arg_insert(entry_block);

    let mut block = entry_block;

    // First node is a dummy root node
    let value_list_node = {
        let mut edges = cfg.graph.edges(cfg.entry);
        let edge  = edges.next().unwrap();
        assert!(edges.next().is_none());

        let edge_weight = edge.weight();
        assert!(edge_weight.kind == Some(NodeKind::Wildcard));
        assert!(edge_weight.variable_binds.len() == 1);

        mapping.insert(edge_weight.variable_binds[0], entry_arg);

        edge.target()
    };

    let outgoing: Vec<_> = cfg.graph.edges(value_list_node).collect();
    if outgoing.len() == 2 {

        // This will always be a ValueList and a Wildcard
        let val_list_target = outgoing.iter()
            .find(|o| o.weight().kind == Some(NodeKind::ValueList))
            .unwrap();
        assert!(outgoing.iter()
                .find(|o| o.weight().kind == Some(NodeKind::Wildcard))
                .is_some());

        if let CfgNodeKind::Match(var) = cfg.graph[value_list_node] {
            let var_list_len = val_list_target.weight().variable_binds.len();
            block = b.op_unpack_value_list(block, mapping[&var], var_list_len);
        } else {
            unreachable!()
        }

        // Insert variable binds for all value list elements
        for (idx, var) in val_list_target.weight().variable_binds
            .iter().enumerate()
        {
            let val = b.fun().block_args(block)[idx];
            mapping.insert(*var, val);
        }

        let destinations = DecisionTreeDestinations {
            fail: no_match,
            guards,
            bodies,
        };

        lower_cfg_rec(
            b,
            &cfg,
            node_map,
            clauses,
            &mut mapping,
            &destinations,
            block,
            val_list_target.target(),
        );

    } else if outgoing.len() == 0 {
        // Fail immediately
        b.op_call(block, no_match, &[]);
    } else {
        unreachable!();
    }

    entry_block
}

fn lower_cfg_rec(
    b: &mut FunctionBuilder,
    cfg: &PatternCfg<ErlangPatternProvider>,
    node_map: &HashMap<PatternNode, Node>,
    clauses: &[PatternClause],
    mapping: &mut HashMap<Var, Value>,
    destinations: &DecisionTreeDestinations,
    mut block: Block,
    node: NodeIndex,
) {
    match cfg.graph[node] {
        CfgNodeKind::Root => unreachable!(),
        CfgNodeKind::Match(var) => {
            let match_val = mapping[&var];

            let mut wildcard_node = None;

            for outgoing in cfg.graph.edges(node) {
                let weight = outgoing.weight();
                let kind = weight.kind.unwrap();

                match kind {
                    NodeKind::Wildcard => {
                        assert!(wildcard_node.is_none());
                        wildcard_node = Some(outgoing);
                    }
                    NodeKind::TupleSize(size) => {
                        assert!(size == weight.variable_binds.len());

                        let (ok, fail) = b.op_unpack_tuple(block, match_val, size);

                        {
                            let args = b.block_args(ok);
                            assert!(args.len() == size);

                            for (var, val) in weight.variable_binds.iter().zip(args.iter()) {
                                mapping.insert(*var, *val);
                            }
                        }

                        // Ok
                        lower_cfg_rec(
                            b, cfg, node_map, clauses, mapping,
                            destinations, ok, outgoing.target(),
                        );

                        // Fail
                        block = fail;
                    }
                    NodeKind::ListCell => {
                        assert!(weight.variable_binds.len() == 2);

                        let (ok, fail) = b.op_unpack_list_cell(block, match_val);

                        {
                            let args = b.block_args(ok);
                            assert!(args.len() == 2);

                            mapping.insert(weight.variable_binds[0], args[0]);
                            mapping.insert(weight.variable_binds[1], args[1]);
                        }

                        // Ok
                        lower_cfg_rec(
                            b, cfg, node_map, clauses, mapping,
                            destinations, ok, outgoing.target(),
                        );

                        // Fail
                        block = fail;
                    }
                    NodeKind::Value(val_or_const) => {
                        let val = val_or_const.to_value(b);

                        let (cont_block, cont_block_val) = b.block_insert_get_val();
                        let eq_res = b.block_arg_insert(cont_block);

                        let (fail_cont_block, fail_cont_block_val) = b.block_insert_get_val();
                        b.block_arg_insert(fail_cont_block);
                        b.block_arg_insert(fail_cont_block);
                        b.block_arg_insert(fail_cont_block);
                        b.op_unreachable(fail_cont_block);

                        // Make atoms
                        let erlang_atom = b.value(Ident::from_str("erlang"));
                        let exacteq_atom = b.value(Ident::from_str("=:="));
                        let two_atom = b.value(2);

                        // Capture exacteq function
                        block = b.op_capture_function(block, erlang_atom, exacteq_atom, two_atom);
                        let fun_val = b.block_args(block)[0];

                        // Call exacteq
                        b.op_call(block, fun_val, &[cont_block_val, fail_cont_block_val, match_val, val]);
                        block = cont_block;

                        let (true_block, false_block, non_block) = b.op_if_bool(block, eq_res);

                        // Non
                        b.op_unreachable(non_block);

                        // True
                        lower_cfg_rec(
                            b, cfg, node_map, clauses, mapping,
                            destinations, true_block, outgoing.target(),
                        );

                        // False
                        block = false_block;

                    }
                    _ => unimplemented!("{:?}", kind),
                }
            }

            let wildcard_edge = wildcard_node.unwrap();
            assert!(wildcard_edge.weight().variable_binds.len() == 0);

            lower_cfg_rec(
                b, cfg, node_map, clauses, mapping,
                destinations, block, wildcard_edge.target(),
            );
        }
        CfgNodeKind::Fail => {
            b.op_call(block, destinations.fail, &[]);
        }
        CfgNodeKind::Leaf(num) => {
            let clause = clauses[num];

            let guard_cont = b.block_insert();
            let guard_ret = b.block_arg_insert(guard_cont);

            let mut args = vec![];

            // Call to guard lambda
            {
                // First argument is the continuation
                args.push(b.value(guard_cont));

                let num_binds = b.pat().clause_binds(clause).len();

                let leaf_bindings = &cfg.leaf_bindings[&node];
                println!("Node: {:?}", node);
                for bind_num in 0..num_binds {
                    let bind_node = b.pat().clause_binds(clause)[bind_num];
                    let prov_node = node_map[&bind_node];
                    println!("prov_node: {:?}", prov_node);
                    let prov_var = leaf_bindings[&prov_node];
                    println!("prov_var: {:?}", prov_var);
                    let val = mapping[&prov_var];

                    args.push(val);
                }

                b.op_call(block, destinations.guards[num], &args);

                args.remove(0);
            }

            // Conditional on return
            let (true_block, false_block, non_block) = b.op_if_bool(guard_cont, guard_ret);
            b.op_unreachable(non_block);

            // If guard succeeds, we enter the body
            b.op_call(true_block, destinations.bodies[num], &args);

            // If guard fails, continue in CFG
            {
                let mut edges = cfg.graph.edges(node);
                let edge = edges.next().unwrap();
                assert!(edges.next().is_none());

                lower_cfg_rec(
                    b, cfg, node_map, clauses,
                    mapping, destinations,
                    false_block, edge.target(),
                );
            }

        }
    }
}
