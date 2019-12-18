use std::collections::HashMap;

use bumpalo::{ Bump, collections::Vec as BVec };

use libeir_ir::FunctionBuilder;
use libeir_ir::{ Block, Value, BasicType };
use libeir_ir::pattern::{ PatternClause, PatternNode };

use libeir_util_pattern_compiler::{ PatternCfg, CfgNodeKind, EdgeRef, NodeIndex };

use super::BFnvHashMap;
use super::erlang_pattern_provider::{
    ErlangPatternProvider, NodeKind, Var, ValueOrConst,
};

pub struct DecisionTreeDestinations<'bump> {
    pub fail: Value,
    pub guards: BVec<'bump, Value>,
    pub bodies: BVec<'bump, Value>,
}

struct LowerCtx<'a, 'b, 'bump> {
    provider: &'a ErlangPatternProvider<'b>,
    destinations: &'a DecisionTreeDestinations<'bump>,
    mapping: BFnvHashMap<'bump, Var, Value>,
}

impl<'a, 'b, 'bump> LowerCtx<'a, 'b, 'bump> {

    fn node_to_value(&self, node: PatternNode, idx: NodeIndex,
                     cfg: &PatternCfg<ErlangPatternProvider>) -> Value
    {
        // PatternNode => PatternCfg Node
        let prov_node = self.provider.pattern_node_to_cfg_node(node);
        // PatternCfg Node => PatternCfg Var
        let prov_var = cfg.leaf_bindings[&idx][&prov_node];
        // PatternCfg Var => Value
        self.mapping[&prov_var]
    }

    fn value_or_const_to_value(&self, bind: ValueOrConst, idx: NodeIndex,
                               b: &mut FunctionBuilder,
                               cfg: &PatternCfg<ErlangPatternProvider>) -> Value {
        match bind {
            ValueOrConst::Value(val) => val,
            ValueOrConst::Const(cons) => b.value(cons),
            ValueOrConst::PrimOp(prim) => b.value(prim),
            ValueOrConst::Node(node) => self.node_to_value(node, idx, cfg),
        }
    }

    fn get_var_value(&self, var: Var) -> Value {
        self.mapping[&var]
    }

    fn bind(&mut self, var: Var, val: Value) {
        self.mapping.insert(var, val);
    }

}

pub fn lower_cfg(
    bump: &Bump,
    b: &mut FunctionBuilder,
    provider: &ErlangPatternProvider,
    cfg: &PatternCfg<ErlangPatternProvider>,
    clauses: &[PatternClause],
    destinations: &DecisionTreeDestinations,
) -> Block {
    assert!(destinations.guards.len() == destinations.bodies.len());

    let entry_kind = &cfg.graph[cfg.entry];
    assert!(*entry_kind == CfgNodeKind::Root);

    let mut ctx = LowerCtx {
        provider,
        mapping: BFnvHashMap::with_hasher_in(&bump, Default::default()),
        destinations,
    };

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

        ctx.bind(edge_weight.variable_binds[0], entry_arg);

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
            block = b.op_unpack_value_list(
                block, ctx.get_var_value(var), var_list_len);
        } else {
            unreachable!()
        }

        // Insert variable binds for all value list elements
        for (idx, var) in val_list_target.weight().variable_binds
            .iter().enumerate()
        {
            let val = b.fun().block_args(block)[idx];
            ctx.bind(*var, val);
        }

        lower_cfg_rec(
            bump,
            b,
            &mut ctx,
            cfg,
            clauses,
            block,
            val_list_target.target(),
        );

    } else if outgoing.len() == 0 {
        // Fail immediately
        b.op_call_flow(block, destinations.fail, &[]);
    } else {
        unreachable!();
    }

    entry_block
}

fn lower_cfg_rec(
    bump: &Bump,
    b: &mut FunctionBuilder,
    ctx: &mut LowerCtx,
    cfg: &PatternCfg<ErlangPatternProvider>,
    clauses: &[PatternClause],
    block: Block,
    node: NodeIndex,
) {
    match cfg.graph[node] {
        CfgNodeKind::Root => unreachable!(),
        CfgNodeKind::Match(var) => {
            let match_val = ctx.get_var_value(var);

            let mut wildcard_node = None;

            let mut match_builder = b.op_match_build();

            for outgoing in cfg.graph.edges(node) {
                let weight = outgoing.weight();
                let kind = weight.kind.unwrap();

                match kind {
                    NodeKind::Wildcard => {
                        assert!(wildcard_node.is_none());
                        wildcard_node = Some(outgoing);
                    }
                    NodeKind::Binary { specifier, size } => {
                        assert!(weight.variable_binds.len() == 2);
                        let size = size.map(|v| ctx.value_or_const_to_value(
                            v, outgoing.target(), b, cfg));
                        let ok = match_builder.push_binary(specifier, size, b);

                        let args = b.block_args(ok);
                        assert!(args.len() == 2);

                        ctx.bind(weight.variable_binds[0], args[0]);
                        ctx.bind(weight.variable_binds[1], args[1]);

                        lower_cfg_rec(
                            bump, b, ctx, cfg, clauses,
                            ok, outgoing.target(),
                        );
                    }
                    NodeKind::TupleSize(size) => {
                        assert!(size == weight.variable_binds.len());

                        let ok = match_builder.push_tuple(size, b);

                        {
                            let args = b.block_args(ok);
                            assert!(args.len() == size);

                            for (var, val) in weight.variable_binds.iter()
                                .zip(args.iter())
                            {
                                ctx.bind(*var, *val);
                            }
                        }

                        // Ok
                        lower_cfg_rec(
                            bump, b, ctx, cfg, clauses,
                            ok, outgoing.target(),
                        );
                    }
                    NodeKind::ListCell => {
                        assert!(weight.variable_binds.len() == 2);

                        let ok = match_builder.push_list_cell(b);

                        {
                            let args = b.block_args(ok);
                            assert!(args.len() == 2);

                            ctx.bind(weight.variable_binds[0], args[0]);
                            ctx.bind(weight.variable_binds[1], args[1]);
                        }

                        // Ok
                        lower_cfg_rec(
                            bump, b, ctx, cfg, clauses,
                            ok, outgoing.target(),
                        );
                    }
                    NodeKind::Map => {
                        let ok = match_builder.push_type(BasicType::Map, b);

                        for bind in weight.variable_binds.iter() {
                            ctx.bind(*bind, match_val);
                        }

                        lower_cfg_rec(
                            bump, b, ctx, cfg, clauses,
                            ok, outgoing.target(),
                        );
                    }
                    NodeKind::MapItem(val_or_const) => {
                        assert!(weight.variable_binds.len() == 1);

                        let val = ctx.value_or_const_to_value(
                            val_or_const, node, b, cfg);

                        let ok = match_builder.push_map_item(val, b);
                        let ok_arg = b.block_args(ok)[0];
                        ctx.bind(weight.variable_binds[0], ok_arg);

                        lower_cfg_rec(
                            bump, b, ctx, cfg, clauses,
                            ok, outgoing.target(),
                        );
                    }
                    NodeKind::Value(val_or_const) => {
                        let val = ctx.value_or_const_to_value(
                            val_or_const, node, b, cfg);
                        let ok = match_builder.push_value(val, b);
                        lower_cfg_rec(
                            bump, b, ctx, cfg, clauses,
                            ok, outgoing.target(),
                        );

                    }
                    _ => unimplemented!("{:?}", kind),
                }
            }

            let wildcard_edge = wildcard_node.unwrap();
            assert!(wildcard_edge.weight().variable_binds.len() == 0);
            let wildcard_block = match_builder.push_wildcard(b);
            lower_cfg_rec(
                bump, b, ctx, cfg, clauses,
                wildcard_block, wildcard_edge.target(),
            );

            match_builder.finish(block, match_val, b);
        }
        CfgNodeKind::Fail => {
            b.op_call_flow(block, ctx.destinations.fail, &[]);
        }
        CfgNodeKind::Leaf(leaf_num) => {
            let clause = clauses[leaf_num];

            let mut args = vec![];
            let num_binds = b.pat().clause_binds(clause).len();
            for bind_num in 0..num_binds {
                let bind_node = b.pat().clause_binds(clause)[bind_num];
                let val = ctx.node_to_value(bind_node, node, cfg);
                args.push(val);
            }

            // Call to guard lambda
            let (ok_block, thr_block) = b.op_call_function(
                block, ctx.destinations.guards[leaf_num], &args);
            let ok_ret = b.block_args(ok_block)[0];

            // Throw is unreachable
            b.op_unreachable(thr_block);

            // Conditional on return
            let (true_block, false_block, non_block) = b.op_if_bool(
                ok_block, ok_ret);
            b.op_unreachable(non_block);

            // If guard succeeds, we enter the body
            b.op_call_flow(true_block, ctx.destinations.bodies[leaf_num], &args);

            // If guard fails, continue in CFG
            {
                let mut edges = cfg.graph.edges(node);
                let edge = edges.next().unwrap();
                assert!(edges.next().is_none());

                lower_cfg_rec(
                    bump, b, ctx, cfg, clauses,
                    false_block, edge.target(),
                );
            }

        }
    }
}
