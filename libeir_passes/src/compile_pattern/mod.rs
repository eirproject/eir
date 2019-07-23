use ::matches::{ matches, assert_matches };

use std::collections::{ HashSet, HashMap };

use ::libeir_ir::OpKind;
use ::libeir_ir::{ Block, Value };
use ::libeir_ir::{ Function, FunctionBuilder, Dialect };

use ::pattern_compiler::{ to_decision_tree, PatternCfg, CfgNodeIndex };

mod erlang_pattern_provider;
use self::erlang_pattern_provider::pattern_to_provider;

mod lower_cfg;
use self::lower_cfg::lower_cfg;

use super::FunctionPass;

pub struct CompilePatternPass {
}

impl CompilePatternPass {

    pub fn new() -> Self {
        CompilePatternPass {
        }
    }

}

impl FunctionPass for CompilePatternPass {
    fn run_function_pass(&mut self, b: &mut FunctionBuilder) {
        self.compile_pattern(b);
    }
}

impl CompilePatternPass {

    pub fn compile_pattern(&mut self, b: &mut FunctionBuilder) {

        // Find all pattern matching constructs
        let case_blocks = {
            let fun = b.fun();

            let mut case_blocks = Vec::new();

            let graph = fun.block_graph();
            for block in graph.dfs_iter() {
                if matches!(fun.block_kind(block), Some(OpKind::Case { .. })) {
                    case_blocks.push(block);
                }
            }

            case_blocks
        };

        for block in case_blocks.iter().cloned() {

            let no_match;
            let mut guards = Vec::new();
            let mut bodies = Vec::new();
            let match_val;
            let mut values = Vec::new();
            let mut clauses = Vec::new();

            if let Some(OpKind::Case { clauses: c }) = b.fun().block_kind(block) {
                clauses.extend(c.as_slice(&b.fun().pool.clause).iter().cloned());
            } else {
                unreachable!()
            };
            let num_clauses = clauses.len();

            // Extract arguments from block reads
            {
                let reads = b.fun().block_reads(block);
                let mut r_iter = reads.iter();

                // First entry is always no_match
                no_match = *r_iter.next().unwrap();

                // Guards and bodies
                for _ in 0..num_clauses {
                    guards.push(*r_iter.next().unwrap());
                    bodies.push(*r_iter.next().unwrap());
                }

                // Match value
                match_val = *r_iter.next().unwrap();

                // Values
                while let Some(val) = r_iter.next() {
                    values.push(*val);
                }
            }

            // Get/validate number of roots
            let mut roots_num = None;
            for clause in clauses.iter() {
                let num = b.pat().clause_root_nodes(*clause).len();
                if let Some(num_old) = roots_num {
                    assert!(num == num_old);
                } else {
                    roots_num = Some(num);
                }
            }

            // Create map of PatternValue => Value
            let mut value_map = HashMap::new();
            let mut value_idx = 0;
            for clause in clauses.iter() {
                for value in b.pat().clause_values(*clause) {
                    value_map.insert(*value, values[value_idx]);
                    value_idx += 1;
                }
            }
            assert!(values.len() == value_idx);

            let mut pat_node_to_node_map = HashMap::new();
            let mut provider = pattern_to_provider(b, &clauses, &mut pat_node_to_node_map, &value_map);
            let decision_tree = to_decision_tree(&mut provider);

            let mut out = Vec::new();
            decision_tree.to_dot(&mut out).unwrap();
            println!("{}", std::str::from_utf8(&out).unwrap());

            let cfg_entry = lower_cfg(b, &decision_tree, &pat_node_to_node_map, &clauses, no_match, &guards, &bodies);

            b.block_clear(block);
            b.op_call(block, cfg_entry, &[match_val]);

        }

        //for start_op in case_starts.iter() {

        //    let (case_map, collector, provider, decision_tree) = {
        //        let fun = b.function();
        //        let case_map = map_case_structure(fun, *start_op);

        //        let start_kind = fun.op_kind(case_map.start_op);
        //        let clauses = if let OpKind::CaseStart { clauses } = start_kind { clauses }
        //        else { panic!() };

        //        let (collector, mut provider) =
        //            erlang_pattern_provider::pattern_to_provider(clauses);
        //        let decision_tree = ::pattern_compiler::to_decision_tree(&mut provider);

        //        (case_map, collector, provider, decision_tree)
        //    };

        //    b.position_at_end(case_map.body_ebb);

        //    let mut value_bindings = HashMap::new();

        //    let match_start = b.insert_ebb();
        //    b.position_at_end(match_start);
        //    let destinations = decision_tree_to_cfg(
        //        &decision_tree, &collector, b, &case_map, &mut value_bindings);

        //    // Graft new CFG into matching construct

        //    // Fail jump
        //    b.position_at_end(destinations.fail);
        //    let fail_node = b.function().ebb_call_target(case_map.fail_branch);
        //    let fail_call = b.create_ebb_call(fail_node, &[]);
        //    b.op_jump(fail_call);

        //    // Remove case_start op
        //    let start_block = b.function().op_ebb(case_map.start_op);
        //    b.function_mut().op_remove(case_map.start_op);
        //    // Insert jump to new control flow instead
        //    b.position_at_end(start_block);
        //    let start_call = b.create_ebb_call(match_start, &[]);
        //    b.op_jump(start_call);

        //    let mut write_tokens = Vec::new();

        //    // Change guard entry points
        //    for (idx, guard_branch) in case_map.guard_branches.iter().enumerate() {
        //        let leaf_ebb = destinations.leaves[idx];
        //        let guard_entry = b.function().ebb_call_target(*guard_branch);

        //        b.position_at_end(leaf_ebb);
        //        let call = b.create_ebb_call(guard_entry, &[]);
        //        b.op_jump(call);

        //        let case_values_op = b.function().iter_op(guard_entry).next().unwrap();
        //        assert_matches!(b.function().op_kind(case_values_op), OpKind::CaseValues);

        //        b.function_mut().op_remove_take_writes(case_values_op, &mut write_tokens);
        //        b.position_at_start(guard_entry);
        //        assert!(write_tokens.len() == collector.clause_assigns[idx].len());
        //        for (tok, assign) in write_tokens.drain(..).zip(collector.clause_assigns[idx].iter()) {
        //            b.op_move_write_token(value_bindings[assign], tok);
        //        }
        //    }

        //    // Change guard fails
        //    for (clause_num, fail_op) in case_map.fail_ops.iter() {
        //        let source_ebb = b.function().op_ebb(*fail_op);
        //        let target_ebb = destinations.guard_fails[*clause_num];
        //        b.function_mut().op_remove(*fail_op);
        //        b.position_at_end(source_ebb);
        //        let call = b.create_ebb_call(target_ebb, &[]);
        //        b.op_jump(call);
        //    }

        //    // Remove case body Ebb
        //    b.function_mut().ebb_remove(case_map.body_ebb);

        //    // Remove CaseGuardOk
        //    for op in case_map.ok_ops {
        //        b.function_mut().op_remove(op);
        //    }

        //}

        //b.function_mut().set_dialect(Dialect::Normal);

    }

}

