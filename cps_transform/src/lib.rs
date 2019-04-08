/// When we have a way to calculate live values in the input IR, this transform
/// is actually pretty easy.
/// For every function call site, we generate two closures, one for the exception
/// branch and one for the ok branch.
/// These closures gets passed into the call to the function as the first two
/// arguments.
/// The result is that every function signature gets two extra arguments after
/// the transform has completed. Argument 0 is the ok continuation, argument
/// 1 is the error continuation.
///
/// TODO
/// Right now the input continuations are manually injected into every closure
/// inside the function. This is not optimal if the function terminates
/// without calling a continuation. This should be relatively rare, so this is
/// probably not a big deal.

use std::collections::{ HashMap, HashSet, VecDeque };

use util::pooled_entity_set::PooledEntitySet;
use eir::LambdaEnvIdx;
use eir::{ Function, FunctionBuilder };
use eir::op::OpKind;
use eir::LambdaEnvIdxGenerator;
use eir::{ Ebb, Op, Value, EbbCall };
use eir::fun::live::LiveValues;

fn copy_op(
    src_fun: &Function,
    src_op: Op,
    b: &mut FunctionBuilder,
    // Map from source function => dest function
    val_map: &mut HashMap<Value, Value>,
    ebb_map: &mut HashMap<Ebb, Ebb>,
) {
    let kind = src_fun.op_kind(src_op);
    b.op_build_start(kind.clone());

    for write in src_fun.op_writes(src_op) {
        let new = b.op_build_write();
        val_map.insert(*write, new);
    }
    for read in src_fun.op_reads(src_op) {
        if src_fun.value_is_constant(*read) {
            let value = b.create_constant(src_fun.value_constant(*read).clone());
            b.op_build_read(value);
        } else {
            b.op_build_read(val_map[read]);
        }
    }
    for branch in src_fun.op_branches(src_op) {
        let old_target = src_fun.ebb_call_target(*branch);

        let new = if let Some(ebb) = ebb_map.get(&old_target) {
            *ebb
        } else {
            let new = b.insert_ebb();
            ebb_map.insert(old_target, new);
            for arg in src_fun.ebb_args(old_target) {
                let val = b.add_ebb_argument(new);
                val_map.insert(*arg, val);
            }
            new
        };

        let mut buf = Vec::new();
        for arg in src_fun.ebb_call_args(*branch) {
            if src_fun.value_is_constant(*arg) {
                let value = b.create_constant(src_fun.value_constant(*arg).clone());
                buf.push(value);
            } else {
                buf.push(val_map[arg]);
            }
        }
        let call = b.create_ebb_call(new, &buf);
        b.op_build_ebb_call(call);
    }

    b.op_build_end();
}

#[derive(Debug, Copy, Clone)]
enum ContSite {
    EbbCall(EbbCall, Option<Value>),
    Op(Op),
}

fn gen_chunk(
    src_fun: &Function,
    site: ContSite,
    cont_sites: &HashSet<Op>,
    live: &LiveValues,
    env_idx_gen: &mut LambdaEnvIdxGenerator,
    needed_continuations: &mut Vec<ContSite>,
    // If this is false, this is an entry point
    // If this is true, this is a continuation
    cont: bool,
) {
    let mut ident = src_fun.ident().clone();
    // First two argument become ok and throw continuations
    ident.arity += 2;

    let mut fun = Function::new(ident);
    let mut b = FunctionBuilder::new(&mut fun);

    let mut to_process = VecDeque::new();
    let mut val_map = HashMap::new();
    let mut ebb_map = HashMap::new();
    let mut handled_ops = HashSet::new();
    // Temp
    let mut call_renames: HashMap<Value, Value> = HashMap::new();

    // Add entry ebb
    let entry_ebb = b.insert_ebb_entry();
    b.position_at_end(entry_ebb);

    let ok_ret_cont;
    let err_ret_cont;

    let src_first_op;

    if cont {
        // This is a continuation

        let result_src_val;

        let mut env_vals = Vec::new();
        match site {
            ContSite::Op(op) => {
                // We entered the continuation from flow
                let prev_op = src_fun.op_before(op).unwrap();
                let live_vals = &live.flow_live[&prev_op];
                let result_src_val_i = src_fun.op_writes(prev_op)[0];
                result_src_val = Some(result_src_val_i);
                for src_live in live_vals.iter(&live.pool) {
                    if src_live == result_src_val_i {
                        continue
                    }
                    env_vals.push(src_live);
                }
            }
            ContSite::EbbCall(call, result_after_branch) => {
                // We entered the continuation from a jump
                result_src_val = result_after_branch;
                let call_source = src_fun.ebb_call_source(call);
                let call_target = src_fun.ebb_call_target(call);
                let live_vals = &live.ebb_live[&call_target];
                let src_result_before_val = src_fun.op_writes(call_source)[1];
                for src_after_live in live_vals.iter(&live.pool) {
                    assert!(src_result_before_val != src_after_live);
                    if Some(src_after_live) == result_after_branch {
                        continue
                    }
                    env_vals.push(src_after_live);
                }
            }
        }

        // Get first op and its ebb
        src_first_op = match site {
            ContSite::Op(op) => op,
            ContSite::EbbCall(call, _) => {
                let target = src_fun.ebb_call_target(call);
                src_fun.ebb_first_op(target)
            },
        };
        let src_first_ebb = src_fun.op_ebb(src_first_op);
        ebb_map.insert(src_first_ebb, entry_ebb);

        // Argument for environment
        let env_val = b.add_ebb_argument(entry_ebb);

        // Argument for result
        let res_val = b.add_ebb_argument(entry_ebb);
        if let Some(v) = result_src_val {
            val_map.insert(v, res_val);
        }

        let mut new_env_vars = Vec::new();
        // +2 for ok and err function continuations
        b.op_unpack_env(env_val, env_vals.len() + 2, &mut new_env_vars);

        // Continuations
        ok_ret_cont = new_env_vars[0];
        err_ret_cont = new_env_vars[1];

        // Insert mappings for all in env
        for (src, dst) in env_vals.iter().zip(new_env_vars.iter().skip(2)) {
            val_map.insert(*src, *dst);
        }

    } else {
        // This is a entry point

        // Get Op and Ebb, insert binding
        src_first_op = if let ContSite::Op(op) = site { op } else { panic!() };
        let src_first_ebb = src_fun.op_ebb(src_first_op);
        ebb_map.insert(src_first_ebb, entry_ebb);

        // Arguments for continuations
        ok_ret_cont = b.add_ebb_argument(entry_ebb);
        err_ret_cont = b.add_ebb_argument(entry_ebb);

        // Entry Ebb arguments, insert bindings
        for arg in src_fun.ebb_args(src_first_ebb) {
            let val = b.add_ebb_argument(entry_ebb);
            val_map.insert(*arg, val);
        }

    }

    // Seed op
    to_process.push_back(src_first_op);

    while to_process.len() > 0 {
        let src_op = to_process.pop_front().unwrap();

        if handled_ops.contains(&src_op) { continue; }
        handled_ops.insert(src_op);

        let src_ebb = src_fun.op_ebb(src_op);
        b.position_at_end(ebb_map[&src_ebb]);

        // If we hit a continuation site
        if cont_sites.contains(&src_op) {
            let kind = src_fun.op_kind(src_op);
            match kind {
                OpKind::Apply { .. } => (),
                OpKind::Call { .. } => (),
                _ => panic!(),
            }

            let mut buf = Vec::new();

            let writes = src_fun.op_writes(src_op);
            let ok_val = writes[0];
            let nok_val = writes[1];

            // =========================
            // ==== Ok continuation ====
            // =========================

            // Live variables at the control flow edge
            let ok_live = &live.flow_live[&src_op];

            // Construct the closure environment for the continuation
            buf.clear();
            buf.push(ok_ret_cont);
            buf.push(err_ret_cont);
            for live in ok_live.iter(&live.pool) {
                if live == ok_val {
                    continue;
                }
                buf.push(val_map[&live]);
            }
            let env_idx = env_idx_gen.next();
            let env = b.op_make_closure_env(env_idx, &buf);

            // Bind closure for continuation
            let mut ident = src_fun.ident().clone();
            ident.lambda = Some((env_idx, 0));
            ident.arity = 1;
            let ok_cont = b.op_bind_closure(ident, env);

            // Schedule control flow edge for continuation generation
            let src_next_op = src_fun.op_after(src_op).unwrap();
            needed_continuations.push(ContSite::Op(src_next_op));

            // ============================
            // ==== Throw continuation ====
            // ============================

            // Live variables at the exception edge
            let src_branch = src_fun.op_branches(src_op)[0];
            let src_target = src_fun.ebb_call_target(src_branch);
            let err_live = &live.ebb_live[&src_target];

            // Generate rename map.
            // Maps values after the ebb call to values before
            call_renames.clear();
            for (from, to) in src_fun.ebb_call_args(src_branch).iter()
                .zip(src_fun.ebb_args(src_target).iter())
            {
                call_renames.insert(*to, *from);
            }

            let mut renamed_nok_val = None;

            // Construct the closure environment for the continuation
            buf.clear();
            buf.push(ok_ret_cont);
            buf.push(err_ret_cont);
            for live in err_live.iter(&live.pool) {
                let renamed = call_renames.get(&live).cloned().unwrap_or(live);
                if renamed == nok_val {
                    renamed_nok_val = Some(live);
                    continue;
                }
                buf.push(val_map[&renamed]);
            }
            let env_idx = env_idx_gen.next();
            let env = b.op_make_closure_env(env_idx, &buf);

            // Bind closure for continuation
            let mut ident = src_fun.ident().clone();
            ident.lambda = Some((env_idx, 0));
            ident.arity = 1;
            let err_cont = b.op_bind_closure(ident, env);

            // Schedule exception edge for continuation generation
            needed_continuations.push(ContSite::EbbCall(src_branch, renamed_nok_val));

            // =======================
            // ==== Function call ====
            // =======================

            // TODO

            // Do not copy the current op, continue processing queue
            continue;
        }

        copy_op(src_fun, src_op, &mut b, &mut val_map, &mut ebb_map);

        // Add outgoing edges to processing queue
        if let Some(next_op) = src_fun.op_after(src_op) {
            to_process.push_back(next_op);
        }
        for branch in src_fun.op_branches(src_op) {
            let target = src_fun.ebb_call_target(*branch);
            let first_op = src_fun.ebb_first_op(target);
            to_process.push_back(first_op);
        }

    }

    println!("{}", b.function().to_text());

}

pub fn cps_transform(
    src_fun: &Function,
    env_idx_gen: &mut LambdaEnvIdxGenerator,
) {
    let live = src_fun.live_values();

    // Identify continuation sites
    let mut cont_sites = HashSet::new();
    for op in live.flow_live.keys() {
        let kind = src_fun.op_kind(*op);
        match kind {
            OpKind::Call { .. } => {
                cont_sites.insert(*op);
            },
            OpKind::Apply { .. } => {
                cont_sites.insert(*op);
            },
            _ => (),
        }
    }

    let mut generated = HashSet::new();
    let mut needed = Vec::new();

    let entry = src_fun.ebb_entry();
    gen_chunk(
        src_fun,
        ContSite::Op(src_fun.ebb_first_op(entry)),
        &cont_sites,
        &live,
        env_idx_gen,
        &mut needed,
        false,
    );

    while needed.len() > 0 {
        let site = needed.pop().unwrap();
        if let ContSite::EbbCall(call, val) = site {
            if generated.contains(&(call, val)) { continue }
            generated.insert((call, val));
        }

        gen_chunk(
            src_fun,
            site,
            &cont_sites,
            &live,
            env_idx_gen,
            &mut needed,
            true,
        );
    }

    // We now have every continuation site and the values that
    // need to be alive. This is all the information we need to
    // generate the lambda environments.

    // Start generation of new Eir functions

    //let mut ident = src_fun.ident().clone();
    //// First two argument become ok and throw continuations
    //ident.arity += 2;

    //let mut fun = Function::new(ident);
    //let mut b = FunctionBuilder::new(&mut fun);

    //let mut to_process = VecDeque::new();
    //let mut val_map = HashMap::new();
    //let mut ebb_map = HashMap::new();
    //let mut handled_ops = HashSet::new();

    //// Temp
    //let mut call_renames: HashMap<Value, Value> = HashMap::new();

    //// Add entry ebb
    //let entry_ebb = b.insert_ebb_entry();
    //let ok_cont = b.add_ebb_argument(entry_ebb);
    //let err_cont = b.add_ebb_argument(entry_ebb);

    //// Add entry ebb arguments
    //ebb_map.insert(src_fun.ebb_entry(), entry_ebb);
    //for arg in src_fun.ebb_args(src_fun.ebb_entry()) {
    //    let val = b.add_ebb_argument(entry_ebb);
    //    val_map.insert(*arg, val);
    //}

    //// Seed entry
    //to_process.push_back(src_fun.iter_op(src_fun.ebb_entry()).next().unwrap());

    //while to_process.len() > 0 {
    //    let src_op = to_process.pop_front().unwrap();

    //    if handled_ops.contains(&src_op) { continue; }
    //    handled_ops.insert(src_op);

    //    let src_ebb = src_fun.op_ebb(src_op);
    //    b.position_at_end(ebb_map[&src_ebb]);

    //    // If we hit a continuation site
    //    if cont_sites.contains(&src_op) {
    //        let kind = src_fun.op_kind(src_op);
    //        match kind {
    //            OpKind::Apply { .. } => (),
    //            OpKind::Call { .. } => (),
    //            _ => panic!(),
    //        }

    //        let mut buf = Vec::new();

    //        let writes = src_fun.op_writes(src_op);
    //        let ok_val = writes[0];
    //        let nok_val = writes[1];

    //        // Ok continuation
    //        let ok_live = &live.flow_live[&src_op];
    //        for live in ok_live.iter(&live.pool) {
    //            if live == ok_val {
    //                continue;
    //            }
    //            buf.push(val_map[&live]);
    //        }
    //        let env_idx = env_idx_gen.next();
    //        let env = b.op_make_closure_env(env_idx, &buf);

    //        let mut ident = src_fun.ident().clone();
    //        ident.lambda = Some((env_idx, 0));
    //        ident.arity = 1;
    //        let ok_cont = b.op_bind_closure(ident, env);

    //        // Throw continuation
    //        let src_branch = src_fun.op_branches(src_op)[0];
    //        let src_target = src_fun.ebb_call_target(src_branch);
    //        let err_live = &live.ebb_live[&src_target];

    //        call_renames.clear();
    //        for (from, to) in src_fun.ebb_call_args(src_branch).iter().zip(src_fun.ebb_args(src_target).iter()) {
    //            call_renames.insert(*to, *from);
    //        }

    //        buf.clear();
    //        for live in err_live.iter(&live.pool) {
    //            let renamed = call_renames.get(&live).cloned().unwrap_or(live);
    //            if renamed == nok_val {
    //                continue;
    //            }
    //            buf.push(val_map[&renamed]);
    //        }

    //        let env_idx = env_idx_gen.next();
    //        let env = b.op_make_closure_env(env_idx, &buf);

    //        let mut ident = src_fun.ident().clone();
    //        ident.lambda = Some((env_idx, 0));
    //        ident.arity = 1;
    //        let err_cont = b.op_bind_closure(ident, env);

    //        continue;
    //    }

    //    copy_op(src_fun, src_op, &mut b, &mut val_map, &mut ebb_map);

    //    if let Some(next_op) = src_fun.op_after(src_op) {
    //        to_process.push_back(next_op);
    //    }
    //    for branch in src_fun.op_branches(src_op) {
    //        let target = src_fun.ebb_call_target(*branch);
    //        let first_op = src_fun.ebb_first_op(target);
    //        to_process.push_back(first_op);
    //    }

    //}

}
