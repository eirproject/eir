use std::collections::HashMap;

use inkwell::AddressSpace;
use inkwell::IntPredicate;
use inkwell::context::Context;
use inkwell::values::{ FunctionValue, ArrayValue, StructValue, PointerValue,
                       GlobalValue, AnyValueEnum, BasicValueEnum, PhiValue };
use inkwell::types::{ StructType, BasicType, BasicTypeEnum };
use inkwell::module::{ Module };
use inkwell::builder::Builder;

use num_traits::cast::ToPrimitive;

use crate::nif_types::NifTypes;
use eir::{ Function, FunctionIdent, SSAVariable, Source, ConstantTerm, AtomicTerm };
use eir::cfg::LabelN;
use eir::op::OpKind;

pub fn mangle_string(string: &str) -> String {
    string
        .replace("_", "__")
        .replace("+", "_p")
}

pub fn mangle_ident(ident: &FunctionIdent) -> String {
    let module_str = mangle_string(ident.module.as_str());
    let name_str = mangle_string(ident.name.as_str());
    format!(
        "GNIF{}_{}{}_{}{}_{}_{}",
        module_str.len(), module_str,
        name_str.len(), name_str,
        ident.arity,
        ident.lambda.map(|l| l.0.to_string()).unwrap_or("n".to_string()),
        ident.lambda.map(|l| l.1.to_string()).unwrap_or("n".to_string()),
    )
}

pub fn emit_read(context: &Context, module: &Module, builder: &Builder,
                 nif_refs: &NifTypes,
                 env: BasicValueEnum,
                 bindings: &HashMap<SSAVariable, BasicValueEnum>,
                 read: &Source) -> BasicValueEnum {
    match read {
        Source::Variable(ssa) => bindings[ssa],
        Source::Constant(constant) => {
            match constant {
                ConstantTerm::Atomic(AtomicTerm::Integer(num)) => {
                    let i64_type = context.i64_type();
                    let num_const = i64_type.const_int(
                        num.to_i64().unwrap() as u64, true);
                    let cs = builder.build_call(
                        nif_refs.enif_make_long,
                        &[env, num_const.into()],
                        "make_int64"
                    );
                    cs.try_as_basic_value().left().unwrap()
                }
                ConstantTerm::Atomic(AtomicTerm::Atom(val)) => {
                    let i8_type = context.i8_type();
                    let i8_ptr = i8_type.ptr_type(AddressSpace::Generic);
                    let string_const = crate::primitives::make_c_string_const(
                        context, module, val.as_str());
                    let string_const_ptr = string_const.as_pointer_value()
                        .const_cast(i8_ptr);

                    let i64_type = context.i64_type();
                    let string_len_val = i64_type.const_int(
                        val.as_str().bytes().len() as u64, true);

                    let cs = builder.build_call(
                        nif_refs.enif_make_atom_len,
                        &[env, string_const_ptr.into(), string_len_val.into()],
                        "make_atom_len"
                    );
                    cs.try_as_basic_value().left().unwrap()
                }
                _ => unimplemented!("{:?}", constant),
            }
        },
    }
}

pub fn emit_eir_fun(context: &Context, module: &Module,
                    nif_refs: &NifTypes, fun: &Function, fn_val: FunctionValue) {
    let name = mangle_ident(&fun.ident);

    let mut fn_args = vec![
        nif_refs.term_ptr_type,
        nif_refs.env_ptr_type,
    ];
    fn_args.extend((0..fun.ident.arity).map(|_| nif_refs.term_type));

    let bool_type = context.bool_type();
    let i64_type = context.i64_type();
    let i8_type = context.i8_type();
    let i8_ptr = i8_type.ptr_type(AddressSpace::Generic);

    assert!(fn_val.count_basic_blocks() == 0);
    //let fn_type = (nif_refs.bool_type).fn_type(&fn_args, false);
    //let fn_val = module.add_function(&name, fn_type, None);

    let fun_ret_term = fn_val.get_nth_param(0).unwrap();
    let env = fn_val.get_nth_param(1).unwrap();

    // Create all basic blocks
    let entry_label = fun.lir.entry;
    let mut basic_blocks = HashMap::new();
    basic_blocks.insert(entry_label, context.append_basic_block(&fn_val, "entry"));
    for node in fun.lir.graph.nodes() {
        if !basic_blocks.contains_key(&node.label) {
            basic_blocks.insert(
                node.label,
                context.append_basic_block(&fn_val, &format!("{}", node.label))
            );
        }
    }

    let builder = context.create_builder();

    //let basic_block = context.append_basic_block(&fn_val, "entry");

    let mut bindings: HashMap<SSAVariable, BasicValueEnum> = HashMap::new();
    let mut phis: HashMap<(LabelN, usize), PhiValue> = HashMap::new();

    for node_label in fun.lir.graph.reverse_post_order(entry_label) {
        let node = fun.lir.graph.node(node_label);
        let node_inner = node.inner.borrow();

        let basic_block = &basic_blocks[&node_label];
        builder.position_at_end(basic_block);

        for (idx, phi) in node_inner.phi_nodes.iter().enumerate() {
            let l_phi = builder.build_phi(i64_type, "");
            phis.insert((node.label, idx), l_phi);
            bindings.insert(phi.ssa, l_phi.as_basic_value());
        }

        //let string_const = crate::primitives::make_c_string_const(
        //    context, module, &format!("Label: {}\n", node_label));
        //let string_const_ptr = string_const.as_pointer_value()
        //    .const_cast(i8_ptr);
        //builder.build_call(nif_refs.printf, &[string_const_ptr.into()], "debug print");

        for op in node_inner.ops.iter() {
            match op.kind {
                OpKind::Arguments => {
                    assert!(op.reads.len() == 0);
                    assert!(op.writes.len() == fun.ident.arity);
                    for (idx, write) in op.writes.iter().enumerate() {
                        bindings.insert(
                            *write,
                            fn_val.get_nth_param(idx as u32 + 2).unwrap());
                    }
                },
                OpKind::Call { tail_call } => {
                    let all_resolved = op.reads.iter().take(2).all(|v| v.is_constant());
                    if all_resolved {
                        // Unwrap function name
                        let module_a = op.reads[0].constant().unwrap().atom().unwrap();
                        let name = op.reads[1].constant().unwrap().atom().unwrap();
                        let arity = op.reads.len() - 2;

                        // Construct EIR ident and mangle
                        let ident = FunctionIdent {
                            module: module_a,
                            name: name,
                            arity: arity,
                            lambda: None,
                        };
                        let name_mangled = mangle_ident(&ident);

                        // Make arguments, (ret_term, env, ..)
                        let ret_term_ptr = builder.build_alloca(
                            nif_refs.term_type, "return term");
                        let mut args = vec![
                            ret_term_ptr.into(),
                            env,
                        ];
                        args.extend(op.reads.iter().skip(2)
                                    .map(|r| emit_read(context, module, &builder,
                                                       nif_refs, env, &bindings, r)));

                        // Build function call
                        let fun = module.get_function(&name_mangled).expect(&name_mangled);
                        let call = builder.build_call(fun, &args, "funcall");

                        // Insert return term into bindings for both Ok and Throw
                        let ret_term = builder.build_load(ret_term_ptr, "return term");
                        if tail_call {
                            let bb_ok = context.append_basic_block(
                                &fn_val, "tail_call_ok");
                            let bb_exc = context.append_basic_block(
                                &fn_val, "tail_call_exc");
                            builder.build_conditional_branch(
                                call.try_as_basic_value().left().unwrap().into_int_value(),
                                &bb_ok,
                                &bb_exc
                            );

                            let b2 = context.create_builder();
                            b2.position_at_end(&bb_ok);
                            b2.build_store(
                                fun_ret_term.into_pointer_value(),
                                ret_term
                            );
                            b2.build_return(Some(&bool_type.const_int(1, false)));

                            b2.position_at_end(&bb_exc);
                            b2.build_store(
                                fun_ret_term.into_pointer_value(),
                                ret_term
                            );
                            b2.build_return(Some(&bool_type.const_int(0, false)));
                        } else {
                            bindings.insert(op.writes[0], ret_term);
                            bindings.insert(op.writes[1], ret_term);

                            builder.build_conditional_branch(
                                call.try_as_basic_value().left().unwrap().into_int_value(),
                                &basic_blocks[&node.outgoing[0].1],
                                &basic_blocks[&node.outgoing[1].1]
                            );
                        }

                    } else {
                        unimplemented!();
                    }
                },
                OpKind::EqualAtomic(ref atomic) => {
                    let arg = emit_read(context, module, &builder, nif_refs,
                                        env, &bindings, &op.reads[0]);
                    match atomic {
                        AtomicTerm::Integer(num) => {
                            let ret_num_ptr = builder.build_alloca(
                                i64_type, "return num");
                            let args = vec![
                                env,
                                arg,
                                ret_num_ptr.into(),
                            ];
                            let call = builder.build_call(nif_refs.enif_get_long,
                                                          &args, "funcall");

                            let bb_ok = context.append_basic_block(
                                &fn_val, "value_type_ok");
                            let ret_val = call.try_as_basic_value().left()
                                .unwrap().into_int_value();
                            builder.build_conditional_branch(
                                ret_val,
                                &bb_ok,
                                &basic_blocks[&node.outgoing[1].1],
                            );

                            let b2 = context.create_builder();
                            b2.position_at_end(&bb_ok);

                            let num_const = i64_type.const_int(
                                num.to_i64().unwrap() as u64, true);
                            let ret_num = b2.build_load(ret_num_ptr, "return num");
                            let cmp = b2.build_int_compare(
                                IntPredicate::EQ,
                                num_const,
                                ret_num.into_int_value(),
                                ""
                            );

                            b2.build_conditional_branch(
                                cmp,
                                &basic_blocks[&node.outgoing[0].1],
                                &basic_blocks[&node.outgoing[1].1],
                            );
                        }
                        _ => unimplemented!(),
                    }
                }
                OpKind::Jump => {
                    builder.build_unconditional_branch(
                        &basic_blocks[&node.outgoing[0].1]);
                }
                OpKind::ReturnOk => {
                    let arg = emit_read(context, module, &builder, nif_refs,
                                        env, &bindings, &op.reads[0]);
                    builder.build_store(
                        fun_ret_term.into_pointer_value(),
                        arg
                    );
                    builder.build_return(Some(&bool_type.const_int(1, false)));
                }
                OpKind::ReturnThrow => {
                    let arg = emit_read(context, module, &builder, nif_refs,
                                        env, &bindings, &op.reads[0]);
                    builder.build_store(
                        fun_ret_term.into_pointer_value(),
                        arg
                    );
                    builder.build_return(Some(&bool_type.const_int(0, false)));
                }
                OpKind::TombstoneSSA(_) => (),
                OpKind::Move => {
                    let arg = emit_read(context, module, &builder, nif_refs,
                                        env, &bindings, &op.reads[0]);
                    bindings.insert(op.writes[0], arg);
                }
                _ => {
                    println!("{:?}", op.kind);
                    unimplemented!();
                },
            }
        }

    }

    for node in fun.lir.graph.nodes() {
        let node_inner = node.inner.borrow();
        for (idx, phi) in node_inner.phi_nodes.iter().enumerate() {
            let l_phi = phis[&(node.label, idx)];
            for entry in phi.entries.iter() {
                let edge_label = entry.0;
                let ssa = &entry.1;
                let from = &basic_blocks[&fun.lir.graph.edge_from(edge_label)];
                //println!("Phi Incoming: {:?} {:?}", bindings[ssa], from);
                l_phi.add_incoming(&[(&bindings[ssa], from)]);
            }
        }
    }

}
