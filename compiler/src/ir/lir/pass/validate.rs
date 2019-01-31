use ::std::collections::HashSet;
use ::ir::lir::{ FunctionCfg, Source, OpKind };
use ::ir::FunctionIdent;

pub fn validate(ident: &FunctionIdent, cfg: &FunctionCfg) {

    validate_proper_ssa(ident, cfg);

}

fn validate_proper_ssa(ident: &FunctionIdent, cfg: &FunctionCfg) {

    {
        let entry_container = cfg.graph.node(cfg.entry());
        let entry = entry_container.inner.borrow();

        // Check entry invariants
        if entry.phi_nodes.len() != 0 {
            println!("Entry block can't have PHI nodes");
        }

        if let OpKind::Arguments = entry.ops[0].kind {
            if entry.ops[0].writes.len() != ident.arity as usize {
                println!("Arity of ident and OpKind::Arguments does not match");
            }
            if ident.lambda.is_some() {
                if let OpKind::UnpackEnv = entry.ops[1].kind {
                } else {
                    println!("Entry block of lambda function must have OpKind::UnpackEnv as its second argument");;
                }
            }

        } else {
            println!("Entry block must start with OpKind::Arguments");
        }
    }

    // Collect all ssa variables and check for double assigns
    let mut assigns = HashSet::new();

    for block_container in cfg.graph.nodes() {
        let block = block_container.inner.borrow();
        for phi in block.phi_nodes.iter() {
            if assigns.contains(&phi.ssa) {
                println!("Double assign of {:?}", phi.ssa);
            }
            assigns.insert(phi.ssa);
        }

        for op in block.ops.iter() {
            for write in op.writes.iter() {
                if assigns.contains(write) {
                    println!("Double assign of {:?}", write);
                }
                assigns.insert(*write);
            }
        }
    }

    // Check for usage of unassigned
    for block_container in cfg.graph.nodes() {
        let block = block_container.inner.borrow_mut();
        for phi in block.phi_nodes.iter() {
            for &(_label, ref src) in phi.entries.iter() {
                if let Source::Variable(ref ssa) = src {
                    if !assigns.contains(&ssa) {
                        println!("Use of unassigned {:?}", ssa);
                    }
                }
            }
        }

        for op in block.ops.iter() {
            for read in op.reads.iter() {
                if let Source::Variable(ref ssa) = *read {
                    if !assigns.contains(&ssa) {
                        println!("Use of unassigned {:?}", ssa);
                    }
                }
            }
        }
    }

    // Check that all blocks strictly end in terminators
    for block_container in cfg.graph.nodes() {
        let block = block_container.inner.borrow_mut();
        if block.ops.len() == 0 {
            println!("{}: Empty block", block_container.label);
        } else {
            for op in block.ops[0..(block.ops.len() - 1)].iter() {
                if let Some(_) = op.kind.num_jumps() {
                    println!("{}: OP in block body can't be terminator",
                             block_container.label);
                    println!("    {:?}", op.kind);
                }
            }
            let last = block.ops.last().unwrap();
            let last_nj = last.kind.num_jumps();
            if let Some(jumps_num) =  last_nj {
                let actual_jumps_num = block_container.outgoing.len();
                if jumps_num != actual_jumps_num {
                    println!("{}: OP must have {} jumps, has {}",
                             block_container.label, jumps_num, actual_jumps_num);
                    println!("    {:?}", last.kind);
                }
            } else {
                println!("{}: OP at end of block must be terminator",
                         block_container.label);
                println!("    {:?}", last.kind);
            }
        }
    }

}
