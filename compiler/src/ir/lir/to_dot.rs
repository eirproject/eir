use ::ir::FunctionDefinition;
use ::ir::lir::Source;

const DOT_BREAK: &str = "<br align=\"left\" />";

fn format_label(label: &str) -> String {
    label.replace("{", "\\{").replace("}", "\\}").replace("\n", DOT_BREAK)
}

use std::io::Write;
pub fn function_to_dot(function: &FunctionDefinition, w: &mut Write) -> ::std::io::Result<()> {
    let lir = function.lir_function.as_ref().unwrap();

    write!(w, "digraph g {{\n")?;
    write!(w, "node [labeljust=\"l\", shape=record, fontname=\"Courier New\"]\n")?;
    write!(w, "edge [fontname=\"Courier New\" ]\n\n")?;

    let fun_name = format_label(&format!("{}", function.ident));
    let args: Vec<_> = function.hir_fun.args.iter()
        .map(|a| (a.var.clone(), a.ssa)).collect();
    write!(w, "entry [ label=<entry|fun: {} free: {:?} write[{:?}]> ];\n",
           fun_name, function.visibility, args)?;
    write!(w, "entry -> blk_{};\n\n", lir.entry())?;

    for block_idx in lir.labels_iter() {
        let block_name = block_idx;
        let block = lir.block(block_idx);

        write!(w, "blk_{} [ label=<{}|", block_name, block_name)?;

        for phi in &block.phi_nodes {
            //if phi.dead {
            //    continue;
            //}
            let fmt = format_label(&format!("{:?}, = PHI[{:?}]\n",
                                            phi.ssa, phi.entries));
            write!(w, "{}", fmt)?;
        }

        for op in block.ops.iter() {
            if op.writes.len() > 0 {
                for write in &op.writes {
                    write!(w, "{:?}, ", write)?;
                }
                write!(w, "= ")?;
            }

            match &op.kind {
                ::ir::lir::OpKind::Case { ref vars, ref clauses, ref value_vars } => {
                    write!(w, "Case \\{{{}", DOT_BREAK)?;

                    let vars_fmt = format_label(&format!("{:?} ", vars));
                    write!(w, "  match on: {}{}", vars_fmt, DOT_BREAK)?;

                    let value_vars_fmt = format_label(&format!("{:?} ", value_vars));
                    write!(w, "  value literals: {}{}", value_vars_fmt, DOT_BREAK)?;

                    for clause in clauses {
                        write!(w, "  clauses:{}", DOT_BREAK)?;
                        for pattern in &clause.patterns {
                            let binds_fmt = format_label(&format!("{:?}", pattern.binds));
                            let clause_fmt = format_label(&format!("{} ", pattern.node));
                            write!(w, "    pattern:{}", DOT_BREAK)?;
                            write!(w, "      {}{}", binds_fmt, DOT_BREAK)?;
                            write!(w, "      {}{}", clause_fmt, DOT_BREAK)?;
                        }
                    }

                    write!(w, "\\}} ")?;
                },
                kind => {
                    let body = format_label(&format!("{:?} ", kind));
                    write!(w, "{}", body)?;
                },
            }

            if op.reads.len() > 0 {
                write!(w, "read[")?;
                for read in op.reads.iter() {
                    match *read {
                        Source::Variable(reg) =>
                            write!(w, "{}", format_label(&format!("{:?}, ", reg)))?,
                        //Source::Literal(ref lit) => write!(w, "{}, ", format_label(
                        //    &format!("{:?}", lit)))?,
                        Source::Constant(ref lit) =>
                            write!(w, "{}", format_label(&format!("{:?}, ", lit)))?,
                    }
                }
                write!(w, "] ")?;
            }

            //write!(w, "r{:?}", op.r)?;
            //write!(w, " w{:?}", op.w)?;

            write!(w, "{}", DOT_BREAK)?;
        }

        //write!(w, "jumps[")?;
        //for label in block.jumps.iter() {
        //    write!(w, "{}, ", label.name())?;
        //}
        //write!(w, "] ")?;

        write!(w, "> ];\n")?;

        //if let Some(label) = block.continuation {
        //    write!(w, "blk_{} -> blk_{} [ label=cont ];\n", block_name, label.name())?;
        //}

        for (idx, edge_id) in lir.jumps_iter(block_idx).enumerate() {
            let edge = lir.edge_target(edge_id);
            use ::petgraph::visit::EdgeRef;
            write!(w, "blk_{} -> blk_L{} [ label={} ];\n", block_name, edge.0.index(), idx)?;
        }
        write!(w, "\n")?;
    }

    write!(w, "}}\n")?;
    Ok(())
}
