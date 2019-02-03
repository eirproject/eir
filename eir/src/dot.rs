use ::ir::FunctionDefinition;
use ::eir::Source;
use ::eir::op::OpKind;

const DOT_BREAK: &str = "<br align=\"left\" />";

fn format_label(label: &str) -> String {
    label
        .replace("{", "\\{")
        .replace("}", "\\}")
        .replace("|", "\\|")
        .replace(">", "&lt;")
        .replace("<", "&gt;")
        .replace("&", "&amp;")
        .replace("\"", "&quot;")
        .replace("\n", DOT_BREAK)
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

    for block_container in lir.graph.nodes() {
        let block_name = block_container.label;
        let block = block_container.inner.borrow();

        write!(w, "blk_{} [ label=<{}|", block_name, block_name)?;

        for phi in &block.phi_nodes {
            let fmt = format_label(&format!("{:?}, = PHI[", phi.ssa));
            write!(w, "{}{}\n", fmt, DOT_BREAK)?;
            for entry in phi.entries.iter() {
                let fmt = format_label(&format!("  {:?},", entry));
                write!(w, "{}{}", fmt, DOT_BREAK)?;
            }
            write!(w, "] {}", DOT_BREAK)?;
        }

        for op in block.ops.iter() {
            if op.writes.len() > 0 {
                for write in &op.writes {
                    write!(w, "{:?}, ", write)?;
                }
                write!(w, "= ")?;
            }

            match &op.kind {
                OpKind::CaseStart { ref vars, ref clauses, ref value_vars } => {
                    write!(w, "CaseStart \\{{{}", DOT_BREAK)?;

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
                OpKind::CaptureNamedFunction(ref ident) => {
                    write!(w, "CaptureNamedFunction({})", ident)?;
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
                        Source::Constant(ref lit) =>
                            write!(w, "{}", format_label(&format!("{:?}, ", lit)))?,
                    }
                }
                write!(w, "] ")?;
            }

            write!(w, "{}", DOT_BREAK)?;
        }

        write!(w, "> ];\n")?;

        for (idx, (_edge_id, dst)) in block_container.outgoing.iter().enumerate() {
            write!(w, "blk_{} -> blk_{} [ label={} ];\n", block_name, dst, idx)?;
        }
        write!(w, "\n")?;
    }

    write!(w, "}}\n")?;
    Ok(())
}
