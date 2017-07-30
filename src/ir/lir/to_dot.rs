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
    write!(w, "entry -> blk_{};\n\n", lir.entry.0)?;

    for block in &lir.basic_blocks {
        let block_name = block.label.0;

        write!(w, "blk_{} [ label=<{}|", block_name, block_name)?;

        //for phi in &block.phi_nodes {
        //    if phi.dead {
        //        continue;
        //    }
        //    let fmt = format_label(&format!("${}, = PHI[{:?}]\n",
        //                                    phi.output.0, phi.inputs));
        //    write!(w, "{}", fmt)?;
        //}

        for op in block.ops.iter() {
            if op.writes.len() > 0 {
                for write in &op.writes {
                    write!(w, "%{}, ", write.0)?;
                }
                write!(w, "= ")?;
            }

            let body = format_label(&format!("{:?} ", op.kind));
            write!(w, "{}", body)?;

            if op.reads.len() > 0 {
                write!(w, "read[")?;
                for read in op.reads.iter() {
                    match *read {
                        Source::Variable(reg) => write!(w, "%{}, ", reg.0)?,
                        //Source::Literal(ref lit) => write!(w, "{}, ", format_label(
                        //    &format!("{:?}", lit)))?,
                        Source::Constant(ref lit) => write!(w, "{:?}, ", lit)?,
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
        for (idx, arg) in block.jumps.iter().enumerate() {
            write!(w, "blk_{} -> blk_{} [ label={} ];\n", block_name, arg.0, idx)?;
        }
        write!(w, "\n")?;
    }

    write!(w, "}}\n")?;
    Ok(())
}
