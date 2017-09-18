use std::io::Write;

use ::petgraph::Direction;
use ::petgraph::visit::EdgeRef;

use ::cfg::PatternCfg;
use ::pattern::PatternProvider;

const DOT_BREAK: &str = "<br align=\"left\" />";

fn format_label(label: &str) -> String {
    label.replace("{", "\\{").replace("}", "\\}").replace("\n", DOT_BREAK)
}

impl<P> PatternCfg<P> where P: PatternProvider {

    pub fn to_dot(&self, w: &mut Write) -> ::std::io::Result<()> {

        write!(w, "digraph g {{\n")?;
        write!(w, "node [labeljust=\"l\", shape=record, fontname=\"Courier New\"]\n")?;
        write!(w, "edge [fontname=\"Courier New\" ]\n\n")?;

        for index in self.graph.node_indices() {
            let node = &self.graph[index];
            println!("{:?}", node);

            let label = format_label(&format!("{:?}", node));
            write!(w, "node_{} [ label=<{}> ]\n", index.index(), label)?;

            for edge in self.graph.edges_directed(index, Direction::Outgoing) {
                let label = format_label(&format!("{:?}", edge.weight()));
                write!(w, "node_{} -> node_{} [ label=<{}> ]\n",
                       edge.source().index(), edge.target().index(), label)?;
            }

            write!(w, "\n")?;
        }

        //for (_, block) in &function.blocks {
        //    let block_name = block.label.name();

        //    write!(w, "blk_{} [ label=<{}|", block_name, block_name)?;

        //    for phi in &block.phi_nodes {
        //        if phi.dead {
        //            continue;
        //        }
        //        let fmt = format_label(&format!("${}, = PHI[{:?}]\n",
        //                                        phi.output.0, phi.inputs));
        //        write!(w, "{}", fmt)?;
        //    }

        //    for op in block.ops.iter() {
        //        if op.writes.len() > 0 {
        //            for write in &op.writes {
        //                write!(w, "${}, ", write.0)?;
        //            }
        //            write!(w, "= ")?;
        //        }

        //        let body = format_label(&format!("{:?} ", op.kind));
        //        write!(w, "{}", body)?;

        //        if op.reads.len() > 0 {
        //            write!(w, "read[")?;
        //            for read in op.reads.iter() {
        //                match *read {
        //                    SSASource::Register(reg) => write!(w, "${}, ", reg.0)?,
        //                    SSASource::Literal(ref lit) => write!(w, "{}, ", format_label(
        //                        &format!("{:?}", lit)))?,
        //                }
        //            }
        //            write!(w, "] ")?;
        //        }

        //        //write!(w, "r{:?}", op.r)?;
        //        //write!(w, " w{:?}", op.w)?;

        //        write!(w, "{}", DOT_BREAK)?;
        //    }

        //    //write!(w, "jumps[")?;
        //    //for label in block.jumps.iter() {
        //    //    write!(w, "{}, ", label.name())?;
        //    //}
        //    //write!(w, "] ")?;

        //    write!(w, "> ];\n")?;

        //    if let Some(label) = block.continuation {
        //        write!(w, "blk_{} -> blk_{} [ label=cont ];\n", block_name, label.name())?;
        //    }
        //    for (idx, arg) in block.jumps.iter().enumerate() {
        //        write!(w, "blk_{} -> blk_{} [ label={} ];\n", block_name, arg.name(), idx)?;
        //    }
        //    write!(w, "\n")?;
        //}

        write!(w, "}}\n")?;
        Ok(())
    }

}

