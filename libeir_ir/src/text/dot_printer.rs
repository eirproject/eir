use crate::Function;
use super::printer::{ ToEirTextFun, ToEirTextContext };

use petgraph::visit::{ Dfs, IntoNeighbors };

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
pub fn function_to_dot(fun: &Function, w: &mut Write) -> ::std::io::Result<()> {
    let mut to_eir_ctx = ToEirTextContext::new();

    write!(w, "digraph g {{\n")?;
    write!(w, "node [labeljust=\"l\", shape=record, fontname=\"Courier New\"]\n")?;
    write!(w, "edge [fontname=\"Courier New\" ]\n\n")?;

    let fun_name = format_label(&format!("{}", fun.ident()));
    write!(w, "entry [ label=<entry|fun: {}> ];\n",
           fun_name)?;
    write!(w, "entry -> blk_{};\n\n", fun.block_entry())?;

    let mut buf = Vec::new();

    write!(w, "constants [ label=<")?;
    super::printer::print_constants(&mut to_eir_ctx, fun, 0, &mut buf)?;
    let text = std::str::from_utf8(&buf).unwrap();
    let text = format_label(text);
    write!(w, "{}", text)?;
    write!(w, "> ];\n")?;

    let block_graph = fun.block_graph();
    let mut block_dfs = Dfs::new(&block_graph, fun.block_entry());

    while let Some(block) = block_dfs.next(&block_graph) {
        write!(w, "blk_{} [ label=<", block)?;

        buf.clear();
        block.to_eir_text_fun(&mut to_eir_ctx, fun, 0, &mut buf).unwrap();
        let text = std::str::from_utf8(&buf).unwrap();
        let text = format_label(text);
        write!(w, "{}", text)?;

        write!(w, "> ];\n")?;

        for out in block_graph.neighbors(block) {
            write!(w, "blk_{} -> blk_{};\n", block, out)?;
        }

        write!(w, "\n")?;
    }

    write!(w, "}}\n")?;
    Ok(())
}
