use crate::Function;
use super::printer::ToEirTextFun;

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

    write!(w, "digraph g {{\n")?;
    write!(w, "node [labeljust=\"l\", shape=record, fontname=\"Courier New\"]\n")?;
    write!(w, "edge [fontname=\"Courier New\" ]\n\n")?;

    let fun_name = format_label(&format!("{}", fun.ident()));
    write!(w, "entry [ label=<entry|fun: {}> ];\n",
           fun_name)?;
    write!(w, "entry -> blk_{};\n\n", fun.ebb_entry())?;

    let mut buf = Vec::new();

    for ebb in fun.iter_ebb() {
        write!(w, "blk_{} [ label=<", ebb)?;

        buf.clear();
        ebb.to_eir_text_fun(fun, 0, &mut buf).unwrap();
        let text = std::str::from_utf8(&buf).unwrap();
        let text = format_label(text);
        write!(w, "{}", text)?;

        write!(w, "> ];\n")?;

        for op in fun.iter_op(ebb) {
            for branch in fun.op_branches(op) {
                let target = fun.ebb_call_target(*branch);
                write!(w, "blk_{} -> blk_{};\n", ebb, target)?;
            }
        }

        write!(w, "\n")?;
    }

    write!(w, "}}\n")?;
    Ok(())
}
