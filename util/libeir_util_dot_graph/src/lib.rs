#![feature(allocator_api)]

use std::fmt::Display;
use std::fmt::{Error, Write};
use std::path::Path;

//mod tablegen;

const DOT_BREAK: &str = "<br align=\"left\" />";

fn format_label(label: &str, out: &mut String) {
    for c in label.chars() {
        match c {
            '{' => out.push_str("\\{"),
            '}' => out.push_str("\\}"),
            '|' => out.push_str("\\|"),
            '>' => out.push_str("&gt;"),
            '<' => out.push_str("&lt;"),
            '&' => out.push_str("&amp;"),
            '"' => out.push_str("&quot;"),
            '\n' => out.push_str(DOT_BREAK),
            c => out.push(c),
        }
    }
}

pub trait NodeId {
    fn make_id(&self, out: &mut String);
}
impl NodeId for &str {
    fn make_id(&self, out: &mut String) {
        out.push_str(self);
    }
}

/// Newtype that implements NodeId for any Display type
#[derive(Copy, Clone)]
pub struct DisplayNid<I: Display>(pub I);
impl<I: Display> NodeId for DisplayNid<I> {
    fn make_id(&self, out: &mut String) {
        write!(out, "{}", self.0).unwrap();
    }
}

/// Adds a prefix to a NodeId
#[derive(Copy, Clone)]
pub struct PrefixedNid<P: Display, I: NodeId>(pub P, pub I);
impl<P: Display, I: NodeId> NodeId for PrefixedNid<P, I> {
    fn make_id(&self, out: &mut String) {
        write!(out, "{}_", self.0).unwrap();
        self.1.make_id(out);
    }
}

/// References a subpart of a node
#[derive(Copy, Clone)]
pub struct SubNid<I: NodeId, S: Display>(pub I, pub S);
impl<I: NodeId, S: Display> NodeId for SubNid<I, S> {
    fn make_id(&self, out: &mut String) {
        self.0.make_id(out);
        write!(out, ":{}", self.1).unwrap();
    }
}

pub trait ContentFormatting {
    fn make_raw(&self, out: &mut String);
}
impl ContentFormatting for &str {
    fn make_raw(&self, out: &mut String) {
        format_label(self, out);
    }
}

pub trait Content {
    fn make_raw(&self, out: &mut String);
}
impl Content for &str {
    fn make_raw(&self, out: &mut String) {
        format_label(self, out);
    }
}

pub struct StructuredNode {}
impl StructuredNode {
    pub fn text<F: ContentFormatting>(&mut self, _content: F) {}
    pub fn vertical(&mut self, _cells: &[()]) {}
    pub fn horizontal(&mut self, _cells: &[()]) {}
}

pub struct GraphPrinter<O: Write> {
    out: O,
    id_buf: Option<String>,
    error: Result<(), Error>,
}

impl GraphPrinter<String> {
    pub fn new() -> Self {
        Self::with_out(String::new())
    }

    pub fn finish_run_dot(self, out: &Path) {
        let dot_res = self.finish().unwrap();

        let dir = std::env::temp_dir();

        let mut dot_path = dir.clone();
        dot_path.push("out.dot");
        {
            let mut dot_file = std::fs::File::create(&dot_path).unwrap();
            use std::io::Write;
            dot_file.write(dot_res.as_bytes()).unwrap();
        }

        let ext = out.extension().unwrap();

        use std::process::Command;
        let res = Command::new("dot")
            .args(&[
                "-o",
                out.to_str().unwrap(),
                "-T",
                ext.to_str().unwrap(),
                dot_path.to_str().unwrap(),
            ])
            .output()
            .expect("failed to execute dot");

        if !res.status.success() {
            println!("status: {}", res.status);
            println!("stdout: {}", std::str::from_utf8(&res.stdout).unwrap());
            println!("stderr: {}", std::str::from_utf8(&res.stderr).unwrap());
            panic!();
        }
    }
}

impl<O: Write> GraphPrinter<O> {
    pub fn with_out(out: O) -> Self {
        let mut g = GraphPrinter {
            out,
            id_buf: Some(String::new()),
            error: Ok(()),
        };
        g.w(|w| {
            write!(w, "digraph g {{\n")?;
            write!(
                w,
                "node [labeljust=\"l\", shape=record, fontname=\"Courier New\"]\n"
            )?;
            write!(w, "edge [fontname=\"Courier New\" ]\n\n")?;
            Ok(())
        });
        g
    }

    pub fn finish(mut self) -> Result<O, Error> {
        self.w(|w| {
            write!(w, "}}\n")?;
            Ok(())
        });
        let error = self.error;
        let out = self.out;
        error.map(|_| out)
    }

    pub fn node<I>(&mut self, id: I, label: &str)
    where
        I: NodeId,
    {
        let mut id_buf = self.id_buf.take().unwrap();

        id_buf.clear();
        id.make_id(&mut id_buf);
        self.w(|w| {
            write!(w, "{} [ label=<", &id_buf)?;
            Ok(())
        });

        id_buf.clear();
        format_label(label, &mut id_buf);
        self.w(|w| {
            write!(w, "{}> ];\n", &id_buf)?;
            Ok(())
        });

        self.id_buf = Some(id_buf);
    }

    pub fn edge<I1, I2>(&mut self, from: I1, to: I2, label: &str)
    where
        I1: NodeId,
        I2: NodeId,
    {
        let mut id_buf = self.id_buf.take().unwrap();

        id_buf.clear();
        from.make_id(&mut id_buf);
        self.w(|w| {
            write!(w, "{} -> ", &id_buf)?;
            Ok(())
        });

        id_buf.clear();
        to.make_id(&mut id_buf);
        self.w(|w| {
            write!(w, "{} ", &id_buf)?;
            Ok(())
        });

        id_buf.clear();
        format_label(label, &mut id_buf);
        self.w(|w| {
            write!(w, "[ label=<{}> ];\n", id_buf)?;
            Ok(())
        });

        self.id_buf = Some(id_buf);
    }

    fn w<F>(&mut self, f: F)
    where
        F: FnOnce(&mut O) -> Result<(), Error>,
    {
        if self.error.is_ok() {
            self.error = f(&mut self.out);
        }
    }
}

//#[allow(clippy::write_with_newline)]
//pub fn function_to_dot(fun: &Function, w: &mut dyn Write) -> ::std::io::Result<()> {
//    let mut to_eir_ctx = ToEirTextContext::new();
//
//    write!(w, "digraph g {{\n")?;
//    write!(w, "node [labeljust=\"l\", shape=record, fontname=\"Courier New\"]\n")?;
//    write!(w, "edge [fontname=\"Courier New\" ]\n\n")?;
//
//    let fun_name = format_label(&format!("{}", fun.ident()));
//    write!(w, "entry [ label=<entry|fun: {}> ];\n",
//           fun_name)?;
//    write!(w, "entry -> blk_{};\n\n", fun.block_entry())?;
//
//    let mut buf = Vec::new();
//
//    write!(w, "constants [ label=<")?;
//    super::printer::print_constants(&mut to_eir_ctx, fun, 0, &mut buf)?;
//    let text = std::str::from_utf8(&buf).unwrap();
//    let text = format_label(text);
//    write!(w, "{}", text)?;
//    write!(w, "> ];\n")?;
//
//    let block_graph = fun.block_graph();
//    let mut block_dfs = Dfs::new(&block_graph, fun.block_entry());
//
//    while let Some(block) = block_dfs.next(&block_graph) {
//        write!(w, "blk_{} [ label=<", block)?;
//
//        buf.clear();
//        block.to_eir_text_fun(&mut to_eir_ctx, fun, 0, &mut buf).unwrap();
//        let text = std::str::from_utf8(&buf).unwrap();
//        let text = format_label(text);
//        write!(w, "{}", text)?;
//
//        write!(w, "> ];\n")?;
//
//        for out in block_graph.neighbors(block) {
//            write!(w, "blk_{} -> blk_{};\n", block, out)?;
//        }
//
//        write!(w, "\n")?;
//    }
//
//    write!(w, "}}\n")?;
//    Ok(())
//}

#[cfg(test)]
mod test {
    use crate::{GraphPrinter, NodeId};
    use std::path::Path;

    #[test]
    fn basic() {
        let mut p = GraphPrinter::new();
        p.node("woo", "Something something");
        p.node("hoo", "Something else");
        p.edge("woo", "hoo", "Some edge");
        let out = p.finish().unwrap();

        panic!("{}", out);
    }

    #[test]
    fn basic_to_file() {
        let mut p = GraphPrinter::new();
        p.node("woo", "Something something");
        p.node("hoo", "Something else");
        p.edge("woo", "hoo", "Some edge");
        p.finish_run_dot(Path::new("something.png"));

        std::fs::remove_file(Path::new("something.png")).unwrap();
    }
}
