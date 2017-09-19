use std::io::Write;

use ::petgraph::Direction;
use ::petgraph::visit::EdgeRef;

use ::cfg::{ PatternCfg, CfgNodeKind };
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
            write!(w, "node_{} [ label=<{}: {}", index.index(), index.index(), label)?;

            if let CfgNodeKind::Leaf(leaf_id) = *node {
                let bindings = &self.leaf_bindings[&self.leaves[&leaf_id]];
                write!(w, "{} {}", DOT_BREAK, format_label(&format!("{:#?}", bindings)))?;
            }

            write!(w, "> ]\n")?;

            for edge in self.graph.edges_directed(index, Direction::Outgoing) {
                let label = format_label(&format!("{:?}", edge.weight()));
                write!(w, "node_{} -> node_{} [ label=<{}> ]\n",
                       edge.source().index(), edge.target().index(), label)?;
            }

            write!(w, "\n")?;
        }

        write!(w, "}}\n")?;
        Ok(())
    }

}

