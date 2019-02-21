use std::fmt;
use super::PatternNode;

impl fmt::Display for PatternNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            PatternNode::Wildcard =>
                write!(f, "_")?,
            PatternNode::Bind(var, pat) =>
                write!(f, "({} = {})", var, pat)?,
            PatternNode::Atomic(lit) =>
                write!(f, "{:?}", lit)?,
            PatternNode::Tuple(nodes) => {
                write!(f, "{{")?;
                for node in nodes {
                    write!(f, "{}, ", node)?;
                }
                write!(f, "}}")?;
            },
            PatternNode::List(head_values, tail) => {
                write!(f, "[")?;
                for val in head_values {
                    write!(f, "{}, ", val)?;
                }
                write!(f, " | {}", tail)?;
                write!(f, "]")?;
            },
            PatternNode::Map(values) => {
                write!(f, "~{{")?;
                for (key_num, val) in values {
                    write!(f, "{}; {}, ", key_num, val)?;
                }
                write!(f, "}}~")?;
            },
            PatternNode::Binary(elems) => {
                write!(f, "#<")?;
                for elem in elems.iter() {
                    write!(f, "{}:{:?}, ", elem.node, elem.args)?;
                }
                write!(f, ">#")?;
            },
            //PatternNode::Binary(elems) => {
            //    write!(f, "#<")?;

            //    for elem in elems {
            //        write!(f, "#<{}>(", elem.0)?;
            //        for attr in &elem.1 {
            //            write!(f, "{}, ", attr)?;
            //        }
            //        write!(f, ")#")?;
            //    }

            //    write!(f, ">#")?;
            //},
        }
        Ok(())
    }
}
