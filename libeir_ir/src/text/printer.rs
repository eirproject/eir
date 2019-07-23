#![allow(clippy::write_with_newline)]

use std::io::Write;

use crate::{ Module, Function, FunctionIdent };
use crate::{ Block, Value };
use crate::OpKind;
use crate::ValueKind;
use crate::pattern::{ PatternContainer, PatternNode, PatternNodeKind };

use cranelift_entity::EntityRef;

use petgraph::visit::Dfs;

use std::collections::HashSet;

// Desired syntax:

// ```
// something {
//
// something:something/2@1.1 {
// entry(%0, %1):
//     %2, %3 = call woo:hoo(%0) except l0(%3);
//     tombstone %3;
//     jump l1(%2);
//
// l0(%4):
//     tombstone %2;
//     return_error %4;
//
// l1(%5):
//     return_ok %5;
//
// l2():
//     %6 = match_start on: %1, values: [%0] {
//         clause {
//             pattern [$0 = 0 | []];
//         };
//     };
//     jump l3();
// l3():
//     match_body %6 fail err() leaves [clause1()];
//
// err():
//     return_error a'nil';
//
// clause1():
//     %7 = case_calues [$0];
//     case_guard_ok %7;
//
// }
//
// }
// ```

pub trait EirAnnotator {
    fn annotate_function(&mut self, out: &mut String, fun: &Function);
    fn annotate_block(&mut self, out: &mut String, fun: &Function, block: Block);
}

//pub struct EirLiveValuesAnnotator {
//    live: Option<crate::fun::live::LiveValues>,
//}
//impl EirLiveValuesAnnotator {
//    pub fn new() -> Self {
//        EirLiveValuesAnnotator {
//            live: None,
//        }
//    }
//}
//impl EirAnnotator for EirLiveValuesAnnotator {
//    fn annotate_function(&mut self, out: &mut String, fun: &Function) {
//        self.live = Some(fun.live_values());
//    }
//    fn annotate_op(&mut self, out: &mut String, fun: &Function, op: Op) {
//        let live = self.live.as_ref().unwrap();
//        if let Some(l) = live.flow_live.get(&op) {
//            out.push_str(" live:");
//            println!("LVLEN: {:?}", live);
//            for var in l.iter(&live.pool) {
//                out.push_str(&format!(" %{}", var.index()));
//            }
//        }
//    }
//    fn annotate_ebb(&mut self, out: &mut String, fun: &Function, ebb: Ebb) {
//        let live = self.live.as_ref().unwrap();
//        let l = &live.ebb_live[&ebb];
//        out.push_str("yay!");
//    }
//}

#[derive(Default)]
pub struct ToEirTextContext {
    out_buf: String,
    annotators: Vec<Box<dyn EirAnnotator>>,
}

impl ToEirTextContext {
    pub fn new() -> Self {
        Self::default()
    }
    pub fn add_annotator<T>(&mut self, ann: T) where T: EirAnnotator + 'static {
        self.annotators.push(Box::new(ann));
    }
    pub fn annotate_function(&mut self, fun: &Function) {
        for ann in self.annotators.iter_mut() {
            ann.annotate_function(&mut self.out_buf, fun);
        }
    }
    pub fn annotate_block(&mut self, fun: &Function, block: Block) -> Option<String> {
        self.out_buf.clear();
        for ann in self.annotators.iter_mut() {
            ann.annotate_block(&mut self.out_buf, fun, block);
        }
        if !self.out_buf.is_empty() {
            Some(self.out_buf.to_string())
        } else {
            None
        }
    }
}

pub trait ToEirText {
    fn to_eir_text(&self, ctx: &mut ToEirTextContext, indent: usize, out: &mut dyn Write) -> std::io::Result<()>;
}
pub trait ToEirTextFun {
    fn to_eir_text_fun(&self, ctx: &mut ToEirTextContext, fun: &Function, indent: usize,
                       out: &mut dyn Write) -> std::io::Result<()>;
}

fn write_indent(out: &mut dyn Write, indent: usize) -> std::io::Result<()> {
    for _ in 0..indent {
        write!(out, "    ")?;
    }
    Ok(())
}

impl ToEirText for FunctionIdent {
    fn to_eir_text(&self, _ctx: &mut ToEirTextContext, indent: usize, out: &mut dyn Write) -> std::io::Result<()> {
        write_indent(out, indent)?;
        write!(out, "{}:{}/{}",
               self.module, self.name, self.arity)?;
        Ok(())
    }
}

pub fn print_constants(_ctx: &mut ToEirTextContext, _fun: &Function, _indent: usize, _out: &mut dyn Write) -> std::io::Result<()> {
    // TODO
    //let mut used_values = HashSet::new();
    //fun.used_values(&mut used_values);

    //let mut values: Vec<_> = used_values.iter().cloned().collect();
    //values.sort();

    //for value in values.iter() {
    //    let typ = fun.value(*value);
    //    match typ {
    //        ValueType::Constant(cons) => {
    //            write_indent(out, indent)?;
    //            write!(out, "%{} = ", value.index())?;
    //            cons.to_eir_text(ctx, indent+1, out)?;
    //            write!(out, ";\n")?;
    //        },
    //        ValueType::Variable => (),
    //    }
    //}

    Ok(())
}

impl ToEirText for Module {
    fn to_eir_text(&self, ctx: &mut ToEirTextContext, indent: usize, out: &mut dyn Write) -> std::io::Result<()> {
        let funs: Vec<_> = self.functions.keys().collect();
        // TODO sort
        //funs.sort();

        write_indent(out, indent)?;
        write!(out, "{} {{\n\n", self.name)?;

        for ident in funs.iter() {
            let fun = &self.functions[ident];
            fun.to_eir_text(ctx, indent+1, out)?;
            write!(out, "\n\n")?;
        }

        write_indent(out, indent)?;
        write!(out, "}}")?;

        Ok(())
    }
}

impl ToEirText for Function {
    fn to_eir_text(&self, ctx: &mut ToEirTextContext, indent: usize, out: &mut dyn Write) -> std::io::Result<()> {
        ctx.annotate_function(self);
        let ident = self.ident();

        write_indent(out, indent)?;
        write!(out, "{}/{} {{\n", ident.name, ident.arity)?;

        // Constants
        print_constants(ctx, self, indent+1, out)?;
        write!(out, "\n")?;

        // Blocks
        let block_graph = self.block_graph();
        let mut block_dfs = Dfs::new(&block_graph, self.block_entry());

        while let Some(block) = block_dfs.next(&block_graph) {
            block.to_eir_text_fun(ctx, self, indent+1, out)?;
            write!(out, "\n")?;
        }

        write_indent(out, indent)?;
        write!(out, "}}")?;

        Ok(())
    }
}

fn format_pattern(_ctx: &mut ToEirTextContext, pat: &PatternContainer, _indent: usize,
                  annotated_nodes: &HashSet<PatternNode>,
                  node: PatternNode, out: &mut dyn Write) -> std::io::Result<()> {
    if annotated_nodes.contains(&node) {
        write!(out, "n{} @ ", node.index())?;
    }
    match pat.node_kind(node) {
        PatternNodeKind::Wildcard => write!(out, "_")?,
        _ => write!(out, "?")?,
    }

    Ok(())
}

impl ToEirTextFun for Block {
    fn to_eir_text_fun(&self, ctx: &mut ToEirTextContext, fun: &Function,
                       indent: usize, out: &mut dyn Write)
                       -> std::io::Result<()>
    {

        write_indent(out, indent)?;
        write!(out, "{}(", self)?;
        format_value_list(fun.block_args(*self), fun, out)?;
        write!(out, "):\n")?;

        let args = fun.block_reads(*self);

        if let Some(kind) = fun.block_kind(*self) {
            write_indent(out, indent+1)?;
            match kind {
                OpKind::Case { clauses } => {
                    let clauses_num = clauses.len(&fun.pool.clause);

                    let values_start = 1 + (clauses_num * 2);

                    write!(out, "case ")?;
                    format_value(args[values_start], fun, out)?;
                    write!(out, " {{")?;
                    write!(out, "\n")?;

                    for clause_num in 0..clauses_num {
                        let clause = clauses.get(0, &fun.pool.clause).unwrap();
                        let clause_nodes = fun.pat().clause_root_nodes(clause);

                        let base = 1 + (2 * clause_num);
                        let guard = args[base];
                        let body = args[base + 1];

                        let mut annotated_nodes = HashSet::new();
                        for bind in fun.pat().clause_binds(clause) {
                            annotated_nodes.insert(*bind);
                        }

                        // Pattern body
                        write_indent(out, indent + 2)?;
                        write!(out, "(")?;
                        if !clause_nodes.is_empty() {
                            write!(out, "\n")?;
                        }

                        for node in clause_nodes {
                            write_indent(out, indent + 3)?;
                            format_pattern(ctx, fun.pat(), indent+3, &annotated_nodes, *node, out)?;
                            write!(out, "\n")?;
                        }

                        if !clause_nodes.is_empty() {
                            write_indent(out, indent + 2)?;
                        }
                        write!(out, ")")?;

                        // Guard
                        write!(out, " guard ")?;
                        format_value(guard, fun, out)?;
                        write!(out, "(return")?;
                        for bind in fun.pat().clause_binds(clause) {
                            write!(out, ", n{}", bind.index())?;
                        }
                        write!(out, ")")?;

                        // Body
                        write!(out, " => ")?;
                        format_value(body, fun, out)?;
                        write!(out, "(")?;
                        let mut first = true;
                        for bind in fun.pat().clause_binds(clause) {
                            if !first {
                                write!(out, ", ")?;
                            }
                            first = false;

                            write!(out, "n{}", bind.index())?;
                        }
                        write!(out, ")")?;

                        write!(out, "\n")?;
                    }

                    write_indent(out, indent + 2)?;
                    write!(out, "_ => ")?;
                    format_value(args[0], fun, out)?;
                    write!(out, "()")?;
                    write!(out, "\n")?;

                    write_indent(out, indent + 1)?;
                    write!(out, "}}")?;

                }
                OpKind::Call => {
                    format_value(args[0], fun, out)?;
                    write!(out, "(")?;
                    format_value_list(&args[1..], fun, out)?;
                    write!(out, ")")?;
                }
                OpKind::Intrinsic(name) => {
                    write!(out, "intrinsic {}(", name)?;
                    format_value_list(args, fun, out)?;
                    write!(out, ")")?;
                }
                _ => {
                    write!(out, "{:?}(", kind)?;
                    format_value_list(args, fun, out)?;
                    write!(out, ")")?;
                }
            }
        }


        Ok(())
    }
}

fn format_value(value: Value, fun: &Function, out: &mut dyn Write) -> std::io::Result<()> {
    match fun.value_kind(value) {
        ValueKind::Block(block) => write!(out, "{}", block)?,
        ValueKind::Const(cons) => {
            fun.cons().write(cons, out);
        },
        _ => write!(out, "%{}", value.index())?,
    }
    Ok(())
}

fn format_value_list(values: &[Value], fun: &Function,
                     out: &mut dyn Write) -> std::io::Result<()> {
    for (idx, value) in values.iter().enumerate() {
        if idx != 0 {
            write!(out, ", ")?;
        }
        format_value(*value, fun, out)?;
    }
    Ok(())
}
