#![allow(clippy::write_with_newline)]

use std::io::{ Write, Error as IoError };

use crate::{ Module, Function, FunctionIdent };
use crate::{ Block, Value };
use crate::{ OpKind, PrimOpKind, BinOp, MatchKind, BasicType, BinaryEntrySpecifier, Endianness, CallKind };
use crate::AtomTerm;
use crate::ValueKind;
use crate::pattern::{ PatternContainer, PatternNode, PatternNodeKind };

use cranelift_entity::EntityRef;

use std::collections::{HashSet, VecDeque, BTreeSet};

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
        let funs: Vec<_> = self.index_iter().collect();

        write_indent(out, indent)?;
        write!(out, "{} {{\n\n", AtomTerm(self.name().name))?;

        for idx in funs.iter() {
            let fun_def = &self[*idx];
            let fun = fun_def.function();
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
        write!(out, "{}/{} {{\n", AtomTerm(ident.name.name), ident.arity)?;

        // Constants
        print_constants(ctx, self, indent+1, out)?;
        write!(out, "\n")?;

        // Blocks
        let mut walked = BTreeSet::new();
        let mut to_walk = VecDeque::new();
        to_walk.push_back(self.block_entry());

        while let Some(block) = to_walk.pop_front() {
            if walked.contains(&block) { continue; }
            walked.insert(block);

            self.block_walk_nested_values::<_, Result<(), ()>>(block, &mut |v| {
                if let Some(inner) = self.value_block(v) {
                    to_walk.push_back(inner);
                }
                Ok(())
            }).unwrap();

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

fn get_value_list<'a>(fun: &'a Function, value: Value) -> Option<&'a [Value]> {
    if let Some(prim) = fun.value_primop(value) {
        match fun.primop_kind(prim) {
            crate::PrimOpKind::ValueList =>
                return Some(fun.primop_reads(prim)),
            _ => (),
        }
    }
    None
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

        fun.block_walk_nested_values::<_, IoError>(*self, &mut |value| {
            match fun.value_kind(value) {
                ValueKind::PrimOp(prim) => {
                    write_indent(out, indent+1)?;
                    write!(out, "%{} = ", value.index())?;

                    let reads = fun.primop_reads(prim);

                    match fun.primop_kind(prim) {
                        PrimOpKind::CaptureFunction => {
                            assert!(reads.len() == 3);
                            format_value(reads[0], fun, out)?;
                            write!(out, ":")?;
                            format_value(reads[1], fun, out)?;
                            write!(out, "/")?;
                            format_value(reads[2], fun, out)?;
                        }
                        PrimOpKind::ListCell => {
                            assert!(reads.len() == 2);
                            write!(out, "[")?;
                            format_value(reads[0], fun, out)?;
                            write!(out, " | ")?;
                            format_value(reads[1], fun, out)?;
                            write!(out, "]")?;
                        }
                        PrimOpKind::ValueList => {
                            write!(out, "<")?;
                            format_value_list(reads, fun, out)?;
                            write!(out, ">")?;
                        }
                        PrimOpKind::BinOp(BinOp::Equal) => {
                            assert!(reads.len() == 2);
                            format_value(reads[0], fun, out)?;
                            write!(out, " == ")?;
                            format_value(reads[1], fun, out)?;
                        }
                        PrimOpKind::Tuple => {
                            write!(out, "{{")?;
                            for (i, value) in reads.iter().enumerate() {
                                if i != 0 {
                                    write!(out, ", ")?;
                                }
                                format_value(*value, fun, out)?;
                            }
                            write!(out, "}}")?;
                        }
                        kind => {
                            write!(out, "{:?}", kind)?;
                            write!(out, "(")?;
                            format_value_list(reads, fun, out)?;
                            write!(out, ")")?;
                        },
                    }

                    write!(out, ";\n")?;
                }
                _ => (),
            }
            Ok(())
        })?;

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
                        write!(out, ");")?;

                        write!(out, "\n")?;
                    }

                    write_indent(out, indent + 2)?;
                    write!(out, "_ => ")?;
                    format_value(args[0], fun, out)?;
                    write!(out, ";")?;
                    write!(out, "\n")?;

                    write_indent(out, indent + 1)?;
                    write!(out, "}}")?;

                }
                OpKind::Match { branches } => {
                    let targets_opt = get_value_list(fun, args[0]);
                    let targets_one = &[args[0]];
                    let targets = targets_opt.unwrap_or(targets_one);

                    write!(out, "match ")?;
                    format_value(args[1], fun, out)?;
                    write!(out, " {{\n")?;

                    for ((kind, arg), target) in branches.iter()
                        .zip(args[2..].iter())
                        .zip(targets.iter())
                    {
                        write_indent(out, indent + 2)?;
                        match kind {
                            MatchKind::Value => {
                                write!(out, "value ")?;
                                format_value(*arg, fun, out)?;
                            }
                            MatchKind::ListCell => {
                                write!(out, "[]")?;
                            }
                            MatchKind::Wildcard => {
                                write!(out, "_")?;
                            }
                            MatchKind::Tuple(n) => {
                                write!(out, "{{}} arity {}", n)?;
                            }
                            MatchKind::Type(BasicType::Map) => {
                                write!(out, "type %{{}}")?;
                            }
                            MatchKind::Type(_) => {
                                unimplemented!()
                            }
                            MatchKind::MapItem => {
                                write!(out, "%{{ ")?;
                                format_value(*arg, fun, out)?;
                                write!(out, "}}")?;
                            }
                            MatchKind::Binary(spec) => {
                                write!(out, "binary ")?;

                                match spec {
                                    BinaryEntrySpecifier::Integer { signed, endianness, unit } => {
                                        if *signed {
                                            write!(out, "signed ")?;
                                        } else {
                                            write!(out, "unsigned ")?;
                                        }
                                        match *endianness {
                                            Endianness::Big => write!(out, "big ")?,
                                            Endianness::Little => write!(out, "little ")?,
                                            Endianness::Native => write!(out, "native ")?,
                                        }
                                        write!(out, "unit {} ", unit)?;
                                        write!(out, "size ")?;
                                        format_value(*arg, fun, out)?;
                                    }
                                    BinaryEntrySpecifier::Bytes { unit } => {
                                        write!(out, "unit {} ", unit)?;
                                        write!(out, "size ")?;
                                        format_value(*arg, fun, out)?;
                                    }
                                    _ => unimplemented!("{:?}", spec),
                                }

                            }
                        }
                        write!(out, " => ")?;
                        format_value(*target, fun, out)?;
                        write!(out, ";\n")?;
                    }

                    write_indent(out, indent + 1)?;
                    write!(out, "}}")?;
                }
                OpKind::Call(CallKind::ControlFlow) => {
                    format_value(args[0], fun, out)?;
                    write!(out, "(")?;
                    format_value_list(&args[1..], fun, out)?;
                    write!(out, ")")?;
                }
                OpKind::Call(CallKind::Function) => {
                    format_value(args[0], fun, out)?;
                    write!(out, "(")?;
                    format_value_list(&args[3..], fun, out)?;
                    write!(out, ") => ")?;
                    format_value(args[1], fun, out)?;
                    write!(out, " except ")?;
                    format_value(args[2], fun, out)?;
                }
                OpKind::Intrinsic(name) => {
                    write!(out, "intrinsic {}(", name)?;
                    format_value_list(args, fun, out)?;
                    write!(out, ")")?;
                }
                OpKind::Unreachable => {
                    write!(out, "unreachable")?;
                }
                OpKind::IfBool => {
                    match args.len() {
                        3 => {
                            write!(out, "if_bool ")?;
                            format_value(args[2], fun, out)?;
                            write!(out, " ")?;
                            format_value(args[0], fun, out)?;
                            write!(out, " ")?;
                            format_value(args[1], fun, out)?;
                        }
                        4 => {
                            write!(out, "if_bool ")?;
                            format_value(args[3], fun, out)?;
                            write!(out, " ")?;
                            format_value(args[0], fun, out)?;
                            write!(out, " ")?;
                            format_value(args[1], fun, out)?;
                            write!(out, " ")?;
                            format_value(args[2], fun, out)?;
                        }
                        _ => panic!(),
                    }
                }
                OpKind::TraceCaptureRaw => {
                    assert!(args.len() == 1);
                    write!(out, "trace_capture_raw ")?;
                    format_value(args[0], fun, out)?;
                }
                OpKind::UnpackValueList(n) => {
                    assert!(args.len() == 2);
                    write!(out, "unpack ")?;
                    format_value(args[1], fun, out)?;
                    write!(out, " arity {} => ", n)?;
                    format_value(args[0], fun, out)?;
                }
                _ => {
                    write!(out, "{:?}(", kind)?;
                    format_value_list(args, fun, out)?;
                    write!(out, ")")?;
                }
            }

            write!(out, ";")?;
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
