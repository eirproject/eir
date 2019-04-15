use pretty::{ Doc, DocBuilder, DocAllocator, Arena as DocArena };

use std::io::Write;

use matches::assert_matches;

use crate::Module;
use crate::{ Function, FunctionIdent, Atom, Value };
use crate::op::{ OpKind, ComparisonOperation, CallType };
use crate::{ Ebb, Op, EbbCall };
use crate::fun::ValueType;
use crate::pattern::PatternNode;
use crate::{ AtomicTerm, ConstantTerm };

use cranelift_entity::EntityRef;

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

pub trait ToEirText {
    fn to_eir_text(&self, indent: usize, out: &mut Write) -> std::io::Result<()>;
}
pub trait ToEirTextFun {
    fn to_eir_text_fun(&self, fun: &Function, indent: usize,
                       out: &mut Write) -> std::io::Result<()>;
}

fn write_indent(out: &mut Write, indent: usize) -> std::io::Result<()> {
    for _ in 0..indent {
        write!(out, "    ")?;
    }
    Ok(())
}

impl ToEirText for FunctionIdent {
    fn to_eir_text(&self, indent: usize, out: &mut Write) -> std::io::Result<()> {
        write_indent(out, indent)?;
        if let Some((env_num, fun_num)) = self.lambda {
            write!(out, "{}:{}@{}.{}/{}",
                   self.module, self.name, env_num.index(),
                   fun_num, self.arity)?;
        } else {
            write!(out, "{}:{}/{}",
                   self.module, self.name, self.arity)?;
        }
        Ok(())
    }
}

impl ToEirText for AtomicTerm {
    fn to_eir_text(&self, indent: usize, out: &mut Write) -> std::io::Result<()> {
        match self {
            AtomicTerm::Atom(atom) => {
                write!(out, "a\"{}\"", atom)?;
            },
            AtomicTerm::Integer(int) => {
                write!(out, "{}", int)?;
            },
            AtomicTerm::Nil => {
                write!(out, "[]")?;
            },
            _ => unimplemented!("{:?}", self),
        }
        Ok(())
    }
}

impl ToEirText for ConstantTerm {
    fn to_eir_text(&self, indent: usize, out: &mut Write) -> std::io::Result<()> {
        match self {
            ConstantTerm::Atomic(atomic) => atomic.to_eir_text(indent, out)?,
            ConstantTerm::List(head, tail) => {
                write!(out, "[")?;
                for (idx, entry) in head.iter().enumerate() {
                    if idx != 0 {
                        write!(out, ", ")?;
                    }
                    entry.to_eir_text(indent, out)?;
                }
                write!(out, " | ")?;
                tail.to_eir_text(indent, out)?;
                write!(out, "]")?;
            },
        }
        Ok(())
    }
}

pub fn print_constants(fun: &Function, indent: usize, out: &mut Write) -> std::io::Result<()> {
    let mut used_values = HashSet::new();
    fun.used_values(&mut used_values);

    let mut values: Vec<_> = used_values.iter().cloned().collect();
    values.sort();

    for value in values.iter() {
        let typ = fun.value(*value);
        match typ {
            ValueType::Constant(cons) => {
                write_indent(out, indent)?;
                write!(out, "%{} = ", value.index())?;
                cons.to_eir_text(indent+1, out)?;
                write!(out, ";\n")?;
            },
            ValueType::Variable => (),
        }
    }

    Ok(())
}

impl ToEirText for Module {
    fn to_eir_text(&self, indent: usize, out: &mut Write) -> std::io::Result<()> {
        let mut funs: Vec<_> = self.functions.keys().collect();
        funs.sort();

        write_indent(out, indent)?;
        write!(out, "{} {{\n\n", self.name)?;

        for ident in funs.iter() {
            let fun = &self.functions[ident];
            fun.to_eir_text(indent+1, out)?;
            write!(out, "\n\n")?;
        }

        write_indent(out, indent)?;
        write!(out, "}}")?;

        Ok(())
    }
}

impl ToEirText for Function {
    fn to_eir_text(&self, indent: usize, out: &mut Write) -> std::io::Result<()> {
        let ident = self.ident();

        write_indent(out, indent)?;
        if let Some((env_num, fun_num)) = ident.lambda {
            write!(out, "{}@{}.{}/{} {{\n",
                   ident.name, env_num.index(), fun_num, ident.arity)?;
        } else {
            write!(out, "{}/{} {{\n", ident.name, ident.arity)?;
        }

        // Constants
        print_constants(self, indent+1, out)?;
        write!(out, "\n")?;

        // EBBs
        for ebb in self.iter_ebb() {
            ebb.to_eir_text_fun(self, indent+1, out)?;
            write!(out, "\n")?;
        }

        write_indent(out, indent)?;
        write!(out, "}}")?;

        Ok(())
    }
}

fn format_value(value: Value, fun: &Function, out: &mut Write) -> std::io::Result<()> {
    write!(out, "%{}", value.index())?;
    Ok(())
}

fn format_value_list(values: &[Value], fun: &Function,
                     out: &mut Write) -> std::io::Result<()> {
    for (idx, value) in values.iter().enumerate() {
        if idx != 0 {
            write!(out, ", ")?;
        }
        write!(out, "%{}", value.index())?;
    }
    Ok(())
}

impl ToEirTextFun for EbbCall {
    fn to_eir_text_fun(&self, fun: &Function, indent: usize, out: &mut Write) -> std::io::Result<()> {
        write!(out, "B{}(", fun.ebb_call_target(*self).index())?;
        format_value_list(fun.ebb_call_args(*self), fun, out)?;
        write!(out, ")")?;
        Ok(())
    }
}

fn format_ebb_label(ebb: Ebb, fun: &Function, out: &mut Write) -> std::io::Result<()> {
    write!(out, "B{}", ebb.index())?;
    let args = fun.ebb_args(ebb);
    if args.len() > 0 {
        write!(out, "(")?;
        for (idx, arg) in args.iter().enumerate() {
            if idx != 0 {
                write!(out, ", ")?;
            }
            write!(out, "%{}", arg.index())?;
        }
        write!(out, ")")?;
    }
    write!(out, ":\n")?;
    Ok(())
}

fn format_branches(calls: &[EbbCall], fun: &Function, indent: usize,
                   out: &mut Write) -> std::io::Result<()> {
    for (idx, ebb_call) in calls.iter().enumerate() {
        if idx != 0 {
            write!(out, ", ")?;
        }
        ebb_call.to_eir_text_fun(fun, indent, out)?;
    }
    Ok(())
}

impl ToEirText for PatternNode {
    fn to_eir_text(&self, indent: usize, out: &mut Write) -> std::io::Result<()> {
        match self {
            PatternNode::Atomic(atomic) => {
                atomic.to_eir_text(indent, out)?;
            },
            PatternNode::Wildcard => {
                write!(out, "_")?;
            },
            PatternNode::Assign(assign, inner) => {
                write!(out, "A{} = (", assign.0)?;
                inner.to_eir_text(indent, out)?;
                write!(out, ")")?;
            },
            PatternNode::List(head, tail) => {
                write!(out, "[")?;

                for (idx, elem) in head.iter().enumerate() {
                    if idx != 0 {
                        write!(out, ", ")?;
                    }
                    elem.to_eir_text(indent, out)?;
                }

                write!(out, " | ")?;
                tail.to_eir_text(indent, out)?;
                write!(out, "]")?;
            },
            PatternNode::Map(entries) => {
                write!(out, "%{{")?;
                for (idx, (key, value)) in entries.iter().enumerate() {
                    if idx != 0 {
                        write!(out, ", ")?;
                    }
                    write!(out, "V{} => (", key.0)?;
                    value.to_eir_text(indent, out)?;
                    write!(out, ")")?;
                }
                write!(out, "}}")?;
            },
            PatternNode::Tuple(entries) => {
                write!(out, "{{")?;
                for (idx, value) in entries.iter().enumerate() {
                    if idx != 0 {
                        write!(out, ", ")?;
                    }
                    write!(out, "(")?;
                    value.to_eir_text(indent, out)?;
                    write!(out, ")")?;
                }
                write!(out, "}}")?;
            },
            _ => unimplemented!("ToEirText unimplemented for pattern: {:?}", self),
        }
        Ok(())
    }
}

impl ToEirTextFun for Ebb {
    fn to_eir_text_fun(&self, fun: &Function, indent: usize, out: &mut Write) -> std::io::Result<()> {
        write_indent(out, indent)?;
        format_ebb_label(*self, fun, out)?;

        for op in fun.iter_op(*self) {
            write_indent(out, indent+1)?;

            let writes = fun.op_writes(op);
            format_value_list(writes, fun, out)?;
            if writes.len() > 0 { write!(out, " = ")?; }

            let reads = fun.op_reads(op);
            let branches = fun.op_branches(op);

            let sig = (reads.len(), writes.len(), branches.len());

            let kind = fun.op_kind(op);
            let mut default_reads = true;
            let mut default_branch = true;
            match kind {
                OpKind::Jump => {
                    assert!(sig == (0, 0, 1));
                    write!(out, "jump ")?;
                    branches[0].to_eir_text_fun(fun, indent, out)?;
                    default_branch = false;
                },
                OpKind::PackValueList => {
                    assert_matches!(sig, (_, 1, 0));
                    write!(out, "pack_value_list")?;
                },
                OpKind::UnpackValueList => {
                    assert_matches!(sig, (1, _, 0));
                    write!(out, "unpack_value_list")?;
                },
                OpKind::CaseStart { clauses } => {
                    assert_matches!(sig, (_, 1, 1));
                    write!(out, "case_start on: ")?;
                    format_value(reads[0], fun, out)?;
                    write!(out, ", values: [")?;
                    format_value_list(&reads[1..], fun, out)?;
                    write!(out, "] {{\n")?;

                    for clause in clauses {
                        write_indent(out, indent+2)?;

                        write!(out, "clause assigns: [")?;
                        for (idx, assign) in clause.assigns.iter().enumerate() {
                            if idx != 0 {
                                write!(out, ", ")?;
                            }
                            write!(out, "A{}", assign.0)?;
                        }
                        write!(out, "] {{\n")?;

                        for pattern in clause.patterns.iter() {
                            write_indent(out, indent+3)?;
                            write!(out, "pattern ")?;
                            pattern.node.to_eir_text(indent, out)?;
                            write!(out, ";\n")?;
                        }

                        write_indent(out, indent+2)?;
                        write!(out, "}};\n")?;
                    }

                    write_indent(out, indent+1)?;
                    write!(out, "}}")?;
                    default_reads = false;
                },
                OpKind::Case(num_clauses) => {
                    assert_matches!(sig, (1, 0, _));
                    assert!(sig.2 == num_clauses + 1);
                    write!(out, "case_body")?;
                },
                OpKind::CaseGuardOk => {
                    assert_matches!(sig, (1, 0, 0));
                    write!(out, "case_guard_ok")?;
                },
                OpKind::CaseGuardFail { .. } => {
                    assert_matches!(sig, (1, 0, 1));
                    write!(out, "case_guard_fail")?;
                },
                OpKind::IfTruthy => {
                    assert_matches!(sig, (1, 0, 1));
                    write!(out, "if_truthy ")?;
                    format_value(reads[0], fun, out)?;
                    write!(out, " else ")?;
                    format_branches(branches, fun, indent, out)?;
                    default_reads = false;
                    default_branch = false;
                },
                OpKind::MakeTuple => {
                    assert_matches!(sig, (_, 1, 0));
                    write!(out, "make_tuple")?;
                },
                OpKind::UnpackTuple => {
                    assert_matches!(sig, (1, _, 1));
                    write!(out, "unpack_tuple")?;
                },
                OpKind::MakeList => {
                    assert_matches!(sig, (_, 1, 0));
                    write!(out, "make_list ")?;

                    write!(out, "[")?;
                    format_value_list(&reads[1..], fun, out)?;
                    write!(out, " | ")?;
                    format_value_list(&[reads[0]], fun, out)?;
                    write!(out, "]")?;

                    default_reads = false;
                },
                OpKind::MakeBinary => {
                    write!(out, "make_binary")?;
                },
                OpKind::UnpackListCell => {
                    write!(out, "unpack_list_cell")?;
                },
                OpKind::MakeNoValue => {
                    write!(out, "make_no_value")?;
                },
                OpKind::Call { call_type, arity } => {
                    match call_type {
                        CallType::Normal => {
                            assert_matches!(sig, (_, 2, 1));
                            write!(out, "call")?;
                        },
                        CallType::Tail => {
                            assert_matches!(sig, (_, 0, 0));
                            write!(out, "call tail")?;
                        },
                        CallType::Cont => {
                            assert_matches!(sig, (_, 0, 0));
                            write!(out, "call cont")?;
                        },
                    }

                    write!(out, " ")?;
                    format_value(reads[0], fun, out)?;
                    write!(out, ":")?;
                    format_value(reads[1], fun, out)?;
                    write!(out, "/{}", arity)?;

                    write!(out, "(")?;
                    format_value_list(&reads[2..], fun, out)?;
                    write!(out, ")")?;

                    if *call_type == CallType::Normal {
                        write!(out, " except ")?;
                        format_branches(branches, fun, indent, out)?;
                    }

                    default_reads = false;
                    default_branch = false;
                },
                OpKind::Apply { call_type } => {
                    match call_type {
                        CallType::Normal => {
                            assert_matches!(sig, (_, 2, 1));
                            write!(out, "apply")?;
                        },
                        CallType::Tail => {
                            assert_matches!(sig, (_, 0, 0));
                            write!(out, "apply tail")?;
                        },
                        CallType::Cont => {
                            assert_matches!(sig, (_, 0, 0));
                            write!(out, "apply cont")?;
                        },
                    }

                    write!(out, " ")?;
                    format_value(reads[0], fun, out)?;

                    write!(out, "(")?;
                    format_value_list(&reads[1..], fun, out)?;
                    write!(out, ")")?;

                    if *call_type == CallType::Normal {
                        write!(out, " except ")?;
                        format_branches(branches, fun, indent, out)?;
                    }

                    default_reads = false;
                    default_branch = false;
                },
                OpKind::ReceiveStart => {
                    assert_matches!(sig, (1, 1, 1));
                    write!(out, "receive_start")?;
                },
                OpKind::ReceiveGetMessage => {
                    assert_matches!(sig, (1, 1, 0));
                    write!(out, "receive_get_message")?;
                },
                OpKind::ReceiveWait => {
                    assert_matches!(sig, (1, 0, 2));
                    write!(out, "receive_wait")?;
                },
                OpKind::UnpackEnv => {
                    assert_matches!(sig, (1, _, 0));
                    write!(out, "unpack_env")?;
                },
                OpKind::MakeClosureEnv { env_idx } => {
                    assert_matches!(sig, (_, 1, 0));
                    write!(out, "pack_env E{}", env_idx.index())?;
                },
                OpKind::MakeMap => {
                    write!(out, "make_map")?;
                },
                OpKind::MapGet => {
                    write!(out, "map_get")?;
                },
                OpKind::CaseValues =>
                    write!(out, "case_values")?,
                OpKind::ReturnThrow => {
                    assert_matches!(sig, (1, 0, 0));
                    write!(out, "return_throw")?;
                },
                OpKind::ReturnOk => {
                    assert_matches!(sig, (1, 0, 0));
                    write!(out, "return_ok")?;
                },
                OpKind::BindClosure { ident } => {
                    assert_matches!(sig, (1, 1, 0));
                    write!(out, "bind_closure ")?;
                    ident.to_eir_text(indent, out)?;
                    write!(out, " with")?;
                },
                OpKind::CaptureNamedFunction(ident) => {
                    assert_matches!(sig, (0, 1, 0));
                    write!(out, "capture_function ")?;
                    ident.to_eir_text(indent, out)?;
                },
                OpKind::ExcTrace => {
                    assert_matches!(sig, (1, 1, 0));
                    write!(out, "exc_trace")?;
                },
                OpKind::Unreachable => {
                    assert_matches!(sig, (0, 0, 0));
                    write!(out, "unreachable")?;
                },
                OpKind::ComparisonOperation(oper) => {
                    assert_matches!(sig, (2, 0, 1));
                    write!(out, "compare ")?;
                    match oper {
                        ComparisonOperation::Equal => {
                            write!(out, "equal")?;
                        }
                        _ => unimplemented!(),
                    }
                },
                OpKind::Move => {
                    assert_matches!(sig, (1, 1, 0));
                    write!(out, "move")?;
                },
                OpKind::PrimOp(atom) => {
                    write!(out, "prim_op {}", atom)?;
                },
                _ => {
                    unimplemented!("ToEirText unimplemented for: {:?}", kind);
                },
            }

            if default_reads && reads.len() > 0 {
                write!(out, " ")?;
                if reads.len() > 1 {
                    write!(out, "[")?;
                }
                format_value_list(reads, fun, out)?;
                if reads.len() > 1 {
                    write!(out, "]")?;
                }
            }

            if default_branch && branches.len() > 0 {
                write!(out, " branch ")?;
                format_branches(branches, fun, indent, out)?;
            }

            write!(out, ";\n")?;
        }

        Ok(())
    }
}

//impl ToEirTextFun for Op {
//    fn to_eir_text_fun(&self, fun: &Function, out: &mut Write) -> std::io::Result<()> {
//
//    }
//}
