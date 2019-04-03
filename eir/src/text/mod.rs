use pretty::{ Doc, DocBuilder, DocAllocator, Arena as DocArena };

use std::io::Write;

use matches::assert_matches;

use crate::{ Function, FunctionIdent, Atom, Value };
use crate::op::{ OpKind };
use crate::{ Ebb, Op, EbbCall };
use crate::new::ValueType;
use crate::pattern::PatternNode;
use crate::AtomicTerm;

use cranelift_entity::EntityRef;

// Desired syntax:

// ```
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
// ```

pub trait ToEirText {
    fn to_eir_text(&self, out: &mut Write) -> std::io::Result<()>;
}
pub trait ToEirTextFun {
    fn to_eir_text_fun(&self, fun: &Function, out: &mut Write) -> std::io::Result<()>;
}

impl ToEirText for FunctionIdent {
    fn to_eir_text(&self, out: &mut Write) -> std::io::Result<()> {
        if let Some((env_num, fun_num)) = self.lambda {
            write!(out, "{}:{}@{}.{}/{}",
                   self.module, self.name, env_num.0,
                   fun_num, self.arity)?;
        } else {
            write!(out, "{}:{}/{}",
                   self.module, self.name, self.arity)?;
        }
        Ok(())
    }
}

impl ToEirText for Function {
    fn to_eir_text(&self, out: &mut Write) -> std::io::Result<()> {
        let ident = self.ident();

        if let Some((env_num, fun_num)) = ident.lambda {
            write!(out, "{}@{}.{}/{} {{\n",
                   ident.name, env_num.0, fun_num, ident.arity)?;
        } else {
            write!(out, "{}/{} {{\n", ident.name, ident.arity)?;
        }

        for ebb in self.iter_ebb() {
            ebb.to_eir_text_fun(self, out)?;
            write!(out, "\n")?;
        }

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
    fn to_eir_text_fun(&self, fun: &Function, out: &mut Write) -> std::io::Result<()> {
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

fn format_branches(calls: &[EbbCall], fun: &Function,
                   out: &mut Write) -> std::io::Result<()> {
    for (idx, ebb_call) in calls.iter().enumerate() {
        if idx != 0 {
            write!(out, ", ")?;
        }
        ebb_call.to_eir_text_fun(fun, out)?;
    }
    Ok(())
}

impl ToEirText for PatternNode {
    fn to_eir_text(&self, out: &mut Write) -> std::io::Result<()> {
        match self {
            PatternNode::Atomic(AtomicTerm::Nil) => {
                write!(out, "[]")?;
            },
            PatternNode::Atomic(AtomicTerm::Atom(atom)) => {
                // TODO: Escape
                write!(out, "a\"{}\"", atom)?;
            },
            PatternNode::Wildcard => {
                write!(out, "_")?;
            },
            PatternNode::Assign(assign, inner) => {
                write!(out, "A{} = (", assign.0)?;
                inner.to_eir_text(out)?;
                write!(out, ")")?;
            },
            PatternNode::List(head, tail) => {
                write!(out, "[")?;

                for (idx, elem) in head.iter().enumerate() {
                    if idx != 0 {
                        write!(out, ", ")?;
                    }
                    elem.to_eir_text(out)?;
                }

                write!(out, " | ")?;
                tail.to_eir_text(out)?;
                write!(out, "]")?;
            },
            PatternNode::Map(entries) => {
                write!(out, "%{{")?;
                for (idx, (key, value)) in entries.iter().enumerate() {
                    if idx != 0 {
                        write!(out, ", ")?;
                    }
                    write!(out, "V{} => (", key.0)?;
                    value.to_eir_text(out)?;
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
    fn to_eir_text_fun(&self, fun: &Function, out: &mut Write) -> std::io::Result<()> {
        format_ebb_label(*self, fun, out)?;

        for op in fun.iter_op(*self) {
            write!(out, "    ")?;

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
                    branches[0].to_eir_text_fun(fun, out)?;
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

                        write!(out, "        clause assigns: [")?;
                        for (idx, assign) in clause.assigns.iter().enumerate() {
                            if idx != 0 {
                                write!(out, ", ")?;
                            }
                            write!(out, "A{}", assign.0)?;
                        }
                        write!(out, "] {{\n")?;

                        for pattern in clause.patterns.iter() {
                            write!(out, "            pattern ")?;
                            pattern.node.to_eir_text(out)?;
                            write!(out, ";\n")?;
                        }

                        write!(out, "        }};\n")?;
                    }

                    write!(out, "    }}")?;
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
                    assert_matches!(sig, (1, 0, 0));
                    write!(out, "case_guard_fail")?;
                },
                OpKind::IfTruthy => {
                    assert_matches!(sig, (1, 0, 1));
                    write!(out, "if_truthy ")?;
                    format_value(reads[0], fun, out)?;
                    default_reads = false;
                },
                OpKind::MakeTuple => {
                    assert_matches!(sig, (_, 1, 0));
                    write!(out, "make_tuple")?;
                },
                OpKind::Call { tail_call } => {
                    if *tail_call {
                        assert_matches!(sig, (_, 0, 2));
                        write!(out, "tail_call")?;
                    } else {
                        assert_matches!(sig, (_, 2, 1));
                        write!(out, "call")?;
                    }

                    write!(out, " ")?;
                    format_value(reads[0], fun, out)?;
                    write!(out, ":")?;
                    format_value(reads[1], fun, out)?;

                    write!(out, "(")?;
                    format_value_list(&reads[2..], fun, out)?;
                    write!(out, ")")?;

                    write!(out, " except ")?;
                    format_branches(branches, fun, out)?;

                    default_reads = false;
                    default_branch = false;
                },
                OpKind::Apply { tail_call } => {
                    if *tail_call {
                        assert_matches!(sig, (_, 0, 2));
                        write!(out, "tail_apply")?;
                    } else {
                        assert_matches!(sig, (_, 2, 1));
                        write!(out, "apply")?;
                    }

                    write!(out, " ")?;
                    format_value(reads[0], fun, out)?;

                    write!(out, "(")?;
                    format_value_list(&reads[1..], fun, out)?;
                    write!(out, ")")?;

                    write!(out, " except ")?;
                    format_branches(branches, fun, out)?;

                    default_reads = false;
                    default_branch = false;
                },
                OpKind::UnpackEnv => {
                    assert_matches!(sig, (1, _, 0));
                    write!(out, "unpack_env")?;
                },
                OpKind::MakeClosureEnv { env_idx } => {
                    assert_matches!(sig, (_, 1, 0));
                    write!(out, "pack_env E{}", env_idx.0)?;
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
                    ident.to_eir_text(out)?;
                    write!(out, " with")?;
                },
                OpKind::CaptureNamedFunction(ident) => {
                    assert_matches!(sig, (0, 1, 0));
                    write!(out, "capture_function ")?;
                    ident.to_eir_text(out)?;
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
                format_branches(branches, fun, out)?;
            }

            write!(out, ";\n")?;
        }

        Ok(())
    }
}

