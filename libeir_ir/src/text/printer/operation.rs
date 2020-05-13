use pretty::{DocAllocator, RefDoc};

use crate::binary::{BinaryEntrySpecifier, Endianness};
use crate::traits::FormatOpCtx;
use crate::{BasicType, Block, CallKind, DynValue, MatchKind, OpKind, Value};

use super::{
    get_value_list, BlockIteratorConfig, BlockValueLayout, FormatConfig, FormatState,
    FunctionFormatData, ValueFormatter,
};

pub struct FormatOpCtxImpl<'a, 'b, 'doc, B, V, L>
where
    B: BlockIteratorConfig,
    V: ValueFormatter,
    L: BlockValueLayout,
{
    format_data: &'a mut FunctionFormatData<'doc, B, V, L>,
    config: &'a FormatConfig<B, V, L>,
    state: &'a mut FormatState<'b>,
}

impl<'a, 'b, 'doc, B, V, L> FormatOpCtx<'doc> for FormatOpCtxImpl<'a, 'b, 'doc, B, V, L>
where
    B: BlockIteratorConfig,
    V: ValueFormatter,
    L: BlockValueLayout,
{
    fn arena(&self) -> &'doc pretty::Arena<'doc> {
        self.format_data.arena
    }

    fn value_use_to_doc(&mut self, value: DynValue) -> RefDoc<'doc, ()> {
        let val = self.state.function.value_get(value).unwrap();
        self.format_data
            .value_use(self.config, self.state, val, None)
    }
}

fn binary_specifier_to_doc<'a>(
    arena: &'a pretty::Arena<'a>,
    spec: &BinaryEntrySpecifier,
) -> RefDoc<'a, ()> {
    let f_endianness = |end| match end {
        &Endianness::Big => arena.text("big"),
        &Endianness::Little => arena.text("little"),
        &Endianness::Native => arena.text("native"),
    };
    let f_signed = |signed| match signed {
        true => arena.text("signed"),
        false => arena.text("unsigned"),
    };

    match spec {
        BinaryEntrySpecifier::Integer {
            signed,
            endianness,
            unit,
        } => {
            let items = [
                f_signed(*signed),
                f_endianness(endianness),
                arena.as_string(unit),
            ];
            arena
                .text("integer")
                .append(
                    arena
                        .intersperse(items.iter().cloned(), arena.text(","))
                        .parens(),
                )
                .into_doc()
        }
        BinaryEntrySpecifier::Float { endianness, unit } => {
            let items = [f_endianness(endianness), arena.as_string(unit)];
            arena
                .text("float")
                .append(
                    arena
                        .intersperse(items.iter().cloned(), arena.text(","))
                        .parens(),
                )
                .into_doc()
        }
        BinaryEntrySpecifier::Bytes { unit } => arena
            .text("bytes")
            .append(arena.as_string(unit).parens())
            .into_doc(),
        BinaryEntrySpecifier::Bits { unit } => arena
            .text("bits")
            .append(arena.as_string(unit).parens())
            .into_doc(),
        BinaryEntrySpecifier::Utf8 => arena.text("utf8").append(arena.nil().parens()).into_doc(),
        BinaryEntrySpecifier::Utf16 { endianness } => arena
            .text("utf16")
            .append(f_endianness(endianness))
            .into_doc(),
        BinaryEntrySpecifier::Utf32 { endianness } => arena
            .text("utf32")
            .append(f_endianness(endianness))
            .into_doc(),
    }
}

impl<'a, B, V, L> FunctionFormatData<'a, B, V, L>
where
    B: BlockIteratorConfig,
    V: ValueFormatter,
    L: BlockValueLayout,
{
    pub fn block_op_to_doc(
        &mut self,
        config: &FormatConfig<B, V, L>,
        state: &mut FormatState,
        block: Block,
    ) -> RefDoc<'a, ()> {
        let arena = self.arena;

        let op_opt = state.function.block_kind(block);
        if op_opt.is_none() {
            return arena.text("EMPTY_BLOCK").into_doc();
        }

        let op = op_opt.unwrap();
        let reads = state.function.block_reads(block);

        let op_doc = match op {
            OpKind::Case { clauses, .. } => {
                let block = arena.nil();

                arena
                    .nil()
                    .append(arena.text("case"))
                    .append(arena.space())
                    .append(block.nest(1).braces())
            }
            OpKind::Match { branches } => {
                let dests = reads[0];
                let num_dests = state.function.value_list_length(dests);
                let num_branches = branches.len();
                let mut branches_formatted = Vec::with_capacity(num_branches);
                for (i, kind) in branches.iter().enumerate() {
                    let block = state.function.value_list_get_n(dests, i).unwrap();
                    let block_val = self.value_use(config, state, block, None);
                    let args_vl = reads[i + 2];
                    let num_args = state.function.value_list_length(args_vl);
                    let mut args = Vec::with_capacity(num_args);
                    for n in 0..num_args {
                        args.push(state.function.value_list_get_n(args_vl, n).unwrap());
                    }
                    let formatted = match kind {
                        MatchKind::Value => {
                            let val = self.value_use(config, state, args[0], None);
                            let block_args = arena
                                .intersperse(
                                    args.iter()
                                        .skip(1)
                                        .map(|v| self.value_use(config, state, *v, None)),
                                    arena.text(",").append(arena.softline()),
                                )
                                .nest(1)
                                .parens();
                            let body = arena.nil().append(block_val).append(block_args);
                            arena
                                .nil()
                                .append(val)
                                .append(arena.space())
                                .append(arena.text("=>"))
                                .append(arena.space())
                                .append(arena.nil().append(body))
                        }
                        MatchKind::Type(ty) => {
                            let block_args = arena
                                .intersperse(
                                    args.iter().map(|v| self.value_use(config, state, *v, None)),
                                    arena.text(",").append(arena.softline()),
                                )
                                .nest(1)
                                .parens();
                            let body = arena.nil().append(block_val).append(block_args);
                            arena
                                .nil()
                                .append(arena.text("is_type"))
                                .append(arena.space())
                                .append(arena.text(type_to_text(ty)))
                                .append(arena.space())
                                .append(arena.text("=>"))
                                .append(arena.space())
                                .append(arena.nil().append(body))
                        }
                        MatchKind::Binary(ref spec) => {
                            let doc = binary_specifier_to_doc(arena, spec);
                            arena.nil().append(doc)
                        }
                        MatchKind::Tuple(arity) => {
                            let block_args = arena
                                .intersperse(
                                    args.iter().map(|v| self.value_use(config, state, *v, None)),
                                    arena.text(",").append(arena.softline()),
                                )
                                .nest(1)
                                .parens();
                            let body = arena.nil().append(block_val).append(block_args);
                            arena
                                .nil()
                                .append(arena.text("{}"))
                                .append(arena.space())
                                .append(arena.text(format!("arity {}", arity)))
                                .append(arena.space())
                                .append(arena.text("=>"))
                                .append(arena.space())
                                .append(arena.nil().append(body))
                        }
                        MatchKind::ListCell => {
                            let block_args = arena
                                .intersperse(
                                    args.iter().map(|v| self.value_use(config, state, *v, None)),
                                    arena.text(",").append(arena.softline()),
                                )
                                .nest(1)
                                .parens();
                            let body = arena.nil().append(block_val).append(block_args);
                            arena
                                .nil()
                                .append(arena.text("[]"))
                                .append(arena.space())
                                .append(arena.text("=>"))
                                .append(arena.space())
                                .append(arena.nil().append(body))
                        }
                        MatchKind::MapItem => {
                            let val = self.value_use(config, state, args[0], None);
                            let block_args = arena
                                .intersperse(
                                    args.iter()
                                        .skip(1)
                                        .map(|v| self.value_use(config, state, *v, None)),
                                    arena.text(",").append(arena.softline()),
                                )
                                .nest(1)
                                .parens();
                            let body = arena.nil().append(block_val).append(block_args);
                            arena
                                .nil()
                                .append(val)
                                .append(arena.space())
                                .append(arena.text("=>"))
                                .append(arena.space())
                                .append(arena.nil().append(body))
                        }
                        MatchKind::Wildcard => {
                            let block_args = arena
                                .intersperse(
                                    args.iter().map(|v| self.value_use(config, state, *v, None)),
                                    arena.text(",").append(arena.softline()),
                                )
                                .nest(1)
                                .parens();
                            let body = arena.nil().append(block_val).append(block_args);
                            arena
                                .nil()
                                .append(arena.text("_"))
                                .append(arena.space())
                                .append(arena.text("=>"))
                                .append(arena.space())
                                .append(arena.nil().append(body))
                        }
                    };
                    branches_formatted.push(formatted.indent(2));
                }

                let selector = self.value_use(config, state, reads[1], None);

                arena
                    .nil()
                    .append(arena.text("match"))
                    .append(arena.space())
                    .append(selector)
                    .append(arena.space())
                    .append(
                        arena
                            .hardline()
                            .append(arena.intersperse(branches_formatted, arena.hardline()))
                            .append(arena.hardline())
                            .braces(),
                    )
            }
            OpKind::Call(CallKind::Function) => {
                let callee_val = self.value_use(config, state, reads[0], None);
                let call_args = arena
                    .intersperse(
                        reads
                            .iter()
                            .skip(3)
                            .map(|v| self.value_use(config, state, *v, None)),
                        arena.text(",").append(arena.softline()),
                    )
                    .nest(1)
                    .parens();
                let flow_val = self.value_use(config, state, reads[1], None);
                let exc_val = self.value_use(config, state, reads[2], None);
                arena
                    .nil()
                    .append(callee_val)
                    .append(call_args)
                    .append(arena.space())
                    .append(arena.text("=>"))
                    .append(arena.space())
                    .append(flow_val)
                    .append(arena.space())
                    .append(arena.text("except"))
                    .append(arena.space())
                    .append(exc_val)
            }
            OpKind::Call(CallKind::ControlFlow) => {
                let fun_val = self.value_use(config, state, reads[0], None);
                let call_args = arena
                    .intersperse(
                        reads
                            .iter()
                            .skip(1)
                            .map(|v| self.value_use(config, state, *v, None)),
                        arena.text(",").append(arena.softline()),
                    )
                    .nest(1)
                    .parens();
                arena.nil().append(fun_val).append(call_args)
            }
            OpKind::TraceCaptureRaw => {
                assert!(reads.len() == 1);
                let arg = self.value_use(config, state, reads[0], None);
                arena
                    .nil()
                    .append(arena.text("trace_capture_raw"))
                    .append(arena.space())
                    .append(arg)
            }
            OpKind::UnpackValueList(n) => {
                assert!(reads.len() == 2);
                let block = self.value_use(config, state, reads[0], None);
                let val = self.value_use(config, state, reads[1], None);
                arena
                    .nil()
                    .append(arena.text("unpack"))
                    .append(arena.space())
                    .append(val)
                    .append(arena.space())
                    .append(arena.text("arity"))
                    .append(arena.space())
                    .append(arena.as_string(&format!("{}", n)))
                    .append(arena.space())
                    .append(arena.text("=>"))
                    .append(arena.space())
                    .append(block)
            }
            OpKind::IfBool => match reads.len() {
                3 => arena
                    .nil()
                    .append(arena.text("if_bool"))
                    .append(arena.space())
                    .append(self.value_use(config, state, reads[2], None))
                    .append(arena.space())
                    .append(self.value_use(config, state, reads[0], None))
                    .append(arena.space())
                    .append(self.value_use(config, state, reads[1], None)),
                4 => arena
                    .nil()
                    .append(arena.text("if_bool"))
                    .append(arena.space())
                    .append(self.value_use(config, state, reads[3], None))
                    .append(arena.space())
                    .append(self.value_use(config, state, reads[0], None))
                    .append(arena.space())
                    .append(self.value_use(config, state, reads[1], None))
                    .append(arena.space())
                    .append(self.value_use(config, state, reads[2], None)),
                _ => panic!(),
            },
            OpKind::Unreachable => arena.text("unreachable"),
            OpKind::Dyn(op) => {
                if let Some(printer) = state.function.dialect().get_op_printer(&**op) {
                    let mut ctx_impl = FormatOpCtxImpl {
                        format_data: self,
                        config,
                        state,
                    };

                    let doc = printer.to_doc(&mut ctx_impl, block);
                    arena.nil().append(doc)
                } else {
                    let call_args = arena
                        .intersperse(
                            reads
                                .iter()
                                .map(|v| self.value_use(config, state, *v, None)),
                            arena.text(",").append(arena.softline()),
                        )
                        .nest(1)
                        .parens();
                    arena.as_string(op.name()).append(call_args)
                }
            }
            _ => {
                println!("UNIMPL: {:?}", op);
                arena.text("unknown")
            }
        };

        op_doc.append(arena.text(";")).into_doc()
    }
}

fn type_to_text(ty: &BasicType) -> String {
    match ty {
        BasicType::List => "list".to_owned(),
        BasicType::ListCell => "cons".to_owned(),
        BasicType::Nil => "nil".to_owned(),
        BasicType::Tuple(arity) => format!("tuple({})", arity),
        BasicType::Map => "map".to_owned(),
        BasicType::Number => "number".to_owned(),
        BasicType::Float => "float".to_owned(),
        BasicType::Integer => "integer".to_owned(),
        BasicType::SmallInteger => "smallint".to_owned(),
        BasicType::BigInteger => "bigint".to_owned(),
    }
}
