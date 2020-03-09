use pretty::{RefDoc, DocAllocator};

use crate::{Block, OpKind, CallKind};

use super::{
    FunctionFormatData, FormatConfig, FormatState,
    BlockIteratorConfig, ValueFormatter, BlockValueLayout,
    get_value_list,
};

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
    ) -> RefDoc<'a, ()>
    {
        let arena = self.arena;

        let op = state.function.block_kind(block).unwrap();
        let reads = state.function.block_reads(block);

        let op_doc = match op {
            OpKind::Match { branches } => {
                let targets_opt = get_value_list(state.function, reads[0]);
                let targets_one = &[reads[0]];
                let targets = targets_opt.unwrap_or(targets_one);

                let block = arena.nil();

                arena.nil()
                    .append(arena.text("match"))
                    .append(arena.space())
                    .append(block.nest(1).braces())
            },
            OpKind::Call(CallKind::Function) => {
                println!("args: {:?}", reads);
                let fun_val = self.value_use_to_doc(config, state, reads[0]);
                let call_args = arena.intersperse(
                    reads.iter().skip(3)
                        .map(|v| self.value_use_to_doc(config, state, *v)),
                    arena.text(",").append(arena.softline()),
                ).nest(1).parens();
                let flow_val = self.value_use_to_doc(config, state, reads[1]);
                let exc_val = self.value_use_to_doc(config, state, reads[2]);
                arena.nil()
                    .append(fun_val)
                    .append(call_args)
                    .append(arena.space())
                    .append(arena.text("=>"))
                    .append(arena.space())
                    .append(flow_val)
                    .append(arena.space())
                    .append(arena.text("except"))
                    .append(arena.space())
                    .append(exc_val)
            },
            OpKind::Call(CallKind::ControlFlow) => {
                let fun_val = self.value_use_to_doc(config, state, reads[0]);
                let call_args = arena.intersperse(
                    reads.iter().skip(1)
                        .map(|v| self.value_use_to_doc(config, state, *v)),
                    arena.text(",").append(arena.softline()),
                ).nest(1).parens();
                arena.nil()
                    .append(fun_val)
                    .append(call_args)
            },
            OpKind::TraceCaptureRaw => {
                assert!(reads.len() == 1);
                let arg = self.value_use_to_doc(config, state, reads[0]);
                arena.nil()
                    .append(arena.text("trace_capture_raw"))
                    .append(arena.space())
                    .append(arg)
            },
            OpKind::UnpackValueList(n) => {
                assert!(reads.len() == 2);
                let block = self.value_use_to_doc(config, state, reads[0]);
                let val = self.value_use_to_doc(config, state, reads[1]);
                arena.nil()
                    .append(arena.text("unpack"))
                    .append(arena.space())
                    .append(val)
                    .append(arena.space())
                    .append(arena.text("arena"))
                    .append(arena.space())
                    .append(arena.as_string(&format!("{}", n)))
                    .append(arena.space())
                    .append(arena.text("=>"))
                    .append(arena.space())
                    .append(block)
            },
            OpKind::IfBool => {
                match reads.len() {
                    3 => {
                        arena.nil()
                            .append(arena.text("if_bool"))
                            .append(arena.space())
                            .append(self.value_use_to_doc(config, state, reads[2]))
                            .append(arena.space())
                            .append(self.value_use_to_doc(config, state, reads[0]))
                            .append(arena.space())
                            .append(self.value_use_to_doc(config, state, reads[1]))
                    },
                    4 => {
                        arena.nil()
                            .append(arena.text("if_bool"))
                            .append(arena.space())
                            .append(self.value_use_to_doc(config, state, reads[3]))
                            .append(arena.space())
                            .append(self.value_use_to_doc(config, state, reads[0]))
                            .append(arena.space())
                            .append(self.value_use_to_doc(config, state, reads[1]))
                            .append(arena.space())
                            .append(self.value_use_to_doc(config, state, reads[2]))
                    },
                    _ => panic!(),
                }
            },
            OpKind::Unreachable => arena.text("unreachable"),
            _ => {
                println!("UNIMPL: {:?}", op);
                arena.text("unknown")
            },
        };

        op_doc.append(arena.text(";")).into_doc()
    }

}

