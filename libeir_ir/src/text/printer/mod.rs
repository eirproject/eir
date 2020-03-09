#![allow(unused)]

use std::error::Error;
use std::fmt::Write;
use std::marker::PhantomData;
use std::collections::BTreeMap;

use cranelift_entity::EntityRef;
use pretty::{DocAllocator, Arena, RefDoc};
use petgraph::visit::Dfs;

use crate::{Function, Block, Value, Const, ValueKind, OpKind, CallKind, PrimOpKind, Module};
use crate::graph::EntityVisitMap;

mod operation;
mod constant;

type DynError = Box<dyn Error>;

//pub trait EirPrint<B, V, L>
//where
//    B: BlockIteratorConfig,
//    V: ValueFormatter,
//    L: BlockValueLayout,
//{
//    type Context;
//    fn to_eir_text(&self, config: &FormatConfig<B, V, L>, context: &Self::Context) -> String;
//}

//impl<B, V, L> EirPrint<B, V, L> for Block
//where
//    B: BlockIteratorConfig,
//    V: ValueFormatter,
//    L: BlockValueLayout,
//{
//}

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

pub struct FormatConfig<B, V, L>
where
    B: BlockIteratorConfig,
    V: ValueFormatter,
    L: BlockValueLayout,
{
    pub width: usize,

    /// Encapsulates the iteration order for blocks within a function.
    pub block_iterator_config: B,

    /// Formatter for values within a function.
    pub value_formatter: V,

    /// Layout for values within a function.
    pub block_value_layout: L,
}

pub type StandardFormatConfig = FormatConfig<DfsBlockIteratorConfig, StandardValueFormatter, ReferencePrimopBlockValueLayout>;
impl Default for StandardFormatConfig {
    fn default() -> Self {
        FormatConfig {
            width: 80,
            block_iterator_config: DfsBlockIteratorConfig,
            value_formatter: StandardValueFormatter,
            block_value_layout: ReferencePrimopBlockValueLayout::default(),
        }
    }
}

pub struct FormatState<'a> {
    pub function: &'a Function,
    pub nesting: usize,
}

/// Iteration strategy for blocks within a function
pub trait BlockIteratorConfig {
    type Iter: BlockIterator;
    fn new(&self, fun: &Function) -> Self::Iter;
}
/// Implementation of block iteration strategy.
/// ## Invariants
/// * Each block MUST be returned at most once.
pub trait BlockIterator {
    fn next(&mut self, fun: &Function) -> Option<Block>;
}

pub struct DfsBlockIteratorConfig;
impl BlockIteratorConfig for DfsBlockIteratorConfig {
    type Iter = DfsBlockIterator;
    fn new(&self, fun: &Function) -> Self::Iter {
        let graph = fun.block_graph();
        let entry = fun.block_entry();
        let dfs = Dfs::new(&graph, entry);
        DfsBlockIterator {
            dfs,
        }
    }
}
pub struct DfsBlockIterator {
    dfs: Dfs<Block, EntityVisitMap<Block>>,
}
impl BlockIterator for DfsBlockIterator {
    fn next(&mut self, fun: &Function) -> Option<Block> {
        self.dfs.next(&fun.block_graph())
    }
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
#[allow(dead_code)]
pub enum ValueSite {
    Decl,
    Use,
}
pub trait ValueFormatter {
    fn value(&self, out: &mut String, fun: &Function, site: ValueSite, value: Value);
}

/// This value formatter prints values in the format supported by the
/// parser, and is considered the standard format.
pub struct StandardValueFormatter;
impl ValueFormatter for StandardValueFormatter {
    fn value(&self, out: &mut String, fun: &Function, _site: ValueSite, value: Value) {
        match fun.value_kind(value) {
            ValueKind::Block(block) => write!(out, "{}", block).unwrap(),
            _ => write!(out, "%{}", value.index()).unwrap(),
        }
    }
}

pub trait BlockValueLayout {
    /// Lays out the root scope for the module. This is called once
    /// at the beginning of processing a module.
    fn layout_module(&mut self, fun: &Function);
    /// Lays out the given block. This will be called once for each block.
    fn layout(&mut self, fun: &Function, block: Block);

    /// Values for the current layout.
    fn values(&self) -> &[Value];

    /// Queries whether the given value should be laid out inline, or if
    /// it should be referenced by value.
    fn should_layout(&self, value: Value, within: Option<Value>) -> bool;
}

pub struct ReferencePrimopBlockValueLayout {
    values: Vec<Value>,
    values_set: BTreeMap<Value, usize>,
}
impl Default for ReferencePrimopBlockValueLayout {
    fn default() -> Self {
        ReferencePrimopBlockValueLayout {
            values: Vec::new(),
            values_set: BTreeMap::new(),
        }
    }
}
impl BlockValueLayout for ReferencePrimopBlockValueLayout {
    fn layout_module(&mut self, fun: &Function) {}
    fn layout(&mut self, fun: &Function, block: Block) {
        self.values.clear();
        self.values_set.clear();

        fun.block_walk_nested_values::<_, ()>(block, &mut |value| {
            if fun.value_primop(value).is_some() {
                self.values.push(value);
            }
            Ok(())
        });

        for (idx, value) in self.values.iter().enumerate() {
            self.values_set.insert(*value, idx);
        }

        let mut idx = 0;
        let values_set = &self.values_set;
        self.values.retain(|val| {
            let ret = values_set[val] == idx;
            idx += 1;
            ret
        });
    }

    fn values(&self) -> &[Value] {
        &self.values
    }

    fn should_layout(&self, value: Value, within: Option<Value>) -> bool {
        !self.values_set.contains_key(&value)
    }
}

pub trait BlockFormatSink {
    type LineIndex: Copy;

    fn write_indent(&mut self, num: usize) -> Result<(), DynError> {
        for _ in 0..num {
            self.write_str("  ")?;
        }
        Ok(())
    }

    fn write_str(&mut self, string: &str) -> Result<(), DynError>;
    fn commit_line(&mut self) -> Result<Self::LineIndex, DynError>;

    /// Informs the sink that the given range of lines
    /// contains the given blocks.
    /// Blocks will never overlap, and this will be called
    /// at most once for each block.
    fn block_lines(
        &mut self,
        _block: Block,
        _range: (Self::LineIndex, Self::LineIndex),
    ) {}
}

pub struct StringSink {
    string: String,
}
impl StringSink {
    pub fn new() -> Self {
        StringSink {
            string: String::new(),
        }
    }
    pub fn finalize(self) -> String {
        self.string
    }
}
impl BlockFormatSink for &mut StringSink {
    type LineIndex = ();
    fn write_str(&mut self, string: &str) -> Result<(), DynError> {
        self.string.push_str(string);
        Ok(())
    }
    fn commit_line(&mut self) -> Result<(), DynError> {
        self.string.push('\n');
        Ok(())
    }
}

pub(crate) struct FunctionFormatData<'a, B, V, L>
where
    B: BlockIteratorConfig,
    V: ValueFormatter,
    L: BlockValueLayout,
{
    pub arena: &'a Arena<'a>,
    pub buf: String,
    pub value_buf: Vec<Value>,
    pub config: PhantomData<FormatConfig<B, V, L>>,
}
impl<'a, B, V, L> FunctionFormatData<'a, B, V, L>
where
    B: BlockIteratorConfig,
    V: ValueFormatter,
    L: BlockValueLayout,
{

    pub(crate) fn block_to_doc(
        &mut self,
        config: &mut FormatConfig<B, V, L>,
        state: &mut FormatState,
        block: Block,
    ) -> RefDoc<'a, ()> {
        let arena = self.arena;

        let ident = arena.as_string(block);
        let args = arena.intersperse(
            state.function.block_args(block)
                .iter().map(|v| {
                    self.buf.clear();
                    config.value_formatter.value(
                        &mut self.buf,
                        state.function,
                        ValueSite::Decl,
                        *v,
                    );
                    arena.as_string(&self.buf)
                }),
            arena.text(", "),
        ).parens();
        let header = ident
            .append(args)
            .append(":")
            .group();

        let body = self.block_body_to_doc(config, state, block);

        header.append(arena.hardline().append(body).nest(2)).into_doc()
    }

    pub(crate) fn block_body_to_doc(
        &mut self,
        config: &mut FormatConfig<B, V, L>,
        state: &mut FormatState,
        block: Block,
    ) -> RefDoc<'a, ()> {
        let arena = self.arena;

        //let mut value_buf = Vec::new();
        //state.function.block_walk_nested_values::<_, ()>(block, &mut |val| {
        //    match state.function.value_kind(val) {
        //        ValueKind::PrimOp(prim) => {
        //            value_buf.push(val);
        //        },
        //        _ => (),
        //    }
        //    Ok(())
        //});
        //println!("value buf: {:?}", value_buf);

        config.block_value_layout.layout(state.function, block);
        let value_buf = config.block_value_layout.values();

        let values = arena.concat(
            value_buf.iter().rev()
                .map(|v| self.value_assign_to_doc(config, state, *v))
                .map(|v| arena.nil().append(v).append(arena.hardline()))
        );

        let op = self.block_op_to_doc(config, state, block);

        values
            .append(op)
            .into_doc()
    }


    pub(crate) fn value_assign_to_doc(
        &mut self,
        config: &FormatConfig<B, V, L>,
        state: &mut FormatState,
        value: Value,
    ) -> RefDoc<'a, ()> {
        let arena = self.arena;

        let value_kind = state.function.value_kind(value);
        let doc = match value_kind {
            ValueKind::PrimOp(prim) => {
                let prim_kind = state.function.primop_kind(prim);
                let reads = state.function.primop_reads(prim);

                match prim_kind {
                    PrimOpKind::CaptureFunction => {
                        assert!(reads.len() == 3);
                        arena.nil()
                             .append(self.value_use_to_doc(config, state, reads[0]))
                             .append(arena.text(":"))
                             .append(self.value_use_to_doc(config, state, reads[1]))
                             .append(arena.text("/"))
                             .append(self.value_use_to_doc(config, state, reads[2]))
                    },
                    PrimOpKind::Tuple => {
                        arena
                            .intersperse(
                                reads.iter()
                                     .map(|r| self.value_use_to_doc(config, state, *r)),
                                arena.text(",").append(arena.space())
                            )
                            .enclose(arena.text("{"), arena.text("}"))
                    },
                    PrimOpKind::ValueList => {
                        arena
                            .intersperse(
                                reads.iter()
                                     .map(|r| self.value_use_to_doc(config, state, *r)),
                                arena.text(",").append(arena.space())
                            )
                            .enclose(arena.text("<"), arena.text(">"))
                    },
                    PrimOpKind::ListCell => {
                        assert!(reads.len() == 2);
                        arena.nil()
                             .append(arena.text("["))
                             .append(self.value_use_to_doc(config, state, reads[0]))
                             .append(arena.space())
                             .append(arena.text("|"))
                             .append(arena.space())
                             .append(self.value_use_to_doc(config, state, reads[1]))
                             .append(arena.text("]"))
                    },
                    _ => unimplemented!("{:?}", prim_kind),
                }
            },
            _ => unimplemented!("{:?}", value_kind),
        };

        self.buf.clear();
        config.value_formatter.value(
            &mut self.buf,
            state.function,
            ValueSite::Decl,
            value,
        );
        let value_doc = arena.as_string(&self.buf);

        arena.nil()
            .append(value_doc)
            .append(arena.space())
            .append(arena.text("="))
            .append(arena.space())
            .append(doc)
            .append(arena.text(";"))
            .into_doc()
    }

    fn constant_to_doc(
        &mut self,
        _config: &FormatConfig<B, V, L>,
        state: &mut FormatState,
        constant: Const,
    ) -> RefDoc<'a, ()> {
        self::constant::constant_to_doc(&self.arena, state.function.cons(), constant)
    }

    fn value_use_to_doc(
        &mut self,
        config: &FormatConfig<B, V, L>,
        state: &mut FormatState,
        value: Value
    ) -> RefDoc<'a, ()> {
        self.buf.clear();
        config.value_formatter.value(
            &mut self.buf,
            state.function,
            ValueSite::Use,
            value,
        );
        self.arena.as_string(&self.buf).into_doc()
    }


}

fn format_function_body_state<B, V, L, S>(
    config: &mut FormatConfig<B, V, L>,
    state: &mut FormatState,
    mut sink: S,
) -> Result<(), DynError>
where
    B: BlockIteratorConfig,
    V: ValueFormatter,
    L: BlockValueLayout,
    S: BlockFormatSink,
{
    let function = state.function;
    let mut block_iter = config.block_iterator_config.new(function);

    let arena = Arena::new();
    let mut ctx = FunctionFormatData {
        arena: &arena,
        buf: String::new(),
        value_buf: Vec::new(),
        config: PhantomData,
    };

    let inner_width = config.width - (state.nesting * 2);

    while let Some(block) = block_iter.next(function) {
        let doc = ctx.block_to_doc(config, state, block);

        ctx.buf.clear();
        doc.render_fmt(inner_width, &mut ctx.buf).unwrap();

        let mut first_line = None;
        let mut last_line = None;

        for line in ctx.buf.lines() {
            sink.write_indent(state.nesting)?;
            sink.write_str(line)?;
            let line = sink.commit_line()?;

            first_line = Some(first_line.unwrap_or(line));
            last_line = Some(line);
        }

        sink.block_lines(block, (first_line.unwrap(), last_line.unwrap()));
    }

    Ok(())
}

pub fn format_function_body<B, V, L, S>(
    function: &Function,
    config: &mut FormatConfig<B, V, L>,
    mut sink: S,
) -> Result<(), DynError>
where
    B: BlockIteratorConfig,
    V: ValueFormatter,
    L: BlockValueLayout,
    S: BlockFormatSink,
{
    let mut state = FormatState {
        function,
        nesting: 0,
    };
    format_function_body_state(config, &mut state, sink)
}

pub fn format_module<B, V, L, S>(
    module: &Module,
    config: &mut FormatConfig<B, V, L>,
    mut sink: S,
) -> Result<(), DynError>
where
    B: BlockIteratorConfig,
    V: ValueFormatter,
    L: BlockValueLayout,
    S: BlockFormatSink,
{
    unimplemented!()
}

impl Function {

    pub fn to_text<B, V, L>(&self, config: &mut FormatConfig<B, V, L>) -> String
    where
        B: BlockIteratorConfig,
        V: ValueFormatter,
        L: BlockValueLayout,
    {
        let mut sink = StringSink::new();
        format_function_body(self, config, &mut sink).unwrap();
        sink.finalize()
    }

    pub fn to_text_standard(&self) -> String {
        self.to_text(&mut StandardFormatConfig::default())
    }

    pub fn block_to_text<B, V, L>(&self, block: Block, config: &mut FormatConfig<B, V, L>) -> String
    where
        B: BlockIteratorConfig,
        V: ValueFormatter,
        L: BlockValueLayout,
    {
        let mut sink = StringSink::new();

        let arena = Arena::new();
        let mut ctx = FunctionFormatData {
            arena: &arena,
            buf: String::new(),
            value_buf: Vec::new(),
            config: PhantomData,
        };

        let mut state = FormatState {
            function: self,
            nesting: 0,
        };

        let doc = ctx.block_to_doc(config, &mut state, block);

        ctx.buf.clear();
        doc.render_fmt(config.width, &mut ctx.buf).unwrap();
        ctx.buf
    }

}

impl Module {

    pub fn to_text<B, V, L>(&self, config: &mut FormatConfig<B, V, L>) -> String
    where
        B: BlockIteratorConfig,
        V: ValueFormatter,
        L: BlockValueLayout,
    {
        let mut sink = StringSink::new();
        format_module(self, config, &mut sink).unwrap();
        sink.finalize()
    }

    pub fn to_text_standard(&self) -> String {
        self.to_text(&mut StandardFormatConfig::default())
    }

}

#[cfg(test)]
mod tests {
    use super::{FormatConfig, StandardFormatConfig, StringSink, format_function_body};

    #[test]
    fn woo() {
        let ir = crate::parse_function_unwrap("
a'woo':a'hoo'/1 {
    entry(%ret, %thr, %a):
        %f1 = a'erlang':a'+'/2;
        %f1(%a, 2) => b2 except %thr;
    b2(%b):
        %f2 = a'erlang':a'/'/2;
        %f2(%b, 2) => %ret except %thr;
}
");
        let text = ir.to_text(&mut StandardFormatConfig::default());
        println!("{}", text);
    }

}
