use std::marker::PhantomData;

use super::printer as pr;
use super::printer::{FormatState, FunctionFormatData};
use crate::Function;
use pretty::Arena;

use libeir_util_dot_graph::GraphPrinter;

use petgraph::visit::{Dfs, IntoNeighbors};

pub fn function_into_graph_printer<O>(fun: &Function, g: &mut GraphPrinter<O>)
where
    O: std::fmt::Write,
{
    let mut buf = String::new();

    let mut config = pr::FormatConfig {
        width: 80,
        print_locations: true,
        block_iterator_config: pr::DfsBlockIteratorConfig,
        value_formatter: pr::StandardValueFormatter,
        block_value_layout: pr::ReferencePrimopBlockValueLayout::default(),
    };
    let mut state = FormatState {
        function: fun,
        nesting: 0,
    };

    let arena = Arena::new();
    let mut ctx = FunctionFormatData {
        arena: &arena,
        buf: String::new(),
        value_buf: Vec::new(),
        config: PhantomData,
    };

    let block_graph = fun.block_graph();
    let mut block_dfs = Dfs::new(&block_graph, fun.block_entry());

    while let Some(block) = block_dfs.next(&block_graph) {
        let block_val = fun.block_value(block);

        let doc = ctx.block_to_doc(&mut config, &mut state, block);
        buf.clear();
        doc.render_fmt(80, &mut buf).unwrap();
        g.node(block_val, &buf);

        for out in block_graph.neighbors(block) {
            let out_val = fun.block_value(out);
            g.edge(block_val, out_val, "");
        }
    }
}

pub fn function_to_dot(fun: &Function) -> String {
    let mut g = GraphPrinter::new();
    function_into_graph_printer(fun, &mut g);
    g.finish().unwrap()
}
