use crate::Function;
use super::printer::{ ToEirTextFun, ToEirTextContext };

use libeir_util_dot_graph::GraphPrinter;

use petgraph::visit::{ Dfs, IntoNeighbors };

pub fn function_into_graph_printer<O>(fun: &Function, g: &mut GraphPrinter<O>)
where
    O: std::fmt::Write,
{
    let mut to_eir_ctx = ToEirTextContext::new();

    let mut buf = Vec::new();

    super::printer::print_constants(&mut to_eir_ctx, fun, 0, &mut buf).unwrap();
    let text = std::str::from_utf8(&buf).unwrap();
    g.node("constants", text);

    let block_graph = fun.block_graph();
    let mut block_dfs = Dfs::new(&block_graph, fun.block_entry());

    while let Some(block) = block_dfs.next(&block_graph) {
        let block_val = fun.block_value(block);

        buf.clear();
        block.to_eir_text_fun(&mut to_eir_ctx, fun, 0, &mut buf).unwrap();
        let text = std::str::from_utf8(&buf).unwrap();
        g.node(block_val, text);

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
