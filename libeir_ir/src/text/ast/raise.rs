use std::collections::BTreeMap;

use libeir_intern::Ident;

use crate::Function;
use crate::{Block, Value};
//use crate::OpKind;
use crate::text::ast;

#[allow(dead_code)]
struct RaiseCtx {
    block_names: BTreeMap<Block, Ident>,
    value_names: BTreeMap<Value, Ident>,
}
impl RaiseCtx {
    pub fn block_name(&mut self, _block: Block) -> Ident {
        unimplemented!()
    }

    pub fn value_name(&mut self, _value: Value) -> Ident {
        unimplemented!()
    }
}

impl Function {
    pub fn raise(&self) -> ast::Function {
        let graph = self.block_graph();

        let mut ctx = RaiseCtx {
            block_names: BTreeMap::new(),
            value_names: BTreeMap::new(),
        };
        let mut items = Vec::new();

        for block in graph.dfs_iter() {
            // Block label
            items.push(ast::FunctionItem::Label(ast::Label {
                name: ast::Value::Block(ctx.block_name(block)),
                args: self
                    .block_args(block)
                    .iter()
                    .map(|v| ast::Value::Value(ctx.value_name(*v)))
                    .collect(),
            }));

            //if let Some(kind) = self.block_kind(block) {
            //    match kind {
            //        OpKind::Call => {
            //            //items.push(ast::FunctionItem::Op(ast::Op::Call(ast::CallOp {
            //            //
            //            //})));
            //        }
            //        _ => unimplemented!(),
            //    }
            //}
        }

        ast::Function {
            name: self.ident().name,
            arity: self.ident().arity.into(),
            items,
        }
    }
}
