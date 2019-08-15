use libeir_ir::{Function, Block, Value};
use libeir_ir::LiveValues;

use cranelift_entity::SecondaryMap;

pub struct LowerData {
    pub live: LiveValues,

    pub block_modes: SecondaryMap<Block, BlockMode>,
    pub value_modes: SecondaryMap<Value, ValueMode>,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BlockMode {
    BasicBlock,
    Function {
        ok_cont: Option<Value>,
        throw_cont: Option<Value>,
    },
}
impl Default for BlockMode {
    fn default() -> Self {
        BlockMode::BasicBlock
    }
}
impl BlockMode {

    pub fn is_fun(&self) -> bool {
        if let BlockMode::Function { .. } = self {
            true
        } else {
            false
        }
    }

}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum ValueMode {
    Value,
    FunctionReturn(Block),
    FunctionThrow(Block),
}
impl Default for ValueMode {
    fn default() -> Self {
        ValueMode::Value
    }
}

pub fn analyze(fun: &Function) -> LowerData {
    let mut data = LowerData {
        live: fun.live_values(),
        block_modes: SecondaryMap::new(),
        value_modes: SecondaryMap::new(),
    };

    let entry = fun.block_entry();
    let graph = fun.block_graph();

    // All annotated functions are marked as such
    for block in graph.dfs_iter() {
        if fun.block_annotated_fun(block) {
            let args = fun.block_args(block);
            assert!(args.len() >= 2, "{:?} {:?}", block, args);

            data.block_modes[block] = BlockMode::Function {
                ok_cont: Some(args[0]),
                throw_cont: Some(args[1]),
            };

            data.value_modes[args[0]] = ValueMode::FunctionReturn(block);
            data.value_modes[args[1]] = ValueMode::FunctionThrow(block);
        }
    }

    assert!(data.block_modes[entry].is_fun());

    data
}
