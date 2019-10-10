use std::collections::{ BTreeSet, BTreeMap };

use crate::{ Function, FunctionBuilder };
use crate::{ Value, Block };
use crate::{ ValueKind, OpKind };

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub struct EntryArg(usize);

/// Utility struct implementing the lambda mangling primitive.
/// Supports both mangling within a single function container, and
/// across function containers, implemented by `run` and `run_across`
/// respectively.
pub struct Mangler {
    entry: Option<Block>,
    num_args: usize,

    new_entry: Option<Block>,

    values_map: BTreeMap<Value, RenameDest>,
    blocks_map: BTreeMap<Block, RenameDest>,

    value_buf: Vec<Value>,

    values: Option<BTreeSet<Value>>,

    scope: Option<BTreeSet<Block>>,
    to_walk: Vec<Block>,
}

impl Mangler {

    pub fn new() -> Self {
        Mangler {
            entry: None,
            num_args: 0,

            new_entry: None,

            values_map: BTreeMap::new(),
            blocks_map: BTreeMap::new(),

            value_buf: Vec::new(),

            values: Some(BTreeSet::new()),

            scope: Some(BTreeSet::new()),
            to_walk: Vec::new(),
        }
    }

    /// Clears the mangler for a new use.
    /// This is automatically done by the `start` method.
    fn clear(&mut self) {
        self.entry = None;
        self.num_args = 0;

        self.new_entry = None;

        self.values_map.clear();
        self.blocks_map.clear();

        self.value_buf.clear();

        self.values.as_mut().unwrap().clear();

        self.scope.as_mut().unwrap().clear();
        self.to_walk.clear();
    }

    /// Clears the mangler of all existing data, and starts a new transaction
    /// for the given entry block.
    pub fn start(&mut self, block: Block, b: &mut FunctionBuilder) {
        self.clear();
        self.entry = Some(block);

        let new_entry = b.block_insert();
        self.new_entry = Some(new_entry);
    }

    /// Add an argument on the new entry block.
    /// The return value can be used to make renames using the `add_rename` method.
    pub fn add_argument(&mut self, b: &mut FunctionBuilder) -> Value {
        b.block_arg_insert(self.new_entry.unwrap())
        //let arg = EntryArg(self.num_args);
        //self.num_args += 1;
        //arg
    }

    /// Copies the signature of the entry block.
    pub fn copy_entry(&mut self, fun: &mut FunctionBuilder) {
        let entry = self.entry.unwrap();
        let new_entry = self.new_entry.unwrap();

        let args_len = fun.fun().block_args(entry).len();
        for n in 0..args_len {
            let from = fun.fun().block_args(entry)[n];
            let to = fun.block_arg_insert(new_entry);
            self.add_rename(from, to);
        }
    }

    pub fn add_rename<S, D>(&mut self, old: S, new: D)
    where
        S: RenameSource,
        D: Into<RenameDest>,
    {
        old.add(new.into(), self);
    }

    pub fn add_rename_recursive<S, D>(&mut self, old: S, new: D)
    where
        S: RenameSource,
        D: Into<RenameDest>,
    {
        old.add(new.into(), self);
    }

    /// Runs lambda mangling on a single function container
    pub fn run(&mut self, fun: &mut FunctionBuilder) -> Block {
        let mut recv = SingleMangleReceiver {
            fun,
        };
        self.run_inner(&mut recv)
    }

    // Runs lambda mangling while copying across function containers
    pub fn run_across(&mut self, from: &Function, to: &mut FunctionBuilder) -> Block {
        let mut recv = CopyMangleReceiver {
            from,
            to,
        };
        self.run_inner(&mut recv)
    }

    fn run_inner<'a, R>(&mut self, recv: &mut R) -> Block where R: MangleReceiver<'a> {
        let mut scope = self.scope.take().unwrap();
        let mut to_map_values = self.values.take().unwrap();

        // Insert new entry block
        let entry = self.entry.unwrap();
        let new_entry = self.new_entry.unwrap();
        {
            let to = recv.to();
            for _ in 0..self.num_args {
                to.block_arg_insert(new_entry);
            }
        }

        // Make sure no added rename sources are blocks
        {
            let from = recv.from();

            self.value_buf.clear();
            self.value_buf.extend(self.values_map.keys().cloned());

            for key in self.value_buf.drain(..) {
                let dest = self.values_map[&key];
                //if let RenameSource::Value(val) = key {
                if let Some(block) = from.value_block(key) {
                    if let Some(existing) = self.blocks_map.get(&block) {
                        assert!(*existing == dest);
                    }
                    self.blocks_map.insert(block, dest);
                    self.values_map.remove(&key);
                }
            }
        }

        // Walk scope
        {
            self.to_walk.push(entry);

            let from = recv.from();
            let graph = from.block_graph();

            while let Some(mut block) = self.to_walk.pop() {
                if scope.contains(&block) { continue; }

                if let Some(mapped) = self.blocks_map.get(&block) {
                    if let RenameDest::Block(mapped_block) = mapped {
                        block = *mapped_block;
                    } else {
                        continue;
                    }
                }
                scope.insert(block);

                println!("AAA {}: {:?}", block, from.block_reads(block));
                for read in from.block_reads(block) {
                    from.value_walk_nested_values::<_, ()>(*read, &mut |val| {
                        println!("A: {}", val);
                        to_map_values.insert(val);
                        Ok(())
                    }).unwrap();
                }

                for out in graph.outgoing(block) {
                    self.to_walk.push(out);
                }
            }
        }

        // Insert new blocks
        {
            for block in scope.iter().cloned() {
                assert!(!self.blocks_map.contains_key(&block));
                let new_block = recv.to().block_insert();
                self.blocks_map.insert(block, RenameDest::Block(new_block));

                self.value_buf.clear();
                self.value_buf.extend(recv.from().block_args(block).iter().cloned());

                // Insert new arguments
                let to = recv.to();
                for arg in self.value_buf.iter().cloned() {
                    let new_arg = to.block_arg_insert(new_block);
                    if !self.values_map.contains_key(&arg) {
                        self.values_map.insert(arg, RenameDest::Value(new_arg));
                    }
                }
            }
        }

        // Propagate values
        self.value_buf.clear();
        self.value_buf.extend(self.values_map.keys());
        for key in self.value_buf.iter() {
            let val = self.values_map[key];
            if let RenameDest::Value(target) = val {
                if let Some(target_dest) = self.values_map.get(&target) {
                    *self.values_map.get_mut(key).unwrap() = *target_dest;
                }
            }
        }

        to_map_values.extend(self.values_map.values().filter_map(|v| {
            match v {
                RenameDest::Value(val) => Some(val),
                _ => None,
            }
        }));

        // Map values
        for val in to_map_values.iter() {
            self.map(recv, *val);
        }

        // Propagate values
        self.value_buf.clear();
        self.value_buf.extend(self.values_map.keys());
        for key in self.value_buf.iter() {
            let val = self.values_map[key];
            if let RenameDest::Value(target) = val {
                if let Some(target_dest) = self.values_map.get(&target) {
                    *self.values_map.get_mut(key).unwrap() = *target_dest;
                }
            }
        }

        let copy_body = |mang: &mut Mangler, recv: &mut R, from_block: Block, to_block: Block| {
            let to_op = recv.map_block_op(from_block);
            let span = recv.from().block_span(from_block);

            // Get and map reads to new values
            mang.value_buf.clear();
            mang.value_buf.extend(recv.from().block_reads(from_block).iter().cloned());
            for n in 0..mang.value_buf.len() {
                let mapped = mang.values_map[&mang.value_buf[n]];
                mang.value_buf[n] = match mapped {
                    RenameDest::Value(value) => value,
                    RenameDest::Block(block) =>
                        recv.to().value(block),
                };
            }

            // Update the new block with kind, span and reads
            let to = recv.to();
            let to_fun = to.fun_mut();
            let data = &mut to_fun.blocks[to_block];

            data.op = Some(to_op);
            data.span = span;

            for read in mang.value_buf.iter().cloned() {
                data.reads.push(read, &mut to_fun.pool.value);
            }

            recv.to().graph_update_block(to_block);
        };

        // Set entry body
        copy_body(self, recv, entry, new_entry);

        // Set new bodies
        for block in scope.iter().cloned() {
            let new_block = self.blocks_map.get(&block).unwrap().block();
            copy_body(self, recv, block, new_block);
        }

        self.values = Some(to_map_values);
        self.scope = Some(scope);
        self.clear();

        new_entry
    }

    fn map<'a, R>(&mut self, recv: &mut R, val: Value) -> RenameDest where R: MangleReceiver<'a> {
        if let Some(to) = self.values_map.get(&val) {
            return *to;
        }

        let kind = recv.from().value_kind(val);
        println!("Map: {} {:?}", val, kind);
        let dest = match kind {
            ValueKind::Const(_) => {
                recv.map_const(val).into()
            },
            ValueKind::PrimOp(prim) => {
                let kind = *recv.from().primop_kind(prim);

                println!("Map PrimOp: {:?}", kind);
                let mut buf: Vec<Value> = recv.from().primop_reads(prim).to_owned();
                println!("Before: {:?}", buf);
                for val in buf.iter_mut() {
                    let to = match self.map(recv, *val) {
                        RenameDest::Value(value) => value,
                        RenameDest::Block(block) => recv.to().value(block),
                    };
                    *val = to;
                }
                println!("After: {:?}", buf);

                recv.to().prim_from_kind(kind, &buf).into()
            },
            ValueKind::Block(block) => {
                self.blocks_map.get(&block).cloned().unwrap_or(RenameDest::Block(block))
            },
            ValueKind::Argument(_block, _num) => {
                recv.map_free_value(val).into()
            },
        };
        self.values_map.insert(val, dest);
        dest
    }

}

pub trait RenameSource {
    fn add(self, dest: RenameDest, mang: &mut Mangler);
}
impl RenameSource for Block {
    fn add(self, dest: RenameDest, mang: &mut Mangler) {
        mang.blocks_map.insert(self, dest);
    }
}
impl RenameSource for Value {
    fn add(self, dest: RenameDest, mang: &mut Mangler) {
        mang.values_map.insert(self, dest);
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum RenameDest {
    Value(Value),
    Block(Block),
    //EntryArg(EntryArg),
}
impl From<Value> for RenameDest {
    fn from(val: Value) -> Self {
        RenameDest::Value(val)
    }
}
impl From<Block> for RenameDest {
    fn from(val: Block) -> Self {
        RenameDest::Block(val)
    }
}
impl RenameDest {
    fn block(&self) -> Block {
        match self {
            RenameDest::Block(block) => *block,
            _ => panic!(),
        }
    }
}

trait MangleReceiver<'b> {
    /// The function container that is the source of the mangle.
    fn from<'a>(&'a mut self) -> &'a Function;

    /// The function builder that is the destination of the mangle.
    fn to<'a>(&'a mut self) -> &'a mut FunctionBuilder<'b>;

    /// Maps a constant.
    /// This should return a value that is usable in the destination
    /// function.
    fn map_const(&mut self, val: Value) -> Value;

    /// Maps a free value.
    /// A free value in this context is a value from outside
    /// the scope of the mangle.
    fn map_free_value(&mut self, val: Value) -> Value;

    /// Maps a block operation. This should return an OpKind that is
    /// usable in the destination function.
    fn map_block_op(&mut self, block: Block) -> OpKind;
}

/// This receiver performs a mangle within a single function container.
struct SingleMangleReceiver<'a, 'b> {
    fun: &'a mut FunctionBuilder<'b>,
}
impl<'b, 'c> MangleReceiver<'b> for SingleMangleReceiver<'c, 'b> {
    fn from<'a>(&'a mut self) -> &'a Function {
        self.fun.fun()
    }
    fn to<'a>(&'a mut self) -> &'a mut FunctionBuilder<'b> {
        self.fun
    }
    fn map_const(&mut self, val: Value) -> Value {
        val
    }
    fn map_free_value(&mut self, val: Value) -> Value {
        val
    }
    fn map_block_op(&mut self, block: Block) -> OpKind {
        self.fun.fun().block_kind(block).unwrap().clone()
    }
}

/// This receiver performs a mangle across to another function container.
/// In the basic case, this is simply a copy across function containers.
struct CopyMangleReceiver<'a, 'b> {
    from: &'a Function,
    to: &'a mut FunctionBuilder<'b>,
}
impl<'b, 'c> MangleReceiver<'b> for CopyMangleReceiver<'c, 'b> {
    fn from<'a>(&'a mut self) -> &'a Function {
        self.from
    }
    fn to<'a>(&'a mut self) -> &'a mut FunctionBuilder<'b> {
        self.to
    }
    fn map_const(&mut self, _val: Value) -> Value {
        unimplemented!()
    }
    fn map_free_value(&mut self, _val: Value) -> Value {
        panic!()
    }
    fn map_block_op(&mut self, _block: Block) -> OpKind {
        unimplemented!()
    }
}


#[cfg(test)]
mod tests {

    use crate::NilTerm;
    use super::Mangler;

    #[test]
    fn test_simple_mangle() {

        let (mut ir, map) = crate::parse_function_map_unwrap("
foo:bar/1 {
    entry(%ret, %thr, %a):
        b1();
    b1():
        %ret(%a);
}
");

        let mut b = ir.builder();

        let mut mangler = Mangler::new();

        let b1 = map.get_block("entry");
        let b1_ret = map.get_value("ret");
        let b1_arg = map.get_value("a");

        let nil_term = b.value(NilTerm);
        mangler.start(b1, &mut b);
        let ret_narg = mangler.add_argument(&mut b);
        mangler.add_rename(b1_ret, ret_narg);
        mangler.add_rename(b1_arg, nil_term);
        let new_b1 = mangler.run(&mut b);

        let after = crate::parse_function_unwrap("
foo:bar/1 {
    entry(%ret):
        b1();
    b1():
        %ret([]);
}
");

        assert!(b.fun().graph_eq(new_b1, &after, after.block_entry()).is_ok());

    }

    #[test]
    fn test_mangle_primop() {

        let (mut ir, map) = crate::parse_function_map_unwrap("
foo:bar/1 {
    entry(%ret, %thr, %a):
        %ret({%a});
}
");

        let mut b = ir.builder();

        let mut mangler = Mangler::new();

        let b1 = map.get_block("entry");
        let b1_arg = map.get_value("a");

        let nil_term = b.value(NilTerm);
        mangler.start(b1, &mut b);
        mangler.copy_entry(&mut b);
        mangler.add_rename(b1_arg, nil_term);
        let new_b1 = mangler.run(&mut b);

        let after = crate::parse_function_unwrap("
foo:bar/1 {
    entry(%ret, %thr, %a):
        %ret({[]});
}
");

        assert!(b.fun().graph_eq(new_b1, &after, after.block_entry()).is_ok());

    }

    #[test]
    fn test_mangle_recursive() {

        let (mut ir, map) = crate::parse_function_map_unwrap("
foo:bar/2 {
    entry(%ret, %thr, %a, %b):
        b2(%a);
    b1(%m):
        %ret(%t);
    b2(%p):
        %ret(%p);

    ! This just exists to have a dummy variable available
    dummy(%t):
        %t();
}
");

        let mut b = ir.builder();

        let mut mangler = Mangler::new();

        let entry = map.get_block("entry");
        let b1 = map.get_block("b1");
        let b2 = map.get_block("b2");
        let vb = map.get_value("b");
        let vt = map.get_value("t");

        mangler.start(entry, &mut b);
        mangler.copy_entry(&mut b);
        mangler.add_rename(b2, b1);
        mangler.add_rename(vt, vb);
        let new_entry = mangler.run(&mut b);

        b.block_set_entry(new_entry);
        println!("{}", b.fun().to_text());

        let after = crate::parse_function_unwrap("
foo:bar/2 {
    entry(%ret, %thr, %a, %b):
        b1(%a);
    b1(%m):
        %ret(%b);
}
");

        assert!(b.fun().graph_eq(new_entry, &after, after.block_entry()).is_ok());

    }

}
