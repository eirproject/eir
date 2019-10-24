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

    actually_mapped: BTreeMap<Value, Value>,
    pub values_map: BTreeMap<Value, (Value, bool)>,

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

            actually_mapped: BTreeMap::new(),
            values_map: BTreeMap::new(),
            //blocks_map: BTreeMap::new(),

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

        self.actually_mapped.clear();
        self.values_map.clear();
        //self.blocks_map.clear();

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

    pub fn add_rename(&mut self, old: Value, new: Value) {
        self.values_map.insert(old, (new, true));
    }
    pub fn add_rename_nofollow(&mut self, old: Value, new: Value) {
        self.values_map.insert(old, (new, false));
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

        // Propagate values
        self.value_buf.clear();
        self.value_buf.extend(self.values_map.keys());
        for key in self.value_buf.iter() {
            let mut acc = *key;
            let mut follow = true;
            while let Some((target, n_follow)) = self.values_map.get(&acc) {
                if acc == *target { break; }
                acc = *target;
                follow = follow && *n_follow;
            }
            *self.values_map.get_mut(&key).unwrap() = (acc, follow);
        }

        // Walk scope
        {
            self.to_walk.push(entry);

            let from = recv.from();
            let graph = from.block_graph();

            while let Some(start_block) = self.to_walk.pop() {
                if scope.contains(&start_block) { continue; }

                let block_value = from.block_value(start_block);
                let block_value_mapped = self.values_map.get(&block_value)
                    .map(|(v, _)| *v).unwrap_or(block_value);

                let block_mapped_opt = from.value_block(block_value_mapped);
                if block_mapped_opt.is_none() { continue; }

                let block = block_mapped_opt.unwrap();
                if scope.contains(&block) { continue; }

                scope.insert(block);

                for read in from.block_reads(block) {
                    from.value_walk_nested_values::<_, ()>(*read, &mut |val| {
                        if let Some((mapped, follow)) = self.values_map.get(&val) {
                            if *follow {
                                if let Some(block) = from.value_block(*mapped) {
                                    self.to_walk.push(block);
                                }
                            }
                        }

                        to_map_values.insert(val);
                        Ok(())
                    }).unwrap();
                }

                for out in graph.outgoing(block) {
                    self.to_walk.push(out);
                }
            }
        }
        println!("Scope: {:?}", scope);

        // Insert new blocks
        {
            for block in scope.iter().cloned() {
                let block_val = recv.from().block_value(block);
                assert!(!self.values_map.contains_key(&block_val));

                let new_block = recv.to().block_insert();
                let new_block_val = recv.to().fun().block_value(new_block);
                self.values_map.insert(block_val, (new_block_val, true));

                self.value_buf.clear();
                self.value_buf.extend(recv.from().block_args(block).iter().cloned());

                // Insert new arguments
                let to = recv.to();
                for arg in self.value_buf.iter().cloned() {
                    let new_arg = to.block_arg_insert(new_block);
                    if !self.values_map.contains_key(&arg) {
                        self.values_map.insert(arg, (new_arg, true));
                    }
                }
            }
        }

        // Propagate values
        self.value_buf.clear();
        self.value_buf.extend(self.values_map.keys());
        for key in self.value_buf.iter() {
            let mut acc = *key;
            while let Some((target, _)) = self.values_map.get(&acc) {
                if acc == *target { break; }
                acc = *target;
            }
            *self.values_map.get_mut(&key).unwrap() = (acc, true);
        }

        to_map_values.extend(self.values_map.values().map(|(v, _)| *v));

        // Map values
        for val in to_map_values.iter() {
            self.map(recv, *val);
        }

        // Propagate values
        self.value_buf.clear();
        self.value_buf.extend(self.values_map.keys());
        for key in self.value_buf.iter() {
            let mut acc = *key;
            while let Some((target, _)) = self.values_map.get(&acc) {
                if acc == *target { break; }
                acc = *target;
            }
            *self.values_map.get_mut(&key).unwrap() = (acc, true);
        }

        let copy_body = |mang: &mut Mangler, recv: &mut R, from_block: Block, to_block: Block| {
            let to_op = recv.map_block_op(from_block);
            let span = recv.from().block_span(from_block);

            // Get and map reads to new values
            mang.value_buf.clear();
            mang.value_buf.extend(recv.from().block_reads(from_block).iter().cloned());
            for n in 0..mang.value_buf.len() {
                let mapped = mang.values_map[&mang.value_buf[n]].0;
                mang.value_buf[n] = mapped;
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
            let block_val = recv.from().block_value(block);
            let new_block_val = self.values_map.get(&block_val).unwrap().0;
            let new_block = recv.to().fun().value_block(new_block_val).unwrap();

            copy_body(self, recv, block, new_block);
        }

        self.values = Some(to_map_values);
        self.scope = Some(scope);
        self.clear();

        new_entry
    }

    fn map<'a, R>(&mut self, recv: &mut R, mut val: Value) -> Value where R: MangleReceiver<'a> {
        if let Some(to) = self.actually_mapped.get(&val) {
            return *to;
        }
        if let Some(to) = self.values_map.get(&val) {
            val = to.0;
        }

        let kind = recv.from().value_kind(val);
        let dest = match kind {
            ValueKind::Const(_) => {
                recv.map_const(val).into()
            },
            ValueKind::PrimOp(prim) => {
                let kind = *recv.from().primop_kind(prim);

                let mut buf: Vec<Value> = recv.from().primop_reads(prim).to_owned();
                for val in buf.iter_mut() {
                    let to = self.map(recv, *val);
                    *val = to;
                }

                recv.to().prim_from_kind(kind, &buf).into()
            },
            ValueKind::Block(block) => {
                let block_val = recv.from().block_value(block);
                if let Some((new_block_val, _)) = self.values_map.get(&block_val).cloned() {
                    new_block_val
                } else {
                    // TODO
                    recv.to().fun().block_value(block)
                }
            },
            ValueKind::Argument(_block, _num) => {
                recv.map_free_value(val).into()
            },
        };
        self.values_map.insert(val, (dest, true));
        self.actually_mapped.insert(val, dest);
        dest
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
    fn simple_mangle() {

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
    fn mangle_primop() {

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
    fn mangle_recursive() {

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
        println!("{}", b.fun().to_text());

        let mut mangler = Mangler::new();

        let entry = map.get_block("entry");
        let b1 = map.get_block("b1");
        let b1_val = b.fun().block_value(b1);
        let b2 = map.get_block("b2");
        let b2_val = b.fun().block_value(b2);
        let vb = map.get_value("b");
        let vt = map.get_value("t");

        mangler.start(entry, &mut b);
        mangler.copy_entry(&mut b);
        mangler.add_rename(b2_val, b1_val);
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
