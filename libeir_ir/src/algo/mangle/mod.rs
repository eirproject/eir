use std::collections::{ BTreeSet, BTreeMap };

use crate::{ Function, FunctionBuilder };
use crate::{ Value, Block };
use crate::{ ValueKind };

mod receiver;
use receiver::MangleReceiver;

mod datatypes;
#[allow(unused_imports)]
use datatypes::{
    FromT, ToT,
    MangleValue, FromValue, ToValue,
    MangleBlock, FromBlock, ToBlock,
};

pub use datatypes::MangleTarget;
pub use datatypes::ToT as MangleTo;
pub use datatypes::FromT as MangleFrom;

#[cfg(test)]
mod tests;

/// Utility struct implementing the lambda mangling primitive.
/// Supports both mangling within a single function container, and
/// across function containers, implemented by `run` and `run_across`
/// respectively.
pub struct Mangler {
    entry: Option<MangleBlock>,
    new_entry: Option<ToBlock>,

    /// Cache to reduce time complexity of `Self::map`
    actually_mapped: BTreeMap<MangleValue, ToValue>,

    pub values_map: BTreeMap<MangleValue, (MangleValue, bool)>,

    value_buf: Vec<MangleValue>,

    values: Option<BTreeSet<MangleValue>>,

    scope: Option<BTreeSet<MangleBlock>>,
    to_walk: Vec<MangleBlock>,
}

impl Mangler {

    pub fn new() -> Self {
        Mangler {
            entry: None,
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
        self.new_entry = None;

        self.actually_mapped.clear();
        self.values_map.clear();
        //self.blocks_map.clear();

        self.value_buf.clear();

        self.values.as_mut().unwrap().clear();

        self.scope.as_mut().unwrap().clear();
        self.to_walk.clear();
    }

    pub fn value_map<'a>(&'a self) -> &'a BTreeMap<MangleValue, (MangleValue, bool)> {
        &self.values_map
    }

    /// Clears the mangler of all existing data, and starts a new transaction
    /// for the given entry block.
    pub fn start<F>(
        &mut self,
        from_block: F,
    )
    where
        F: Into<MangleBlock>,
    {
        self.clear();
        self.entry = Some(from_block.into());
    }

    pub fn new_entry<T>(&mut self, block: T) where T: Into<ToBlock> {
        assert!(self.new_entry.is_none());
        self.new_entry = Some(block.into());
    }

    #[allow(dead_code)]
    fn new_entry_copy_inner<'a, F, R>(
        &mut self,
        recv: &'a mut R,
    )
    where R: MangleReceiver<'a>,
    {
        let original_entry = self.entry.unwrap();
        println!("CopyEntry {}", original_entry);

        assert!(self.new_entry.is_none());
        let new_entry = ToT(recv.to().block_insert());

        let args_len = original_entry.fun(recv)
            .block_args(original_entry.inner()).len();
        for _ in 0..args_len {
            recv.to().block_arg_insert(new_entry.inner());
        }

        let mut entry = original_entry;
        loop {
            let value = entry.map_fun(recv, |fun, v| fun.block_value(v));
            if let Some(next) = self.values_map.get(&value) {
                if let Some(next_block) = next.0
                    .map_fun(recv, |fun, v| fun.value_block(v))
                    .transpose_opt()
                {
                    entry = next_block;

                    assert_eq!(
                        entry.fun(recv).block_args(entry.inner()).len(),
                        args_len,
                    );
                    for n in 0..args_len {
                        let from_arg = entry.map_fun(recv, |fun, v| fun.block_args(v)[n]);
                        let to_arg = new_entry.map_fun(recv, |fun, v| fun.block_args(v)[n]);

                        if self.values_map.contains_key(&from_arg) {
                            println!(
                                "{} -> {} (ALREADY {} -> {})",
                                from_arg, to_arg,
                                from_arg, self.values_map[&from_arg].0,
                            );
                        } else {
                            println!(
                                "{} -> {}",
                                from_arg, to_arg,
                            );
                        }

                        if !self.values_map.contains_key(&from_arg) {
                            self.values_map.insert(from_arg, (to_arg.into(), true));
                        }
                    }

                    continue;
                } else {
                    panic!()
                }
            }
            break;
        }
    }

    pub fn add_rename<O, N>(
        &mut self,
        old: O,
        new: N,
    ) where
        O: Into<MangleValue>,
        N: Into<MangleValue>,
    {
        self.values_map.insert(old.into(), (new.into(), true));
    }
    pub fn add_rename_nofollow<O, N>(
        &mut self,
        old: O,
        new: N,
    ) where
        O: Into<MangleValue>,
        N: Into<MangleValue>,
    {
        self.values_map.insert(old.into(), (new.into(), false));
    }

    /// Runs lambda mangling on a single function container
    pub fn run(&mut self, fun: &mut FunctionBuilder) -> Block {
        let mut recv = receiver::SingleMangleReceiver {
            fun,
        };
        self.run_inner(&mut recv)
    }

    // Runs lambda mangling while copying across function containers
    pub fn run_across(&mut self, from: &Function, to: &mut FunctionBuilder) -> Block {
        let mut recv = receiver::CopyMangleReceiver {
            from,
            to,
        };
        self.run_inner(&mut recv)
    }

    fn propagate_values(&mut self) {
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
            if *key == acc {
                self.values_map.remove(key);
            } else {
                *self.values_map.get_mut(key).unwrap() = (acc, follow);
            }
        }

        //self.value_buf.clear();
        //self.value_buf.extend(self.values_map.keys());
        //for key in self.value_buf.iter() {
        //    let mut acc = *key;
        //    while let Some((target, _)) = self.values_map.get(&acc) {
        //        if acc == *target { break; }
        //        acc = *target;
        //    }
        //    *self.values_map.get_mut(&key).unwrap() = (acc, true);
        //}
    }

    fn run_inner<'a, 'b, R>(&mut self, recv: &'a mut R) -> Block where R: MangleReceiver<'b> {
        let mut scope = self.scope.take().unwrap();
        let mut to_map_values = self.values.take().unwrap();

        // Propagate values
        self.propagate_values();

        // Get the mapped entry block
        let mut entry = self.entry.unwrap();
        if let Some((value, follow)) = self.values_map.get(
            &entry.map_fun(recv, |f, v| f.block_value(v)))
        {
            if !follow {
                panic!("WARNING: Mangle mapped to entry block due to no-follow");
                //return entry;
            }
            if let Some(b) = value.map_fun(recv, |f, v| f.value_block(v))
                .transpose_opt()
            {
                entry = b;
            } else {
                // TODO: In this case, we need to directly return the value the
                // entry block has been mapped to.
                unimplemented!();
            }
        }

        // Insert new entry block
        //let new_entry = self.new_entry.unwrap();
        //{
        //    let to = recv.to();
        //    for _ in 0..self.num_args {
        //        to.block_arg_insert(new_entry);
        //    }
        //}

        // Walk scope
        {
            self.to_walk.push(entry);

            while let Some(start_block) = self.to_walk.pop() {
                if scope.contains(&start_block) { continue; }

                let block_value = start_block.map_fun(recv, |f, v| f.block_value(v));
                let block_value_mapped = self.values_map.get(&block_value)
                    .map(|(v, _)| *v).unwrap_or(block_value);

                let block_mapped_opt = block_value_mapped
                    .map_fun(recv, |f, v| f.value_block(v))
                    .transpose_opt();
                if block_mapped_opt.is_none() { continue; }

                let block = block_mapped_opt.unwrap();
                if scope.contains(&block) { continue; }

                scope.insert(block);

                let block_fun = block.fun(recv);
                for read in block_fun.block_reads(block.inner())
                    .iter().map(|b| block.new_with(b))
                {
                    let fun = read.fun(recv);
                    fun.value_walk_nested_values::<_, ()>(*read.inner(), &mut |val| {
                        let val = read.new_with(val);
                        if let Some((mapped, follow)) = self.values_map.get(&val) {
                            if *follow {
                                if let Some(block) = mapped
                                    .map_fun(recv, |f, v| f.value_block(v))
                                    .transpose_opt()
                                {
                                    self.to_walk.push(block);
                                }
                            }
                        }

                        to_map_values.insert(val);
                        Ok(())
                    }).unwrap();
                }

                let block_fun = block.fun(recv);
                for out in block_fun.block_graph().outgoing(block.inner()) {
                    self.to_walk.push(block.new_with(out));
                }
            }
        }
        println!("Scope: {:?}", scope);

        // Insert new blocks
        {
            for block in scope.iter().cloned() {
                let block_val = block.map_fun(recv, |f, b| f.block_value(b));
                assert!(!self.values_map.contains_key(&block_val));

                let new_block = recv.to().block_insert();
                let new_block_val = ToT(recv.to().fun().block_value(new_block));
                self.values_map.insert(block_val, (new_block_val.into(), true));

                self.value_buf.clear();
                let block_fun = block.fun(recv);
                self.value_buf.extend(
                    block_fun.block_args(block.inner())
                        .iter().map(|v| block.new_with(*v))
                );

                // Insert new arguments
                let to = recv.to();
                for arg in self.value_buf.iter().cloned() {
                    let new_arg = to.block_arg_insert(new_block);
                    if !self.values_map.contains_key(&arg) {
                        self.values_map.insert(arg, (ToT(new_arg).into(), true));
                    }
                }
            }
        }

        // Propagate values
        self.propagate_values();

        // Map values
        to_map_values.extend(self.values_map.values().map(|(v, _)| *v));
        for val in to_map_values.iter() {
            self.map(recv, *val);
        }

        // Propagate values
        self.propagate_values();

        //println!("{}", crate::function_format!(
        //    recv.from(), "{:#?}", &self.values_map));

        let copy_body = |mang: &mut Mangler, recv: &mut R, from_block: MangleBlock, to_block: ToBlock| {
            let to_op = recv.map_block_op(from_block);
            let loc = from_block.map_fun(recv, |f, b| f.block_location(b)).inner();

            // Get and map reads to new values
            mang.value_buf.clear();
            let from_block_fun = from_block.fun(recv);
            mang.value_buf.extend(
                from_block_fun.block_reads(from_block.inner())
                    .iter().map(|v| from_block.new_with(*v))
            );
            for n in 0..mang.value_buf.len() {
                let v = mang.value_buf[n];
                let mapped = mang.values_map
                    .get(&v)
                    .map(|v| v.0)
                    .unwrap_or(v);
                mang.value_buf[n] = mapped;
            }

            // Update the new block with kind, span and reads
            let to = recv.to();
            let to_fun = to.fun_mut();
            let data = &mut to_fun.blocks[to_block.inner()];

            data.op = Some(to_op);
            data.location = loc;

            for read in mang.value_buf.iter() {
                data.reads.push(read.to().unwrap().inner(), &mut to_fun.pool.value);
            }

            recv.to().graph_update_block(to_block.inner());
        };

        // Set entry body
        //copy_body(self, recv, entry, new_entry);

        // Set new bodies
        for block in scope.iter().cloned() {
            let block_val = block.map_fun(recv, |f, b| f.block_value(b));
            let new_block_val_either = self.values_map.get(&block_val).unwrap().0;
            let new_block_val = new_block_val_either.to()
                .expect("attempted to set body on from block");
            let new_block = new_block_val
                .map_fun(recv, |f, b| f.value_block(b).unwrap());

            copy_body(self, recv, block, new_block);
        }

        let entry = self.entry.unwrap();
        let entry_val = entry.map_fun(recv, |f, v| f.block_value(v));
        let new_entry_val = self.values_map.get(&entry_val)
            .map(|v| v.0)
            .unwrap_or(entry_val)
            .to().unwrap();
        let new_entry = new_entry_val
            .map_fun(recv, |f, v| f.value_block(v).unwrap());

        self.values = Some(to_map_values);
        self.scope = Some(scope);
        self.clear();

        new_entry.inner()
    }

    fn map<'a, 'b, R>(&mut self, recv: &'a mut R, mut val: MangleValue) -> ToValue where R: MangleReceiver<'b> {
        if let Some(to) = self.actually_mapped.get(&val) {
            return *to;
        }
        if let Some(to) = self.values_map.get(&val) {
            val = to.0;
        }

        let kind = val.map_fun(recv, |f, v| f.value_kind(v));
        let dest: ToValue = match kind.inner() {
            ValueKind::Const(_) => {
                recv.map_const(val)
            },
            ValueKind::PrimOp(prim) => {
                let prim = val.new_with(prim);
                let kind = prim
                    .map_fun(recv, |f, _v| *f.primop_kind(prim.inner()))
                    .inner();

                let mut buf: Vec<Value>  = {
                    let fun = prim.fun(recv);
                    fun.primop_reads(prim.inner()).to_owned()
                };
                for value in buf.iter_mut() {
                    let value_m = prim.new_with(*value);
                    *value = self.map(recv, value_m).inner();
                }

                recv.to().prim_from_kind(kind, &buf).into()
            },
            ValueKind::Block(block) => {
                let block = val.new_with(block);
                let block_val = block.map_fun(recv, |f, b| f.block_value(b));
                let new = if let Some((new_block_val, _)) = self.values_map
                    .get(&block_val).cloned()
                {
                    new_block_val
                } else {
                    block.map_fun(recv, |f, b| f.block_value(b))
                };

                new.to()
                    .expect("mangle reached block in target with nofollow and no replacement")
            },
            ValueKind::Argument(_block, _num) => {
                recv.map_free_value(val).into()
            },
        };
        self.values_map.insert(val, (dest.into(), true));
        self.actually_mapped.insert(val, dest);
        dest
    }

}
