use std::collections::{BTreeMap, BTreeSet};

use bumpalo::{collections::Vec as BVec, Bump};

use libeir_diagnostics::SourceSpan;

use crate::Block;
use crate::ValueKind;
use crate::{Function, FunctionBuilder};

mod receiver;
use receiver::MangleReceiver;

mod datatypes;
#[allow(unused_imports)]
use datatypes::{FromBlock, FromT, FromValue, MangleBlock, MangleValue, ToBlock, ToT, ToValue};

pub use datatypes::FromT as MangleFrom;
pub use datatypes::MangleTarget;
pub use datatypes::ToT as MangleTo;

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

    bump: Option<Bump>,
}

impl Mangler {
    pub fn new() -> Self {
        Mangler {
            entry: None,
            new_entry: None,

            actually_mapped: BTreeMap::new(),
            values_map: BTreeMap::new(),

            value_buf: Vec::new(),

            values: Some(BTreeSet::new()),

            scope: Some(BTreeSet::new()),
            to_walk: Vec::new(),

            bump: Some(Bump::new()),
        }
    }

    /// Clears the mangler for a new use.
    /// This is automatically done by the `start` method.
    fn clear(&mut self) {
        self.entry = None;
        self.new_entry = None;

        self.actually_mapped.clear();
        self.values_map.clear();

        self.value_buf.clear();

        self.values.as_mut().unwrap().clear();

        self.scope.as_mut().unwrap().clear();
        self.to_walk.clear();

        self.bump.as_mut().unwrap().reset();
    }

    pub fn value_map<'a>(&'a self) -> &'a BTreeMap<MangleValue, (MangleValue, bool)> {
        &self.values_map
    }

    /// Clears the mangler of all existing data, and starts a new transaction
    /// for the given entry block.
    pub fn start<F>(&mut self, from_block: F)
    where
        F: Into<MangleBlock>,
    {
        self.clear();
        self.entry = Some(from_block.into());
    }

    pub fn add_rename<O, N>(&mut self, old: O, new: N)
    where
        O: Into<MangleValue>,
        N: Into<MangleValue>,
    {
        self.values_map.insert(old.into(), (new.into(), true));
    }

    /// Adds a value rename that should not be followed in the mangling process.
    /// This is extremely useful in that it allows you to limit the scope of the
    /// mangle.
    /// An example would be allowing you to only mangle the relevant parts when
    /// inlining a closure.
    pub fn add_rename_nofollow<O, N>(&mut self, old: O, new: N)
    where
        O: Into<MangleValue>,
        N: Into<MangleValue>,
    {
        self.values_map.insert(old.into(), (new.into(), false));
    }

    /// Runs lambda mangling on a single function container
    pub fn run(&mut self, fun: &mut FunctionBuilder) -> Block {
        let mut recv = receiver::SingleMangleReceiver { fun };
        self.run_inner(&mut recv)
    }

    // Runs lambda mangling while copying across function containers
    pub fn run_across(&mut self, from: &Function, to: &mut FunctionBuilder) -> Block {
        let mut recv = receiver::CopyMangleReceiver { from, to };
        self.run_inner(&mut recv)
    }

    fn propagate_values(&mut self) {
        self.value_buf.clear();
        self.value_buf.extend(self.values_map.keys());
        for key in self.value_buf.iter() {
            let mut acc = *key;
            let mut follow = true;
            while let Some((target, n_follow)) = self.values_map.get(&acc) {
                if acc == *target {
                    break;
                }
                acc = *target;
                follow = follow && *n_follow;
            }
            if *key == acc {
                self.values_map.remove(key);
            } else {
                *self.values_map.get_mut(key).unwrap() = (acc, follow);
            }
        }
    }

    fn run_inner<'a, 'b, R>(&mut self, recv: &'a mut R) -> Block
    where
        R: MangleReceiver<'b>,
    {
        let bump = self.bump.take().unwrap();

        let mut scope = self.scope.take().unwrap();
        let mut to_map_values = self.values.take().unwrap();

        // Propagate values
        self.propagate_values();

        // Get the mapped entry block
        let mut entry = self.entry.unwrap();
        if let Some((value, follow)) = self
            .values_map
            .get(&entry.map_fun(recv, |f, v| f.block_value(v)))
        {
            if !follow {
                panic!("WARNING: Mangle mapped to entry block due to no-follow");
                //return entry;
            }
            if let Some(b) = value.map_fun(recv, |f, v| f.value_block(v)).transpose_opt() {
                entry = b;
            } else {
                // TODO: In this case, we need to directly return the value the
                // entry block has been mapped to.
                unimplemented!();
            }
        }

        // Walk scope
        {
            self.to_walk.push(entry);

            while let Some(start_block) = self.to_walk.pop() {
                if scope.contains(&start_block) {
                    continue;
                }

                let block_value = start_block.map_fun(recv, |f, v| f.block_value(v));
                let block_value_mapped = self
                    .values_map
                    .get(&block_value)
                    .map(|(v, _)| *v)
                    .unwrap_or(block_value);

                let block_mapped_opt = block_value_mapped
                    .map_fun(recv, |f, v| f.value_block(v))
                    .transpose_opt();
                if block_mapped_opt.is_none() {
                    continue;
                }

                let block = block_mapped_opt.unwrap();
                if scope.contains(&block) {
                    continue;
                }

                scope.insert(block);

                let block_fun = block.fun(recv);
                for read in block_fun
                    .block_reads(block.inner())
                    .iter()
                    .map(|b| block.new_with(b))
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
                    })
                    .unwrap();
                }

                let block_fun = block.fun(recv);
                for out in block_fun.block_graph().outgoing(block.inner()) {
                    self.to_walk.push(block.new_with(out));
                }
            }
        }

        // Insert new blocks
        {
            for block in scope.iter().cloned() {
                let block_val = block.map_fun(recv, |f, b| f.block_value(b));
                assert!(!self.values_map.contains_key(&block_val));

                let new_block = recv.to().block_insert();
                let new_block_val = ToT(recv.to().fun().block_value(new_block));
                self.values_map
                    .insert(block_val, (new_block_val.into(), true));

                self.value_buf.clear();
                let block_fun = block.fun(recv);
                self.value_buf.extend(
                    block_fun
                        .block_args(block.inner())
                        .iter()
                        .map(|v| block.new_with(*v)),
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
            self.map(&bump, recv, *val);
        }

        // Propagate values
        self.propagate_values();

        let copy_body =
            |mang: &mut Mangler, recv: &mut R, from_block: MangleBlock, to_block: ToBlock| {
                let to_op = recv.map_block_op(from_block);
                let loc = from_block.map_fun(recv, |f, b| f.block_location(b)).inner();

                // Get and map reads to new values
                mang.value_buf.clear();
                let from_block_fun = from_block.fun(recv);
                mang.value_buf.extend(
                    from_block_fun
                        .block_reads(from_block.inner())
                        .iter()
                        .map(|v| from_block.new_with(*v)),
                );
                for n in 0..mang.value_buf.len() {
                    let v = mang.value_buf[n];
                    let mapped = mang.values_map.get(&v).map(|v| v.0).unwrap_or(v);
                    mang.value_buf[n] = mapped;
                }

                // Update the new block with kind, span and reads
                let to = recv.to();
                let to_fun = to.fun_mut();
                let data = &mut to_fun.blocks[to_block.inner()];

                data.op = Some(to_op);
                data.location = loc;

                for read in mang.value_buf.iter() {
                    data.reads
                        .push(read.to().unwrap().inner(), &mut to_fun.pool.value);
                }

                recv.to().graph_update_block(to_block.inner());
            };

        // Set new bodies
        for block in scope.iter().cloned() {
            let block_val = block.map_fun(recv, |f, b| f.block_value(b));
            let new_block_val_either = self.values_map.get(&block_val).unwrap().0;
            let new_block_val = new_block_val_either
                .to()
                .expect("attempted to set body on from block");
            let new_block = new_block_val.map_fun(recv, |f, b| f.value_block(b).unwrap());

            copy_body(self, recv, block, new_block);
        }

        let entry = self.entry.unwrap();
        let entry_val = entry.map_fun(recv, |f, v| f.block_value(v));
        let new_entry_val = self
            .values_map
            .get(&entry_val)
            .map(|v| v.0)
            .unwrap_or(entry_val)
            .to()
            .unwrap();
        let new_entry = new_entry_val.map_fun(recv, |f, v| f.value_block(v).unwrap());

        self.values = Some(to_map_values);
        self.scope = Some(scope);
        self.bump = Some(bump);
        self.clear();

        new_entry.inner()
    }

    fn map<'a, 'b, R>(&mut self, bump: &Bump, recv: &'a mut R, mut val: MangleValue) -> ToValue
    where
        R: MangleReceiver<'b>,
    {
        if let Some(to) = self.actually_mapped.get(&val) {
            return *to;
        }
        if let Some(to) = self.values_map.get(&val) {
            val = to.0;
        }

        let kind = val.map_fun(recv, |f, v| f.value_kind(v));
        let locs = val.map_fun(recv, |f, v| f.value_locations(v));
        let dest: ToValue = match kind.inner() {
            ValueKind::Const(_) => recv.map_const(val),
            ValueKind::PrimOp(prim) => {
                let prim = val.new_with(prim);
                let kind = prim
                    .map_fun(recv, |f, _v| *f.primop_kind(prim.inner()))
                    .inner();

                let span = locs
                    .inner()
                    .map(|spans| spans.first().copied().unwrap_or(SourceSpan::UNKNOWN))
                    .unwrap_or(SourceSpan::UNKNOWN);

                let mut buf = BVec::new_in(bump);
                {
                    let fun = prim.fun(recv);
                    buf.extend(fun.primop_reads(prim.inner()).iter().cloned())
                }

                for value in buf.iter_mut() {
                    let value_m = prim.new_with(*value);
                    *value = self.map(bump, recv, value_m).inner();
                }

                recv.to().prim_from_kind(span, kind, &buf).into()
            }
            ValueKind::Block(block) => {
                let block = val.new_with(block);
                let block_val = block.map_fun(recv, |f, b| f.block_value(b));
                let new = if let Some((new_block_val, _)) = self.values_map.get(&block_val).cloned()
                {
                    new_block_val
                } else {
                    block.map_fun(recv, |f, b| f.block_value(b))
                };

                new.to()
                    .expect("mangle reached block in target with nofollow and no replacement")
            }
            ValueKind::Argument(_block, _num) => recv.map_free_value(val).into(),
        };
        self.values_map.insert(val, (dest.into(), true));
        self.actually_mapped.insert(val, dest);
        dest
    }
}
