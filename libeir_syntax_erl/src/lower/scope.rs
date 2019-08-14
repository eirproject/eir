use std::collections::{ HashMap, HashSet };

use libeir_util::hashmap_stack::HashMapStack;

use libeir_ir::{
    Value as IrValue,
    Block as IrBlock,
    FunctionBuilder,
};

use libeir_intern::{ Ident };

use super::{ LowerError, LowerCtx };

pub fn is_wildcard(ident: Ident) -> bool {
    ident.name.as_str() == "_"
}

pub struct ScopeToken {
    /// The position in the stack of the referenced scope.
    height: usize,
}

#[derive(Debug)]
pub struct ScopeTracker {
    // FIXME: Annoying that we have to store the key in the value.
    // Fix when get_key_value makes it into stable.
    stack: HashMapStack<Ident, (Ident, IrValue)>,
}

impl ScopeTracker {

    pub fn new() -> Self {
        ScopeTracker {
            stack: HashMapStack::new(),
        }
    }

    /// This will push a new scope, returning a token which is required
    /// when later popping the token off the stack.
    pub fn push(&mut self) -> ScopeToken {
        self.stack.push();
        ScopeToken {
            height: self.stack.height(),
        }
    }

    /// This will pop all scopes above and including the provided token.
    /// Popping a token after one of its predecessors has been popped
    /// is illegal.
    pub fn pop(&mut self, token: ScopeToken) {
        assert!(self.stack.height() >= token.height);
        while self.stack.height() >= token.height {
            self.stack.pop();
        }
    }

    pub fn pop_take(&mut self, token: ScopeToken) -> HashMap<Ident, IrValue> {
        assert!(self.stack.height() >= token.height);

        let mut ret = HashMap::new();
        for layer_n in (token.height - 1)..self.stack.height() {
            let layer = self.stack.layer(layer_n);
            for (key, value) in layer.iter() {
                ret.insert(*key, value.1);
            }
        }

        self.pop(token);
        ret
    }

    pub fn resolve(&self, ident: Ident) -> Result<IrValue, LowerError> {
        if let Some(val) = self.stack.get(&ident) {
            Ok(val.1)
        } else {
            Err(LowerError::UnresolvedVariable { span: ident.span })
        }
    }

    pub fn bind(&mut self, ident: Ident, val: IrValue) -> Result<(), LowerError> {
        if is_wildcard(ident) {
            Ok(())
        } else {
            if let Some(prev_val) = self.stack.get(&ident) {
                Err(LowerError::AlreadyBound { new: ident.span, old: prev_val.0.span })
            } else {
                self.stack.insert(ident, (ident, val));
                Ok(())
            }
        }
    }

    pub fn bind_shadow(&mut self, ident: Ident, val: IrValue) -> Result<(), LowerError> {
        if is_wildcard(ident) {
            Ok(())
        } else {
            let ret = if let Some(prev_val) = self.stack.get(&ident) {
                Err(LowerError::ShadowingBind {
                    new: ident.span, old: prev_val.0.span })
            } else {
                Ok(())
            };
            self.stack.insert(ident, (ident, val));
            ret
        }
    }

    pub fn height(&self) -> usize {
        self.stack.height()
    }

}

#[derive(Debug)]
struct Branch {
    cont_block: IrBlock,
    ret: IrValue,
    binds: HashMap<Ident, IrValue>,
}

/// Utility for performing a scope merge, as is
/// needed for case, if and receive branches.
/// This will insert variable binding in the top
/// scope if the binding is found to be present in
/// every branch.
pub(super) struct ScopeMerge {
    branches: Vec<Branch>,
}

impl ScopeMerge {

    pub fn new() -> Self {
        ScopeMerge {
            branches: Vec::new(),
        }
    }

    pub fn branch(&mut self, cont: IrBlock, ret: IrValue,
                  binds: HashMap<Ident, IrValue>) {
        self.branches.push(Branch {
            cont_block: cont,
            ret,
            binds,
        });
    }

    pub fn finish(&mut self, ctx: &mut LowerCtx,
                  b: &mut FunctionBuilder) -> (IrBlock, IrValue) {

        // Find all bindings that are common to all branches
        let common_vars = if self.branches.len() > 0 {
            let mut common_set: HashSet<_> =
                self.branches[0].binds.keys()
                .cloned().collect();
            common_set.retain(
                |ident| self.branches.iter().all(
                    |res| res.binds.contains_key(ident)));
            common_set.drain().collect()
        } else {
            Vec::new()
        };

        // Insert the join block and the return argument
        let join_block = b.block_insert();
        let ret = b.block_arg_insert(join_block);

        // Insert common bindings on join block
        for var in common_vars.iter() {
            let val = b.block_arg_insert(join_block);
            ctx.bind(*var, val);
        }

        // Create calls from all branches to join block
        let mut val_buf = Vec::new();
        for result in self.branches.iter() {
            val_buf.clear();
            val_buf.push(result.ret);
            for var in common_vars.iter() {
                val_buf.push(result.binds[var]);
            }
            b.op_call(result.cont_block, join_block, &val_buf);
        }

        (join_block, ret)
    }

}
