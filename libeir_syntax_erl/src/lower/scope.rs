use libeir_util::hashmap_stack::HashMapStack;

use libeir_ir::{
    Value as IrValue,
};

use libeir_intern::{ Ident };

use super::LowerError;

pub fn is_wildcard(ident: Ident) -> bool {
    ident.name.as_str().starts_with("_")
}

pub struct ScopeToken {
    /// The position in the stack of the referenced scope.
    height: usize,
}

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

    pub fn resolve(&mut self, ident: Ident) -> Result<IrValue, LowerError> {
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

    pub fn height(&self) -> usize {
        self.stack.height()
    }

}
