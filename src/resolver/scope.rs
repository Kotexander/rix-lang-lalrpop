use super::annotations::DefId;
use crate::strings;
use std::collections::HashMap;

pub struct ScopeStack {
    scopes: Vec<HashMap<strings::Id, DefId>>,
}
impl ScopeStack {
    pub fn new() -> Self {
        Self {
            scopes: vec![HashMap::default()],
        }
    }
    pub fn push(&mut self) {
        self.scopes.push(HashMap::default());
    }
    pub fn pop(&mut self) {
        self.scopes
            .pop()
            .expect("a scope should be on the stack to pop");
    }

    pub fn resolve(&self, ident: strings::Id) -> Option<DefId> {
        for scope in self.scopes.iter().rev() {
            if let Some(res) = scope.get(&ident) {
                return Some(*res);
            }
        }
        None
    }

    /// Define a new value binding in the current scope. Overwrites any existing binding.
    pub fn define(&mut self, id: strings::Id, def: DefId) {
        let scope = self.scopes.last_mut().unwrap();
        scope.insert(id, def);
    }
    /// Declare a new value binding in the current scope.
    /// Returns an error if the type is already defined in the current scope.
    pub fn declare(&mut self, id: strings::Id, def: DefId) -> Result<(), DefId> {
        let scope = self.scopes.last_mut().unwrap();
        if let Some(prev) = scope.get(&id) {
            return Err(*prev);
        }
        scope.insert(id, def);
        Ok(())
    }
}
