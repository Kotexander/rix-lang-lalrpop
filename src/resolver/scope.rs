use super::resource::{Type, Value};
use crate::strings;
use std::collections::HashMap;

#[derive(Default)]
struct Scope {
    val_bindings: HashMap<strings::Id, Value>,
    typ_bindings: HashMap<strings::Id, Type>,
}

pub struct ScopeStack {
    scopes: Vec<Scope>,
}
impl ScopeStack {
    pub fn new() -> Self {
        Self {
            scopes: vec![Scope::default()],
        }
    }
    pub fn push(&mut self) {
        self.scopes.push(Scope::default());
    }
    pub fn pop(&mut self) {
        self.scopes
            .pop()
            .expect("a scope should be on the stack to pop");
    }

    pub fn get_val(&self, ident: strings::Id) -> Option<Value> {
        for scope in self.scopes.iter().rev() {
            if let Some(res) = scope.val_bindings.get(&ident) {
                return Some(*res);
            }
        }
        None
    }
    pub fn get_typ(&self, ident: strings::Id) -> Option<Type> {
        for scope in self.scopes.iter().rev() {
            if let Some(typ) = scope.typ_bindings.get(&ident) {
                return Some(*typ);
            }
        }
        None
    }

    /// Define a new type binding in the current scope. Overwrites any existing binding.
    pub fn define_typ(&mut self, id: strings::Id, typ: Type) {
        let scope = self.scopes.last_mut().unwrap();
        scope.typ_bindings.insert(id, typ);
    }
    /// Declare a new type binding in the current scope.
    /// Returns an error if the type is already defined in the current scope.
    pub fn declare_typ(&mut self, id: strings::Id, typ: Type) -> Result<(), Type> {
        if let Some(prev) = self.scopes.last_mut().unwrap().typ_bindings.get(&id) {
            return Err(*prev);
        }

        self.define_typ(id, typ);
        Ok(())
    }

    /// Define a new value binding in the current scope. Overwrites any existing binding.
    pub fn define_val(&mut self, id: strings::Id, val: Value) {
        let scope = self.scopes.last_mut().unwrap();
        scope.val_bindings.insert(id, val);
    }
    /// Declare a new value binding in the current scope.
    /// Returns an error if the type is already defined in the current scope.
    pub fn declare_val(&mut self, id: strings::Id, val: Value) -> Result<(), Value> {
        if let Some(prev) = self.scopes.last_mut().unwrap().val_bindings.get(&id) {
            return Err(*prev);
        }
        self.define_val(id, val);
        Ok(())
    }
}
