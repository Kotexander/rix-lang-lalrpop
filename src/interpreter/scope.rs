use super::Value;
use std::collections::HashMap;

pub struct ScopeManager {
    // a stack of scopes
    vars: Vec<HashMap<String, Value>>,
}
impl ScopeManager {
    pub fn new() -> Self {
        Self { vars: vec![] }
    }
    pub fn new_scope<'a>(&'a mut self) -> ScopeGuard<'a> {
        ScopeGuard::new(self)
    }

    fn push(&mut self) {
        self.vars.push(HashMap::new());
    }
    fn pop(&mut self) {
        self.vars.pop();
    }
}
pub struct ScopeGuard<'a>(&'a mut ScopeManager);
impl<'a> ScopeGuard<'a> {
    fn new(scope_manager: &'a mut ScopeManager) -> Self {
        scope_manager.push();
        Self(scope_manager)
    }
    pub fn new_scope<'b>(&'b mut self) -> ScopeGuard<'b> {
        self.0.push();
        ScopeGuard(self.0)
    }
    pub fn set_var(&mut self, name: String, value: Value) {
        if let Some(scope) = self.0.vars.last_mut() {
            scope.insert(name, value);
        } else {
            panic!("No scope to set variable in");
        }
    }
    pub fn get_var(&self, name: &str) -> Option<&Value> {
        for scope in self.0.vars.iter().rev() {
            if let Some(value) = scope.get(name) {
                return Some(value);
            }
        }
        None
    }
}
impl Drop for ScopeGuard<'_> {
    fn drop(&mut self) {
        self.0.pop();
    }
}
