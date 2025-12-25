use super::resource::Value;
use crate::ast::NodeId;
use std::collections::HashMap;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct DefId(usize);
pub struct Annotations {
    refs: HashMap<NodeId, DefId>,
    defs: Vec<Value>,
    consts: HashMap<NodeId, u64>,
}
impl Annotations {
    pub fn new() -> Self {
        Self {
            defs: Vec::new(),
            refs: HashMap::new(),
            consts: HashMap::new(),
        }
    }
    pub fn resolve(&self, node: NodeId) -> &Value {
        self.get(self.def_of(node))
    }
    pub fn resolve_mut(&mut self, node: NodeId) -> &mut Value {
        self.get_mut(self.def_of(node))
    }

    pub fn def_of(&self, node: NodeId) -> DefId {
        *self.refs.get(&node).unwrap()
    }
    pub fn get(&self, def: DefId) -> &Value {
        &self.defs[def.0]
    }
    pub fn get_mut(&mut self, def: DefId) -> &mut Value {
        &mut self.defs[def.0]
    }

    pub fn bind(&mut self, node: NodeId, val: Value) -> DefId {
        debug_assert!(
            !self.refs.contains_key(&node),
            "{:?} is already bound",
            node
        );

        let def = DefId(self.defs.len());
        self.defs.push(val);
        self.refs.insert(node, def);
        def
    }
    pub fn set_const(&mut self, node: NodeId, value: u64) {
        self.consts.insert(node, value);
    }
    pub fn get_const(&self, node: NodeId) -> Option<u64> {
        self.consts.get(&node).copied()
    }
    pub fn add_ref(&mut self, node: NodeId, def: DefId) {
        debug_assert!(
            !self.refs.contains_key(&node),
            "{:?} is already bound",
            node
        );
        self.refs.insert(node, def);
    }
}
