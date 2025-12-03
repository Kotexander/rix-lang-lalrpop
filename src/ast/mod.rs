pub mod debug;
mod expr;
mod instr;
mod item;
mod typ;

pub use expr::*;
pub use instr::*;
pub use item::*;
pub use typ::*;

use crate::strings;

#[derive(Debug, Clone, Copy)]
pub struct Span {
    pub start: usize,
    pub end: usize,
}
impl From<Span> for std::ops::Range<usize> {
    fn from(val: Span) -> Self {
        val.start..val.end
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct NodeId(usize);
#[derive(Debug, Clone, Copy, Default)]
pub struct NodeIdGen(usize);
impl NodeIdGen {
    pub fn next_id(&mut self) -> NodeId {
        let id = self.0;
        self.0 += 1;
        NodeId(id)
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Node<T> {
    pub kind: T,
    pub span: Span,
    pub id: NodeId,
}
impl<T> Node<T> {
    pub fn new(start: usize, kind: T, end: usize, id: NodeId) -> Self {
        Self {
            kind,
            span: Span { start, end },
            id,
        }
    }
}

pub type Ident = Node<strings::Id>;

#[derive(Default)]
pub struct AstBuilder {
    pub node_id_gen: NodeIdGen,
    pub interner: strings::Interner,
    // pub strings: Vec<strings::Id>,
}
