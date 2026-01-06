#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Id(usize);

#[derive(Debug)]
pub struct Interner {
    strings: Vec<String>,
}
impl Interner {
    pub fn new() -> Self {
        Self {
            strings: Vec::new(),
        }
    }
    pub fn intern(&mut self, s: &str) -> Id {
        if let Some(i) = self.strings.iter().position(|i| i == s) {
            return Id(i);
        }

        let id = self.strings.len();
        self.strings.push(s.to_owned());
        Id(id)
    }
    pub fn get(&self, id: Id) -> &str {
        &self.strings[id.0]
    }
}
