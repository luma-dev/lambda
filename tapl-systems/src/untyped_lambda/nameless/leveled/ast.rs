#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct LeveledNamelessTerm(NamelessTerm);
impl LeveledNamelessTerm {
    pub fn new(t: NamelessTerm) -> Self {
        Self(t)
    }
}
