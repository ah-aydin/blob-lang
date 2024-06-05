#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum BType {
    None,
    Bool(bool),
    I64(i64),
}
