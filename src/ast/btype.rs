#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BType {
    None,
    Bool,
    I64,
    Str,
    Struct(String),
}
