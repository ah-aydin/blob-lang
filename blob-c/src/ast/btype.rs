#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BType {
    None,
    Bool,
    I32,
    Str,
    Struct(String),
}
