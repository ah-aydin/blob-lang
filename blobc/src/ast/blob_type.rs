#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub enum BlobType {
    #[default]
    None,
    Bool,
    I32,
    Custom(String),
}
