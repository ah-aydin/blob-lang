#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BlobType {
    Bool,
    I32,
    Custom(String)
}
