#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BlobType {
    I32,
    Custom(String)
}
