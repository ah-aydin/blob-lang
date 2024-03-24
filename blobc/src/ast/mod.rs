use derive_new::new;

pub mod blob_type;
pub mod expr;
pub mod op_type;
pub mod stmt;

#[derive(Debug, Clone, PartialEq, Eq, new)]
pub struct FileCoords {
    pub line: usize,
    pub col: usize,
}
