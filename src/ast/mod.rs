pub mod blob_type;
pub mod expr;
pub mod op_type;
pub mod stmt;

#[derive(Default, Debug, PartialEq, Eq, Clone)]
pub struct FileCoords {
    pub line: usize,
    pub col: usize,
}
