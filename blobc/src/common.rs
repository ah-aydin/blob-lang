use std::fmt::Display;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FileCoords {
    pub line: usize,
    pub col: usize,
}

impl Display for FileCoords {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}, {})", self.line, self.col)
    }
}
