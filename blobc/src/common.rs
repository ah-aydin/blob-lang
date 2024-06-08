use std::fmt::Display;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FileCoords {
    pub line: usize,
    pub col: usize,
}

impl FileCoords {
    pub fn new_offset(&self, line_offset: isize, col_offset: isize) -> FileCoords {
        FileCoords {
            line: (self.line as isize + line_offset) as usize,
            col: (self.col as isize + col_offset) as usize,
        }
    }
}

impl Display for FileCoords {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}, {})", self.line, self.col)
    }
}
