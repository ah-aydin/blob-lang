#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinaryOpType {
    And,
    Or,

    Equal,
    NotEqual,
    Greater,
    GreaterOrEqual,
    Less,
    LessOrEqual,

    Add,
    Subtract,
    Multiply,
    Divide,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnaryOpType {
    Negate,
    Not,
}
