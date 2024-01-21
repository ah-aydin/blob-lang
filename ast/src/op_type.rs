#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BinaryOpType {
    Add,
    Subtract,
    Multiply,
    Divide,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum BooleanOpType {
    And,
    Or,
    Equal,
    NotEqual,
    Greater,
    GreaterOrEqual,
    Less,
    LessOrEqual,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnaryOpType {
    Negate,
    Not,
}
