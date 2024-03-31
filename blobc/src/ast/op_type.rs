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

impl BooleanOpType {
    pub fn is_boolean_specific(&self) -> bool {
        match self  {
            BooleanOpType::And | BooleanOpType::Or => true,
            _ => false
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum UnaryOpType {
    Negate,
    Not,
}
