use super::btype::BType;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    BooleanOr,
    BooleanAnd,
    BitwiseOr,
    BitwiseAnd,
    Eq,
    Neq,
    Gt,
    Gte,
    Lt,
    Lte,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UnaryOp {
    Neg,
    Not,
}

pub trait SupportedBTypes {
    fn get_supported_btypes(&self) -> Vec<BType>;
}

impl SupportedBTypes for BinaryOp {
    fn get_supported_btypes(&self) -> Vec<BType> {
        match self {
            BinaryOp::Add
            | BinaryOp::Sub
            | BinaryOp::Mul
            | BinaryOp::Div
            | BinaryOp::BitwiseOr
            | BinaryOp::BitwiseAnd
            | BinaryOp::Gt
            | BinaryOp::Gte
            | BinaryOp::Lt
            | BinaryOp::Lte => vec![BType::I64],
            BinaryOp::BooleanOr | BinaryOp::BooleanAnd => vec![BType::Bool],
            BinaryOp::Eq | BinaryOp::Neq => vec![BType::Bool, BType::I64],
        }
    }
}

impl SupportedBTypes for UnaryOp {
    fn get_supported_btypes(&self) -> Vec<BType> {
        match self {
            UnaryOp::Neg => vec![BType::I64],
            UnaryOp::Not => vec![BType::Bool],
        }
    }
}
