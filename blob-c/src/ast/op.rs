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

pub trait OpBTypes {
    fn get_supported_btypes(&self) -> Vec<BType>;
    fn get_result_btype(&self) -> BType;
}

impl OpBTypes for BinaryOp {
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
            | BinaryOp::Lte => vec![BType::I32],
            BinaryOp::BooleanOr | BinaryOp::BooleanAnd => vec![BType::Bool],
            BinaryOp::Eq | BinaryOp::Neq => vec![BType::Bool, BType::I32],
        }
    }

    fn get_result_btype(&self) -> BType {
        match self {
            BinaryOp::Add
            | BinaryOp::Sub
            | BinaryOp::Mul
            | BinaryOp::Div
            | BinaryOp::BitwiseOr
            | BinaryOp::BitwiseAnd => BType::I32,
            BinaryOp::BooleanOr
            | BinaryOp::BooleanAnd
            | BinaryOp::Eq
            | BinaryOp::Neq
            | BinaryOp::Gt
            | BinaryOp::Gte
            | BinaryOp::Lt
            | BinaryOp::Lte => BType::Bool,
        }
    }
}

impl OpBTypes for UnaryOp {
    fn get_supported_btypes(&self) -> Vec<BType> {
        match self {
            UnaryOp::Neg => vec![BType::I32],
            UnaryOp::Not => vec![BType::Bool],
        }
    }

    fn get_result_btype(&self) -> BType {
        match self {
            UnaryOp::Neg => BType::I32,
            UnaryOp::Not => BType::Bool,
        }
    }
}
