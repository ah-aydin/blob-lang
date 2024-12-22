use super::btype::{BType, BTypeWrapper};

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
    fn get_supported_btype_wrappers(&self) -> Vec<BTypeWrapper>;
    fn get_result_btype_wrapper(&self) -> BTypeWrapper;
}

impl OpBTypes for BinaryOp {
    fn get_supported_btype_wrappers(&self) -> Vec<BTypeWrapper> {
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
            | BinaryOp::Lte => vec![BTypeWrapper::new(BType::I64)],
            BinaryOp::BooleanOr | BinaryOp::BooleanAnd => vec![BTypeWrapper::new(BType::Bool)],
            BinaryOp::Eq | BinaryOp::Neq => vec![
                BTypeWrapper::new(BType::Bool),
                BTypeWrapper::new(BType::I64),
            ],
        }
    }

    fn get_result_btype_wrapper(&self) -> BTypeWrapper {
        match self {
            BinaryOp::Add
            | BinaryOp::Sub
            | BinaryOp::Mul
            | BinaryOp::Div
            | BinaryOp::BitwiseOr
            | BinaryOp::BitwiseAnd => BTypeWrapper::new(BType::I64),
            BinaryOp::BooleanOr
            | BinaryOp::BooleanAnd
            | BinaryOp::Eq
            | BinaryOp::Neq
            | BinaryOp::Gt
            | BinaryOp::Gte
            | BinaryOp::Lt
            | BinaryOp::Lte => BTypeWrapper::new(BType::Bool),
        }
    }
}

impl OpBTypes for UnaryOp {
    fn get_supported_btype_wrappers(&self) -> Vec<BTypeWrapper> {
        match self {
            UnaryOp::Neg => vec![BTypeWrapper::new(BType::I64)],
            UnaryOp::Not => vec![BTypeWrapper::new(BType::Bool)],
        }
    }

    fn get_result_btype_wrapper(&self) -> BTypeWrapper {
        match self {
            UnaryOp::Neg => BTypeWrapper::new(BType::I64),
            UnaryOp::Not => BTypeWrapper::new(BType::Bool),
        }
    }
}
