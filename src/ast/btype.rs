use std::fmt::Debug;

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BType {
    Void,
    Bool,
    I64,
    Struct(String),
}

#[derive(Clone, PartialEq, Eq)]
pub struct BTypeWrapper {
    pub btype: BType,
    pub is_ref: bool,
}

impl BTypeWrapper {
    pub fn void() -> BTypeWrapper {
        BTypeWrapper {
            btype: BType::Void,
            is_ref: false,
        }
    }

    pub fn new(btype: BType) -> BTypeWrapper {
        BTypeWrapper {
            btype,
            is_ref: false,
        }
    }

    pub fn new_ref(btype: BType) -> BTypeWrapper {
        BTypeWrapper {
            btype,
            is_ref: true,
        }
    }

    pub fn into_deref(&self) -> BTypeWrapper {
        BTypeWrapper {
            btype: self.btype.clone(),
            is_ref: false,
        }
    }

    pub fn into_ref(&self) -> BTypeWrapper {
        BTypeWrapper {
            btype: self.btype.clone(),
            is_ref: true,
        }
    }

    pub fn is_type(&self, btype: BType) -> bool {
        self.btype == btype && !self.is_ref
    }

    pub fn is_type_ref(&self, btype: BType) -> bool {
        self.btype == btype && self.is_ref
    }
}

impl Debug for BTypeWrapper {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_ref {
            return write!(f, "&{:?}", self.btype);
        } else {
            return write!(f, "{:?}", self.btype);
        }
    }
}
