#[derive(Clone, Debug, PartialEq, Eq)]
pub enum BType {
    None,
    Bool,
    I32,
    Str,
    Struct(String),
}

impl BType {
    pub fn get_struct_name(&self) -> Option<&str> {
        match self {
            BType::Struct(s) => Some(s),
            _ => None,
        }
    }
}
