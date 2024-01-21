use std::fmt;

pub enum Arm32Reg {
    R0,
    R1,
    R2,
    R3,
    R4,
    R5,
    R6,
    R7,
    R8,
    R9,
    R10,
    FP,
    IP,
    SP,
    LR,
    PC,
    CSRP,
}

impl fmt::Display for Arm32Reg {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{}",
            match self {
                Arm32Reg::R0 => "r0",
                Arm32Reg::R1 => "r1",
                Arm32Reg::R2 => "r2",
                Arm32Reg::R3 => "r3",
                Arm32Reg::R4 => "r4",
                Arm32Reg::R5 => "r5",
                Arm32Reg::R6 => "r6",
                Arm32Reg::R7 => "r7",
                Arm32Reg::R8 => "r8",
                Arm32Reg::R9 => "r9",
                Arm32Reg::R10 => "r10",
                Arm32Reg::FP => "fp",
                Arm32Reg::IP => "ip",
                Arm32Reg::SP => "sp",
                Arm32Reg::LR => "lr",
                Arm32Reg::PC => "pc",
                Arm32Reg::CSRP => "csrp",
            }
        )
    }
}

fn join_regs(regs: &Vec<Arm32Reg>) -> String {
    regs.iter()
        .map(|reg| format!("{}", reg))
        .collect::<Vec<_>>()
        .join(",")
}

#[derive(Debug)]
pub enum Arm32Condition {
    Eq,
    Ne,

    /// Signed >
    Gt,
    /// Signed >=
    Ge,
    /// Signed <
    Lt,
    /// Signed <=
    Le,

    /// Unsigned >
    Hi,
    /// Unsigned >=
    Hs,
    /// Unsigned <
    Lo,
    /// Unsigned <=
    Ls,

    /// Always (dosn't change anything)
    Al,
    None,
}

impl fmt::Display for Arm32Condition {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Arm32Condition::Eq => write!(f, "eq"),
            Arm32Condition::Ne => write!(f, "ne"),
            Arm32Condition::Gt => write!(f, "gt"),
            Arm32Condition::Ge => write!(f, "ge"),
            Arm32Condition::Lt => write!(f, "lt"),
            Arm32Condition::Le => write!(f, "le"),
            Arm32Condition::Hi => write!(f, "hi"),
            Arm32Condition::Hs => write!(f, "hs"),
            Arm32Condition::Lo => write!(f, "lo"),
            Arm32Condition::Ls => write!(f, "ls"),
            Arm32Condition::Al => write!(f, "al"),
            Arm32Condition::None => write!(f, ""),
        }
    }
}

pub enum Arm32Offset {
    Number(String),
    PosReg(Arm32Reg),
    NegReg(Arm32Reg),
}

impl fmt::Display for Arm32Offset {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Arm32Offset::Number(n) => write!(f, "{}", n),
            Arm32Offset::PosReg(reg) => write!(f, "{}", reg),
            Arm32Offset::NegReg(reg) => write!(f, "-{}", reg),
        }
    }
}

pub enum Arm32Ins {
    Label(String),

    /// Arithmetic ``` r1 = r2 + r3 ```
    Add(Arm32Reg, Arm32Reg, Arm32Reg, Arm32Condition),
    /// Arithmetic ``` r1 = r2 + 123 ```
    AddImd(Arm32Reg, Arm32Reg, String, Arm32Condition),
    /// Arithmetic ``` r1 = r2 - r3 ```
    Sub(Arm32Reg, Arm32Reg, Arm32Reg, Arm32Condition),
    /// Arithmetic ``` r1 = r2 - 123 ```
    SubImd(Arm32Reg, Arm32Reg, String, Arm32Condition),
    /// Arithmetic ``` r1 = r2 * r3 ```
    Mul(Arm32Reg, Arm32Reg, Arm32Reg, Arm32Condition),
    /// Arithmetic ``` r1 = r2 * 123 ```
    MulImd(Arm32Reg, Arm32Reg, String, Arm32Condition),
    /// Arithmetic ``` r1 = r2 / r3 // Signed ```
    SDiv(Arm32Reg, Arm32Reg, Arm32Reg, Arm32Condition),
    /// Arithmetic ``` r1 = r2 / 123 // Signed ```
    SDivImd(Arm32Reg, Arm32Reg, Arm32Reg, Arm32Condition),
    /// Arithmetic ``` r1 = r2 / r3 // Unsigned ```
    UDiv(Arm32Reg, Arm32Reg, Arm32Reg, Arm32Condition),
    /// Arithmetic ``` r1 = r2 / 123 // Unsigned ```
    UDivImd(Arm32Reg, Arm32Reg, String, Arm32Condition),

    /// Logic ``` r1 = r2 & ~r3 ```
    Bic(Arm32Reg, Arm32Reg, Arm32Reg, Arm32Condition),
    /// Logic ``` r1 = r2 & ~123 ```
    BicImd(Arm32Reg, Arm32Reg, String, Arm32Condition),
    /// Logic ``` r1 = r2 & r3 ```
    And(Arm32Reg, Arm32Reg, Arm32Reg, Arm32Condition),
    /// Logic ``` r1 = r2 & 123 ```
    AndImd(Arm32Reg, Arm32Reg, String, Arm32Condition),
    /// Logic ``` r1 = r2 | r3 ```
    Orr(Arm32Reg, Arm32Reg, Arm32Reg, Arm32Condition),
    /// Logic ``` r1 = r2 | 123 ```
    OrrImd(Arm32Reg, Arm32Reg, String, Arm32Condition),
    /// Logic ``` r1 = r2 ^ r3 ```
    Eor(Arm32Reg, Arm32Reg, Arm32Reg, Arm32Condition),
    /// Logic ``` r1 = r2 ^ 123 ```
    EorImd(Arm32Reg, Arm32Reg, String, Arm32Condition),

    /// Move ``` r1 = r2 ```
    Mov(Arm32Reg, Arm32Reg, Arm32Condition),
    /// Move ``` r1 = 123 ```
    MovImd(Arm32Reg, String, Arm32Condition),
    /// Move Not ``` r1 = ~r2 ```
    Mvn(Arm32Reg, Arm32Reg, Arm32Condition),
    /// Move Not ``` r1 = ~123 ```
    MvnImd(Arm32Reg, Arm32Reg, Arm32Condition),

    /// Jump to label ``` pc = label ```
    BLabel(String, Arm32Condition),
    /// Jump at location stored in register ``` b = r0```
    BReg(Arm32Reg, Arm32Condition),
    /// Jump at label and store pc in lr ``` bl label ```. Same as
    /// ```asm
    /// mov lr, pc;
    /// b myLabel;
    /// ```
    BL(String, Arm32Condition),
    /// ``` pc = reg ```
    BX(Arm32Reg, Arm32Condition),

    ///  ``` CPRS = compare(reg1, reg2) ```
    Cmp(Arm32Reg, Arm32Reg, Arm32Condition),
    ///  ``` CPRS = compare(reg1, 123) ```
    CmpImd(Arm32Reg, String, Arm32Condition),

    /// Load value of word from memory location ``` r0 = M[r1] ```
    LdrReg(Arm32Reg, Arm32Reg, Arm32Condition),
    /// Load ``` r0 = M[r1 + offset] ```
    LdrRegOffset(Arm32Reg, Arm32Reg, Arm32Offset, Arm32Condition),
    /// Load ``` r0 = labelAddress ```
    LdrLbl(Arm32Reg, String, Arm32Condition),

    /// Store ``` M[r1] = r0 ```
    StrReg(Arm32Reg, Arm32Reg, Arm32Condition),
    /// Store ``` M[r1 + offset] = r0 ```
    StrOffset(Arm32Reg, Arm32Reg, Arm32Offset, Arm32Condition),

    /// Stack push
    Push(Vec<Arm32Reg>, Arm32Condition),
    /// Stack pop
    Pop(Vec<Arm32Reg>, Arm32Condition),
}

impl fmt::Display for Arm32Ins {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Arm32Ins::Label(label) => {
                write!(f, "{}:", label)
            }
            Arm32Ins::Add(dest, reg1, reg2, cond) => {
                write!(f, "\tadd{} {}, {}, {}", cond, dest, reg1, reg2)
            }
            Arm32Ins::AddImd(dest, reg, imd, cond) => {
                write!(f, "\tadd{} {}, {}, {}", cond, dest, reg, imd)
            }
            Arm32Ins::Sub(dest, reg1, reg2, cond) => {
                write!(f, "\tsub{} {}, {}, {}", cond, dest, reg1, reg2)
            }
            Arm32Ins::SubImd(dest, reg, imd, cond) => {
                write!(f, "\tsub{} {}, {}, {}", cond, dest, reg, imd)
            }
            Arm32Ins::Mul(dest, reg1, reg2, cond) => {
                write!(f, "\tmul{} {}, {}, {}", cond, dest, reg1, reg2)
            }
            Arm32Ins::MulImd(dest, reg, imd, cond) => {
                write!(f, "\tmul{} {}, {}, {}", cond, dest, reg, imd)
            }
            Arm32Ins::SDiv(dest, reg1, reg2, cond) => {
                write!(f, "\tsdiv{} {}, {}, {}", cond, dest, reg1, reg2)
            }
            Arm32Ins::SDivImd(dest, reg, imd, cond) => {
                write!(f, "\tsdiv{} {}, {}, {}", cond, dest, reg, imd)
            }
            Arm32Ins::UDiv(dest, reg1, reg2, cond) => {
                write!(f, "\tudiv{} {}, {}, {}", cond, dest, reg1, reg2)
            }
            Arm32Ins::UDivImd(dest, reg, imd, cond) => {
                write!(f, "\tudiv{} {}, {}, {}", cond, dest, reg, imd)
            }

            Arm32Ins::Bic(dest, reg1, reg2, cond) => {
                write!(f, "\tbic{} {}, {}, {}", cond, dest, reg1, reg2)
            }
            Arm32Ins::BicImd(dest, reg, imd, cond) => {
                write!(f, "\tbic{} {}, {}, {}", cond, dest, reg, imd)
            }
            Arm32Ins::And(dest, reg1, reg2, cond) => {
                write!(f, "\tand{} {}, {}, {}", cond, dest, reg1, reg2)
            }
            Arm32Ins::AndImd(dest, reg, imd, cond) => {
                write!(f, "\tand{} {}, {}, {}", cond, dest, reg, imd)
            }
            Arm32Ins::Orr(dest, reg1, reg2, cond) => {
                write!(f, "\torr{} {}, {}, {}", cond, dest, reg1, reg2)
            }
            Arm32Ins::OrrImd(dest, reg, imd, cond) => {
                write!(f, "\torr{} {}, {}, {}", cond, dest, reg, imd)
            }
            Arm32Ins::Eor(dest, reg1, reg2, cond) => {
                write!(f, "\teor{} {}, {}, {}", cond, dest, reg1, reg2)
            }
            Arm32Ins::EorImd(dest, reg, imd, cond) => {
                write!(f, "\teor{} {}, {}, {}", cond, dest, reg, imd)
            }

            Arm32Ins::Mov(dest, reg, cond) => write!(f, "\tmov{} {}, {}", cond, dest, reg),
            Arm32Ins::MovImd(dest, imd, cond) => write!(f, "\tmov{} {}, {}", cond, dest, imd),
            Arm32Ins::Mvn(dest, reg, cond) => write!(f, "\tmvn{} {}, {}", cond, dest, reg),
            Arm32Ins::MvnImd(dest, imd, cond) => write!(f, "\tmvn{} {}, {}", cond, dest, imd),

            Arm32Ins::BLabel(label, cond) => write!(f, "\tb{} {}", cond, label),
            Arm32Ins::BReg(reg, cond) => write!(f, "\tb{} {}", cond, reg),
            Arm32Ins::BL(label, cond) => write!(f, "\tbl{} {}", cond, label),
            Arm32Ins::BX(reg, cond) => write!(f, "\tbx{} {}", cond, reg),

            Arm32Ins::Cmp(reg1, reg2, cond) => write!(f, "\tcmp{} {}, {}", cond, reg1, reg2),
            Arm32Ins::CmpImd(reg, imd, cond) => write!(f, "\tcmp{} {}, {}", cond, reg, imd),

            Arm32Ins::LdrReg(dest, reg, cond) => write!(f, "\tldr{} {}, [{}]", cond, dest, reg),
            Arm32Ins::LdrRegOffset(dest, reg, offset, cond) => {
                write!(f, "\tldr{} {}, [{}, {}]", cond, dest, reg, offset)
            }
            Arm32Ins::LdrLbl(dest, lbl, cond) => write!(f, "\tldr{} {}, ={}", cond, dest, lbl),

            Arm32Ins::StrReg(reg, dest, cond) => write!(f, "\tstr{} {}, [{}]", cond, reg, dest),
            Arm32Ins::StrOffset(reg, dest, offest, cond) => {
                write!(f, "\tstr{} {}, [{}, {}]", cond, reg, dest, offest)
            }

            Arm32Ins::Push(regs, cond) => write!(f, "\tpush{} {{{}}}", cond, join_regs(&regs)),
            Arm32Ins::Pop(regs, cond) => write!(f, "\tpop{} {{{}}}", cond, join_regs(&regs)),
        }
    }
}

macro_rules! ins_with_3_terms {
    ($ins:ident, $term1:ident, $term2:ident, &term3:ident) => {
        crate::arm32:;assembly::Arm32Ins::$ins(
            crate::arm32::assembly::Arm32Reg::$term1,
            crate::arm32::assembly::Arm32Reg::$term2,
            crate::arm32::assembly::Arm32Reg::$term3,
            crate::arm32::assembly::Arm32Condition::None,
        )
    };
    ($ins:ident,$term1:ident, $term2:ident, $term3:ident, $condition:ident) => {
        crate::arm32::assembly::Arm32Ins::$ins(
            crate::arm32::assembly::Arm32Reg::$term1,
            crate::arm32::assembly::Arm32Reg::$term2,
            crate::arm32::assembly::Arm32Reg::$term3,
            crate::arm32::assembly::Arm32Condition::$condition,
        )
    };
    ($ins:ident,$term1:ident, $term2:ident, $term3:literal) => {
        crate::arm32::assembly::Arm32Ins::$ins(
            crate::arm32::assembly::Arm32Reg::$term1,
            crate::arm32::assembly::Arm32Reg::$term2,
            String::from($term3),
            crate::arm32::assembly::Arm32Condition::None,
        )
    };
    ($ins:ident,$term1:ident, $term2:ident, $term3:literal, $condition:ident) => {
        crate::arm32::assembly::Arm32Ins::$ins(
            crate::arm32::assembly::Arm32Reg::$term1,
            crate::arm32::assembly::Arm32Reg::$left_term_r,
            String::from($term3),
            crate::arm32::assembly::Arm32Condition::$condition,
        )
    };
}

macro_rules! ins_with_2_terms {
    ($ins:ident, $term1:ident, $term2:ident) => {
        crate::arm32::assembly::Arm32Ins::$ins(
            crate::arm32::assembly::Arm32Reg::$term1,
            crate::arm32::assembly::Arm32Reg::$term2,
            crate::arm32::assembly::Arm32Condition::None,
        )
    };
    ($ins:ident, $term1:ident, $term2:ident,$condition:ident) => {
        crate::arm32::assembly::Arm32Ins::$ins(
            crate::arm32::assembly::Arm32Reg::$term1,
            crate::arm32::assembly::Arm32Reg::$term2,
            crate::arm32::assembly::Arm32Condition::$condition,
        )
    };
    ($ins:ident, $term1:ident, $term2:literal) => {
        crate::arm32::assembly::Arm32Ins::$ins(
            crate::arm32::assembly::Arm32Reg::$term1,
            String::from($term2),
            crate::arm32::assembly::Arm32Condition::None,
        )
    };
    ($ins:ident,$term1:ident,  $term2:literal, $condition:ident) => {
        crate::arm32::assembly::Arm32Ins::$ins(
            crate::arm32::assembly::Arm32Reg::$term1,
            String::from($term2),
            crate::arm32::assembly::Arm32Condition::$condition,
        )
    };
}

macro_rules! ins_with_1_term {
    ($ins:ident, $jump_to:ident) => {
        crate::arm32::assembly::Arm32Ins::$ins(
            crate::arm32::assembly::Arm32Reg::$jump_to,
            crate::arm32::assembly::Arm32Condition::None,
        )
    };
    ($ins:ident, $condition:literal) => {
        crate::arm32::assembly::Arm32Ins::$ins(
            String::from($jump_to),
            crate::arm32::assembly::Arm32Condition::$condition,
        )
    };
}

macro_rules! ins_with_vec_of_terms {
    ($ins:ident, $($reg:ident),+) => {
        crate::arm32::assembly::Arm32Ins::$ins(
            vec![$(crate::arm32::assembly::Arm32Reg::$reg),+],
            crate::arm32::assembly::Arm32Condition::None
        )
    };
    ($ins:ident, $($reg:ident),+; $condition:ident) => {
        crate::arm32::assembly::Arm32Ins::$ins(
            vec![$(crate::arm32::assembly::Arm32Reg::$reg),+],
            crate::arm32::assembly::Arm32Condition::$condition
        )
    };
}

#[macro_export]
macro_rules! label {
    ($label:ident) => {
        crate::arm32::assembly::Arm32Ins::Label(String::from($label))
    };
}

#[macro_export]
macro_rules! add {
    ($into_r:ident, $left_term_r:ident, $right_term_r:ident) => {
        ins_with_3_terms!(Add, $into_r, $left_term_r, $right_term_r)
    };
    ($into_r:ident, $left_term_r:ident, $right_term_r:ident, $condition:ident) => {
        ins_with_3_terms!(Add, $into_r, $left_term_r, $right_term_r, $condition)
    };
    ($into_r:ident, $left_term_r:ident, $right_term:literal) => {
        ins_with_3_terms!(AddImd, $into_r, $left_term_r, $right_term)
    };
    ($into_r:ident, $left_term_r:ident, $right_term:literal, $condition:ident) => {
        ins_with_3_terms!(AddImd, $into_r, $left_term_r, $right_term, $condition)
    };
}

#[macro_export]
macro_rules! sub {
    ($into_r:ident, $left_term_r:ident, $right_term_r:ident) => {
        ins_with_3_terms!(Sub, $into_r, $left_term_r, $right_term_r)
    };
    ($into_r:ident, $left_term_r:ident, $right_term_r:ident, $condition:ident) => {
        ins_with_3_terms!(Sub, $into_r, $left_term_r, $right_term_r, $condition)
    };
    ($into_r:ident, $left_term_r:ident, $right_term:literal) => {
        ins_with_3_terms!(SubImd, $into_r, $left_term_r, $right_term)
    };
    ($into_r:ident, $left_term_r:ident, $right_term:literal, $condition:ident) => {
        ins_with_3_terms!(SubImd, $into_r, $left_term_r, $right_term, $condition)
    };
}

#[macro_export]
macro_rules! mul {
    ($into_r:ident, $left_term_r:ident, $right_term_r:ident) => {
        ins_with_3_terms!(Mul, $into_r, $left_term_r, $right_term_r)
    };
    ($into_r:ident, $left_term_r:ident, $right_term_r:ident, $condition:ident) => {
        ins_with_3_terms!(Mul, $into_r, $left_term_r, $right_term_r, $condition)
    };
    ($into_r:ident, $left_term_r:ident, $right_term:literal) => {
        ins_with_3_terms!(MulImd, $into_r, $left_term_r, $right_term)
    };
    ($into_r:ident, $left_term_r:ident, $right_term:literal, $condition:ident) => {
        ins_with_3_terms!(MulImd, $into_r, $left_term_r, $right_term, $condition)
    };
}

#[macro_export]
macro_rules! sdiv {
    ($into_r:ident, $left_term_r:ident, $right_term_r:ident) => {
        ins_with_3_terms!(SDiv, $into_r, $left_term_r, $right_term_r)
    };
    ($into_r:ident, $left_term_r:ident, $right_term_r:ident, $condition:ident) => {
        ins_with_3_terms!(SDiv, $into_r, $left_term_r, $right_term_r, $condition)
    };
    ($into_r:ident, $left_term_r:ident, $right_term:literal) => {
        ins_with_3_terms!(SDivImd, $into_r, $left_term_r, $right_term)
    };
    ($into_r:ident, $left_term_r:ident, $right_term:literal, $condition:ident) => {
        ins_with_3_terms!(SDivImd, $into_r, $left_term_r, $right_term, $condition)
    };
}

#[macro_export]
macro_rules! udiv {
    ($into_r:ident, $left_term_r:ident, $right_term_r:ident) => {
        ins_with_3_terms!(UDiv, $into_r, $left_term_r, $right_term_r)
    };
    ($into_r:ident, $left_term_r:ident, $right_term_r:ident, $condition:ident) => {
        ins_with_3_terms!(UDiv, $into_r, $left_term_r, $right_term_r, $condition)
    };
    ($into_r:ident, $left_term_r:ident, $right_term:literal) => {
        ins_with_3_terms!(UDivImd, $into_r, $left_term_r, $right_term)
    };
    ($into_r:ident, $left_term_r:ident, $right_term:literal, $condition:ident) => {
        ins_with_3_terms!(UDivImd, $into_r, $left_term_r, $right_term, $condition)
    };
}

#[macro_export]
macro_rules! bic {
    ($into_r:ident, $left_term_r:ident, $right_term_r:ident) => {
        ins_with_3_terms!(Bic, $into_r, $left_term_r, $right_term_r)
    };
    ($into_r:ident, $left_term_r:ident, $right_term_r:ident, $condition:ident) => {
        ins_with_3_terms!(Bic, $into_r, $left_term_r, $right_term_r, $condition)
    };
    ($into_r:ident, $left_term_r:ident, $right_term:literal) => {
        ins_with_3_terms!(BicImd, $into_r, $left_term_r, $right_term)
    };
    ($into_r:ident, $left_term_r:ident, $right_term:literal, $condition:ident) => {
        ins_with_3_terms!(BicImd, $into_r, $left_term_r, $right_term, $condition)
    };
}

#[macro_export]
macro_rules! and {
    ($into_r:ident, $left_term_r:ident, $right_term_r:ident) => {
        ins_with_3_terms!(And, $into_r, $left_term_r, $right_term_r)
    };
    ($into_r:ident, $left_term_r:ident, $right_term_r:ident, $condition:ident) => {
        ins_with_3_terms!(And, $into_r, $left_term_r, $right_term_r, $condition)
    };
    ($into_r:ident, $left_term_r:ident, $right_term:literal) => {
        ins_with_3_terms!(AndImd, $into_r, $left_term_r, $right_term)
    };
    ($into_r:ident, $left_term_r:ident, $right_term:literal, $condition:ident) => {
        ins_with_3_terms!(AndImd, $into_r, $left_term_r, $right_term, $condition)
    };
}

#[macro_export]
macro_rules! orr {
    ($into_r:ident, $left_term_r:ident, $right_term_r:ident) => {
        ins_with_3_terms!(Orr, $into_r, $left_term_r, $right_term_r)
    };
    ($into_r:ident, $left_term_r:ident, $right_term_r:ident, $condition:ident) => {
        ins_with_3_terms!(Orr, $into_r, $left_term_r, $right_term_r, $condition)
    };
    ($into_r:ident, $left_term_r:ident, $right_term:literal) => {
        ins_with_3_terms!(OrrImd, $into_r, $left_term_r, $right_term)
    };
    ($into_r:ident, $left_term_r:ident, $right_term:literal, $condition:ident) => {
        ins_with_3_terms!(OrrImd, $into_r, $left_term_r, $right_term, $condition)
    };
}

#[macro_export]
macro_rules! eor {
    ($into_r:ident, $left_term_r:ident, $right_term_r:ident) => {
        ins_with_3_terms!(Eor, $into_r, $left_term_r, $right_term_r)
    };
    ($into_r:ident, $left_term_r:ident, $right_term_r:ident, $condition:ident) => {
        ins_with_3_terms!(Eor, $into_r, $left_term_r, $right_term_r, $condition)
    };
    ($into_r:ident, $left_term_r:ident, $right_term:literal) => {
        ins_with_3_terms!(EorImd, $into_r, $left_term_r, $right_term)
    };
    ($into_r:ident, $left_term_r:ident, $right_term:literal, $condition:ident) => {
        ins_with_3_terms!(EorImd, $into_r, $left_term_r, $right_term, $condition)
    };
}

#[macro_export]
macro_rules! mov {
    ($into_r:ident, $term:ident) => {
        ins_with_2_terms!(Mov, $into_r, $term)
    };
    ($into_r:ident, $term:ident, $condition:ident) => {
        ins_with_2_terms!(Mov, $into_r, $term, $condition)
    };
    ($into_r:ident, $term:literal) => {
        ins_with_2_terms!(MovImd, $into_r, $term)
    };
    ($into_r:ident, $term:literal, $condition:ident) => {
        ins_with_2_terms!(MovImd, $into_r, $term, $condition)
    };
}

#[macro_export]
macro_rules! mvn {
    ($into_r:ident, $term:ident) => {
        ins_with_2_terms!(Mvn, $into_r, $term)
    };
    ($into_r:ident, $term:ident, $condition:ident) => {
        ins_with_2_terms!(Mvn, $into_r, $term, $condition)
    };
    ($into_r:ident, $term:literal) => {
        ins_with_2_terms!(MvnImd, $into_r, $term)
    };
    ($into_r:ident, $term:literal, $condition:ident) => {
        ins_with_2_terms!(MvnImd, $into_r, $term, $condition)
    };
}

#[macro_export]
macro_rules! branch {
    ($jump_to:literal) => {
        ins_with_1_term!(BLabel, $jump_to)
    };
    ($jump_to:literal, $condition:ident) => {
        ins_with_1_term!(BLabel, $jump_to, $condition)
    };
    ($jump_to:ident) => {
        ins_with_1_term!(BReg, $jump_to)
    };
    ($jump_to:ident, $condition:ident) => {
        ins_with_1_term!(BReg, $jump_to, $condition)
    };
}

#[macro_export]
macro_rules! branch_link {
    ($jump_to:ident) => {
        ins_with_1_term!(BL, $jump_to)
    };
    ($jump_to:ident, $condition:ident) => {
        ins_with_1_term!(BL, $jump_to, $condition)
    };
}

#[macro_export]
macro_rules! branch_exchange {
    ($jump_to:ident) => {
        ins_with_1_term!(BX, $jump_to)
    };
    ($jump_to:ident, $condition:ident) => {
        ins_with_1_term!(BX, $jump_to, $condition)
    };
}

#[macro_export]
macro_rules! cmp {
    ($left_term:ident, $right_term:ident) => {
        ins_with_2_terms!(Cmp, $left_term, $right_term)
    };
    ($left_term:ident, $right_term:ident, $condition:ident) => {
        ins_with_2_terms!(CmpImd, $left_term, $right_term, $condition)
    };
}

#[macro_export]
macro_rules! ldr {
    ($reg:ident, $label:ident) => {
        crate::arm32::assembly::Arm32Ins::LdrLbl(
            crate::arm32::assembly::Arm32Reg::$reg,
            String::from($label),
            crate::arm32::assembly::Arm32Condition::None,
        )
    };
    ($reg:ident, $label:ident; $condition:ident) => {
        crate::arm32::assembly::Arm32Ins::LdrLbl(
            crate::arm32::assembly::Arm32Reg::$reg,
            String::from($label),
            crate::arm32::assembly::Arm32Condition::$condition,
        )
    }
}

#[macro_export]
macro_rules! push {
    ($($reg:ident),+) => {
        ins_with_vec_of_terms!(Push, $($reg),+)
    };
    ($($reg:ident),+; $condition:ident) => {
        ins_with_vec_of_terms!(Push, $($reg),+; $condition)
    };
}

#[macro_export]
macro_rules! pop {
    ($($reg:ident),+) => {
        ins_with_vec_of_terms!(Pop, $($reg),+)
    };
    ($($reg:ident),+; $condition:ident) => {
        ins_with_vec_of_terms!(Pop, $($reg),+; $condition)
    };
}
