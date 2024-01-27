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
            Arm32Offset::Number(n) => write!(f, "#{}", n),
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
    /// Arithmetic ``` r1 = -r1
    Neg(Arm32Reg, Arm32Reg, Arm32Condition),
    /// Arithmetic ``` r1 = r2 * r3 ```
    Mul(Arm32Reg, Arm32Reg, Arm32Reg, Arm32Condition),
    /// Arithmetic ``` r1 = r2 * 123 ```
    MulImd(Arm32Reg, Arm32Reg, String, Arm32Condition),

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

    /// Load ``` r0 = labelAddress ```
    /// Load ``` r0 = the given number ```
    Ldr(Arm32Reg, String, Arm32Condition),
    /// Load value of word from memory location ``` r0 = M[r1] ```
    LdrReg(Arm32Reg, Arm32Reg, Arm32Condition),
    /// Load ``` r0 = M[r1 + offset] ```
    LdrRegOffset(Arm32Reg, Arm32Reg, Arm32Offset, Arm32Condition),

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
                write!(f, "\tadd{} {}, {}, #{}", cond, dest, reg, imd)
            }
            Arm32Ins::Sub(dest, reg1, reg2, cond) => {
                write!(f, "\tsub{} {}, {}, {}", cond, dest, reg1, reg2)
            }
            Arm32Ins::SubImd(dest, reg, imd, cond) => {
                write!(f, "\tsub{} {}, {}, #{}", cond, dest, reg, imd)
            }
            Arm32Ins::Neg(dest, reg, cond) => {
                write!(f, "\tneg{} {}, {}", cond, dest, reg)
            }
            Arm32Ins::Mul(dest, reg1, reg2, cond) => {
                write!(f, "\tmul{} {}, {}, {}", cond, dest, reg1, reg2)
            }
            Arm32Ins::MulImd(dest, reg, imd, cond) => {
                write!(f, "\tmul{} {}, {}, #{}", cond, dest, reg, imd)
            }

            Arm32Ins::Bic(dest, reg1, reg2, cond) => {
                write!(f, "\tbic{} {}, {}, {}", cond, dest, reg1, reg2)
            }
            Arm32Ins::BicImd(dest, reg, imd, cond) => {
                write!(f, "\tbic{} {}, {}, #{}", cond, dest, reg, imd)
            }
            Arm32Ins::And(dest, reg1, reg2, cond) => {
                write!(f, "\tand{} {}, {}, {}", cond, dest, reg1, reg2)
            }
            Arm32Ins::AndImd(dest, reg, imd, cond) => {
                write!(f, "\tand{} {}, {}, #{}", cond, dest, reg, imd)
            }
            Arm32Ins::Orr(dest, reg1, reg2, cond) => {
                write!(f, "\torr{} {}, {}, {}", cond, dest, reg1, reg2)
            }
            Arm32Ins::OrrImd(dest, reg, imd, cond) => {
                write!(f, "\torr{} {}, {}, #{}", cond, dest, reg, imd)
            }
            Arm32Ins::Eor(dest, reg1, reg2, cond) => {
                write!(f, "\teor{} {}, {}, {}", cond, dest, reg1, reg2)
            }
            Arm32Ins::EorImd(dest, reg, imd, cond) => {
                write!(f, "\teor{} {}, {}, #{}", cond, dest, reg, imd)
            }

            Arm32Ins::Mov(dest, reg, cond) => write!(f, "\tmov{} {}, {}", cond, dest, reg),
            Arm32Ins::MovImd(dest, imd, cond) => write!(f, "\tmov{} {}, #{}", cond, dest, imd),
            Arm32Ins::Mvn(dest, reg, cond) => write!(f, "\tmvn{} {}, {}", cond, dest, reg),
            Arm32Ins::MvnImd(dest, imd, cond) => write!(f, "\tmvn{} {}, #{}", cond, dest, imd),

            Arm32Ins::BLabel(label, cond) => write!(f, "\tb{} {}", cond, label),
            Arm32Ins::BReg(reg, cond) => write!(f, "\tb{} {}", cond, reg),
            Arm32Ins::BL(label, cond) => write!(f, "\tbl{} {}", cond, label),
            Arm32Ins::BX(reg, cond) => write!(f, "\tbx{} {}", cond, reg),

            Arm32Ins::Cmp(reg1, reg2, cond) => write!(f, "\tcmp{} {}, {}", cond, reg1, reg2),
            Arm32Ins::CmpImd(reg, imd, cond) => write!(f, "\tcmp{} {}, #{}", cond, reg, imd),

            Arm32Ins::Ldr(dest, lbl, cond) => write!(f, "\tldr{} {}, ={}", cond, dest, lbl),
            Arm32Ins::LdrReg(dest, reg, cond) => write!(f, "\tldr{} {}, [{}]", cond, dest, reg),
            Arm32Ins::LdrRegOffset(dest, reg, offset, cond) => {
                write!(f, "\tldr{} {}, [{}, {}]", cond, dest, reg, offset)
            }

            Arm32Ins::StrReg(reg, dest, cond) => write!(f, "\tstr{} {}, [{}]", cond, reg, dest),
            Arm32Ins::StrOffset(src_reg, addr_reg, offset, cond) => {
                write!(f, "\tstr{} {}, [{}, {}]", cond, src_reg, addr_reg, offset)
            }

            Arm32Ins::Push(regs, cond) => write!(f, "\tpush{} {{{}}}", cond, join_regs(&regs)),
            Arm32Ins::Pop(regs, cond) => write!(f, "\tpop{} {{{}}}", cond, join_regs(&regs)),
        }
    }
}

#[macro_export]
macro_rules! label {
    ($label:expr) => {
        crate::arm32::assembly::Arm32Ins::Label(String::from($label))
    };
}

#[macro_export]
macro_rules! b {
    ($jump_to:expr) => {
        crate::arm32::assembly::Arm32Ins::BLabel(
            String::from($jump_to),
            crate::arm32::assembly::Arm32Condition::None,
        )
    };
    ($jump_to:expr, $condition:ident) => {
        crate::arm32::assembly::Arm32Ins::BLabel(
            String::from($jump_to),
            crate::arm32::assembly::Arm32Condition::$condition,
        )
    };
}

#[macro_export]
macro_rules! bl {
    ($label:expr) => {
        crate::arm32::assembly::Arm32Ins::BL(
            String::from($label),
            crate::arm32::assembly::Arm32Condition::None,
        )
    };
}

#[macro_export]
macro_rules! bx {
    ($jump_reg:ident) => {
        crate::arm32::assembly::Arm32Ins::BX(
            crate::arm32::assembly::Arm32Reg::$jump_reg,
            crate::arm32::assembly::Arm32Condition::None,
        )
    };
}

#[macro_export]
macro_rules! push {
    ($($reg:ident),+) => {
        crate::arm32::assembly::Arm32Ins::Push(
            vec![$(crate::arm32::assembly::Arm32Reg::$reg),+],
            crate::arm32::assembly::Arm32Condition::None
        )
    };
}

#[macro_export]
macro_rules! pop {
    ($($reg:ident),+) => {
        crate::arm32::assembly::Arm32Ins::Pop(
            vec![$(crate::arm32::assembly::Arm32Reg::$reg),+],
            crate::arm32::assembly::Arm32Condition::None
        )
    };
}

#[macro_export]
macro_rules! ldr {
    ($dest_reg:ident, $label:ident) => {
        crate::arm32::assembly::Arm32Ins::Ldr(
            crate::arm32::assembly::Arm32Reg::$dest_reg,
            String::from($label),
            crate::arm32::assembly::Arm32Condition::None,
        )
    };
}

#[macro_export]
macro_rules! neg {
    ($dest_reg:ident, $reg:ident) => {
        crate::arm32::assembly::Arm32Ins::Neg(
            crate::arm32::assembly::Arm32Reg::$dest_reg,
            crate::arm32::assembly::Arm32Reg::$reg,
            crate::arm32::assembly::Arm32Condition::None,
        )
    };
    ($dest_reg:ident, $reg:ident, $condition:ident) => {
        crate::arm32::assembly::Arm32Ins::Neg(
            crate::arm32::assembly::Arm32Reg::$dest_reg,
            crate::arm32::assembly::Arm32Reg::$reg,
            crate::arm32::assembly::Arm32Condition::$condition,
        )
    };
}

#[macro_export]
macro_rules! cmp {
    ($reg1:ident, $reg2:ident) => {
        crate::arm32::assembly::Arm32Ins::Cmp(
            crate::arm32::assembly::Arm32Reg::$reg1,
            crate::arm32::assembly::Arm32Reg::$reg2,
            crate::arm32::assembly::Arm32Condition::None,
        )
    };
    ($reg:ident, #$imd:tt) => {
        crate::arm32::assembly::Arm32Ins::CmpImd(
            crate::arm32::assembly::Arm32Reg::$reg,
            String::from($imd.to_string()),
            crate::arm32::assembly::Arm32Condition::None,
        )
    };
}

#[macro_export]
macro_rules! mov {
    ($dest_reg:ident, $source_reg:ident) => {
        crate::arm32::assembly::Arm32Ins::Mov(
            crate::arm32::assembly::Arm32Reg::$dest_reg,
            crate::arm32::assembly::Arm32Reg::$source_reg,
            crate::arm32::assembly::Arm32Condition::None,
        )
    };
    ($dest_reg:ident, #$imd:tt) => {
        crate::arm32::assembly::Arm32Ins::MovImd(
            crate::arm32::assembly::Arm32Reg::$dest_reg,
            String::from($imd.to_string()),
            crate::arm32::assembly::Arm32Condition::None,
        )
    };
    ($dest_reg:ident, #$imd:tt, $condition:ident) => {
        crate::arm32::assembly::Arm32Ins::MovImd(
            crate::arm32::assembly::Arm32Reg::$dest_reg,
            String::from($imd.to_string()),
            crate::arm32::assembly::Arm32Condition::$condition,
        )
    };
}

#[macro_export]
macro_rules! add {
    ($dest_reg:ident, $reg1:ident, $reg2:ident) => {
        crate::arm32::assembly::Arm32Ins::Add(
            crate::arm32::assembly::Arm32Reg::$dest_reg,
            crate::arm32::assembly::Arm32Reg::$reg1,
            crate::arm32::assembly::Arm32Reg::$reg2,
            crate::arm32::assembly::Arm32Condition::None,
        )
    };
    ($dest_reg:ident, $reg:ident, #$imd:tt) => {
        crate::arm32::assembly::Arm32Ins::AddImd(
            crate::arm32::assembly::Arm32Reg::$dest_reg,
            crate::arm32::assembly::Arm32Reg::$reg,
            String::from($imd.to_string()),
            crate::arm32::assembly::Arm32Condition::None,
        )
    };
    ($dest_reg:ident, $reg:ident, #$imd:tt) => {
        crate::arm32::assembly::Arm32Ins::AddImd(
            crate::arm32::assembly::Arm32Reg::$dest_reg,
            crate::arm32::assembly::Arm32Reg::$reg,
            String::from($imd.to_string()),
            crate::arm32::assembly::Arm32Condition::None,
        )
    };
    ($dest_reg:ident, $reg:ident, #$imd:tt, $cond:ident) => {
        crate::arm32::assembly::Arm32Ins::AddImd(
            crate::arm32::assembly::Arm32Reg::$dest_reg,
            crate::arm32::assembly::Arm32Reg::$reg,
            String::from($imd.to_string()),
            crate::arm32::assembly::Arm32Condition::$cond,
        )
    };
}

#[macro_export]
macro_rules! sub {
    ($dest_reg:ident, $reg1:ident, $reg2:ident) => {
        crate::arm32::assembly::Arm32Ins::Sub(
            crate::arm32::assembly::Arm32Reg::$dest_reg,
            crate::arm32::assembly::Arm32Reg::$reg1,
            crate::arm32::assembly::Arm32Reg::$reg2,
            crate::arm32::assembly::Arm32Condition::None,
        )
    };
    ($dest_reg:ident, $reg:ident, #$value:tt) => {
        crate::arm32::assembly::Arm32Ins::SubImd(
            crate::arm32::assembly::Arm32Reg::$dest_reg,
            crate::arm32::assembly::Arm32Reg::$reg,
            String::from($value.to_string()),
            crate::arm32::assembly::Arm32Condition::None,
        )
    };
}

#[macro_export]
macro_rules! mul {
    ($dest_reg:ident, $reg1:ident, $reg2:ident) => {
        crate::arm32::assembly::Arm32Ins::Mul(
            crate::arm32::assembly::Arm32Reg::$dest_reg,
            crate::arm32::assembly::Arm32Reg::$reg1,
            crate::arm32::assembly::Arm32Reg::$reg2,
            crate::arm32::assembly::Arm32Condition::None,
        )
    };
}

#[macro_export]
macro_rules! str {
    ($source_reg:ident, [$addr_reg:ident, $offset:expr]) => {
        crate::arm32::assembly::Arm32Ins::StrOffset(
            crate::arm32::assembly::Arm32Reg::$source_reg,
            crate::arm32::assembly::Arm32Reg::$addr_reg,
            crate::arm32::assembly::Arm32Offset::Number(String::from($offset)),
            crate::arm32::assembly::Arm32Condition::None,
        )
    };
}
