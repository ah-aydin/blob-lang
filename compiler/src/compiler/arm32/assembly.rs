use std::fmt;

#[derive(Debug)]
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
    PR,
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
                Arm32Reg::PR => "pr",
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

#[derive(Debug)]
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

#[derive(Debug)]
pub enum Arm32Ins {
    /// Arithmetic ``` r1 = r2 + r3 ```
    AddRegReg(Arm32Reg, Arm32Reg, Arm32Reg, Arm32Condition),
    /// Arithmetic ``` r1 = r2 + 123 ```
    AddRegImd(Arm32Reg, Arm32Reg, String, Arm32Condition),
    /// Arithmetic ``` r1 = r2 - r3 ```
    SubRegReg(Arm32Reg, Arm32Reg, Arm32Reg, Arm32Condition),
    /// Arithmetic ``` r1 = r2 - 123 ```
    SubRegImd(Arm32Reg, Arm32Reg, String, Arm32Condition),
    /// Arithmetic ``` r1 = r2 * r3 ```
    MulRegReg(Arm32Reg, Arm32Reg, Arm32Reg, Arm32Condition),
    /// Arithmetic ``` r1 = r2 * 123 ```
    MulRegImd(Arm32Reg, Arm32Reg, String, Arm32Condition),
    /// Arithmetic ``` r1 = r2 / r3 // Signed ```
    SDivRegReg(Arm32Reg, Arm32Reg, Arm32Reg, Arm32Condition),
    /// Arithmetic ``` r1 = r2 / 123 // Signed ```
    SDivRegImd(Arm32Reg, Arm32Reg, Arm32Reg, Arm32Condition),
    /// Arithmetic ``` r1 = r2 / r3 // Unsigned ```
    UDivRegReg(Arm32Reg, Arm32Reg, Arm32Reg, Arm32Condition),
    /// Arithmetic ``` r1 = r2 / 123 // Unsigned ```
    UDivRegImd(Arm32Reg, Arm32Reg, String, Arm32Condition),

    /// Logic ``` r1 = r2 & ~r3 ```
    BicRegReg(Arm32Reg, Arm32Reg, Arm32Reg, Arm32Condition),
    /// Logic ``` r1 = r2 & ~123 ```
    BicRegImd(Arm32Reg, Arm32Reg, String, Arm32Condition),
    /// Logic ``` r1 = r2 & r3 ```
    AndRegReg(Arm32Reg, Arm32Reg, Arm32Reg, Arm32Condition),
    /// Logic ``` r1 = r2 & 123 ```
    AndRegImd(Arm32Reg, Arm32Reg, String, Arm32Condition),
    /// Logic ``` r1 = r2 | r3 ```
    OrrRegReg(Arm32Reg, Arm32Reg, Arm32Reg, Arm32Condition),
    /// Logic ``` r1 = r2 | 123 ```
    OrrRegImd(Arm32Reg, Arm32Reg, String, Arm32Condition),
    /// Logic ``` r1 = r2 ^ r3 ```
    EorRegReg(Arm32Reg, Arm32Reg, Arm32Reg, Arm32Condition),
    /// Logic ``` r1 = r2 ^ 123 ```
    EorRegImd(Arm32Reg, Arm32Reg, String, Arm32Condition),

    /// Move ``` r1 = r2 ```
    MovReg(Arm32Reg, Arm32Reg, Arm32Condition),
    /// Move ``` r1 = 123 ```
    MovImd(Arm32Reg, String, Arm32Condition),
    /// Move Not ``` r1 = ~r2 ```
    MvnReg(Arm32Reg, Arm32Reg, Arm32Condition),
    /// Move Not ``` r1 = ~123 ```
    MvnImd(Arm32Reg, Arm32Reg, Arm32Condition),

    /// Jump to label ``` b label ```
    BLabel(String, Arm32Condition),
    /// Jump at location stored in register ``` b reg```
    BReg(Arm32Reg, Arm32Condition),
    /// Jump at label and store pc in lr ``` bl label ```
    BL(String, Arm32Condition),
    /// ``` pc = reg ```
    BX(Arm32Reg, Arm32Condition),

    ///  ``` CPRS = compare(reg1, reg2) ```
    CmpReg(Arm32Reg, Arm32Reg, Arm32Condition),
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
            Arm32Ins::AddRegReg(dest, reg1, reg2, cond) => {
                write!(f, "\tadd{} {}, {}, {}", cond, dest, reg1, reg2)
            }
            Arm32Ins::AddRegImd(dest, reg, imd, cond) => {
                write!(f, "\tadd{} {}, {}, {}", cond, dest, reg, imd)
            }
            Arm32Ins::SubRegReg(dest, reg1, reg2, cond) => {
                write!(f, "\tsub{} {}, {}, {}", cond, dest, reg1, reg2)
            }
            Arm32Ins::SubRegImd(dest, reg, imd, cond) => {
                write!(f, "\tsub{} {}, {}, {}", cond, dest, reg, imd)
            }
            Arm32Ins::MulRegReg(dest, reg1, reg2, cond) => {
                write!(f, "\tmul{} {}, {}, {}", cond, dest, reg1, reg2)
            }
            Arm32Ins::MulRegImd(dest, reg, imd, cond) => {
                write!(f, "\tmul{} {}, {}, {}", cond, dest, reg, imd)
            }
            Arm32Ins::SDivRegReg(dest, reg1, reg2, cond) => {
                write!(f, "\tsdiv{} {}, {}, {}", cond, dest, reg1, reg2)
            }
            Arm32Ins::SDivRegImd(dest, reg, imd, cond) => {
                write!(f, "\tsdiv{} {}, {}, {}", cond, dest, reg, imd)
            }
            Arm32Ins::UDivRegReg(dest, reg1, reg2, cond) => {
                write!(f, "\tudiv{} {}, {}, {}", cond, dest, reg1, reg2)
            }
            Arm32Ins::UDivRegImd(dest, reg, imd, cond) => {
                write!(f, "\tudiv{} {}, {}, {}", cond, dest, reg, imd)
            }

            Arm32Ins::BicRegReg(dest, reg1, reg2, cond) => {
                write!(f, "\tbic{} {}, {}, {}", cond, dest, reg1, reg2)
            }
            Arm32Ins::BicRegImd(dest, reg, imd, cond) => {
                write!(f, "\tbic{} {}, {}, {}", cond, dest, reg, imd)
            }
            Arm32Ins::AndRegReg(dest, reg1, reg2, cond) => {
                write!(f, "\tand{} {}, {}, {}", cond, dest, reg1, reg2)
            }
            Arm32Ins::AndRegImd(dest, reg, imd, cond) => {
                write!(f, "\tand{} {}, {}, {}", cond, dest, reg, imd)
            }
            Arm32Ins::OrrRegReg(dest, reg1, reg2, cond) => {
                write!(f, "\torr{} {}, {}, {}", cond, dest, reg1, reg2)
            }
            Arm32Ins::OrrRegImd(dest, reg, imd, cond) => {
                write!(f, "\torr{} {}, {}, {}", cond, dest, reg, imd)
            }
            Arm32Ins::EorRegReg(dest, reg1, reg2, cond) => {
                write!(f, "\teor{} {}, {}, {}", cond, dest, reg1, reg2)
            }
            Arm32Ins::EorRegImd(dest, reg, imd, cond) => {
                write!(f, "\teor{} {}, {}, {}", cond, dest, reg, imd)
            }

            Arm32Ins::MovReg(dest, reg, cond) => write!(f, "\tmov{} {}, {}", cond, dest, reg),
            Arm32Ins::MovImd(dest, imd, cond) => write!(f, "\tmov{} {}, {}", cond, dest, imd),
            Arm32Ins::MvnReg(dest, reg, cond) => write!(f, "\tmvn{} {}, {}", cond, dest, reg),
            Arm32Ins::MvnImd(dest, imd, cond) => write!(f, "\tmvn{} {}, {}", cond, dest, imd),

            Arm32Ins::BLabel(label, cond) => write!(f, "\tb{} {}", cond, label),
            Arm32Ins::BReg(reg, cond) => write!(f, "\tb{} {}", cond, reg),
            Arm32Ins::BL(label, cond) => write!(f, "\tbl{} {}", cond, label),
            Arm32Ins::BX(reg, cond) => write!(f, "\tbx{} {}", cond, reg),

            Arm32Ins::CmpReg(reg1, reg2, cond) => write!(f, "\tcmp{} {}, {}", cond, reg1, reg2),
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
