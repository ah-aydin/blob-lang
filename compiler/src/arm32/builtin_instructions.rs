use super::assembly::Arm32Ins;

pub const DIV_START: &'static str = "div_start";

/// Required, since there isn't `sdiv` or 'udiv` instructions on arm32 :(
pub fn goto_div_instructions() -> Vec<Arm32Ins> {
    vec![
        mov!(R4, PC),
        b!(DIV_START),
    ]
}

/// Required, since there isn't `sdiv` or 'udiv` instructions on arm32 :(
pub fn div_instructions() -> Vec<Arm32Ins> {
    vec![
        // R1 / R0
        // R0 will hold the result
        label!(DIV_START),
        mov!(R2, R0),
        mov!(R3, "#0"), // R3 holds the sum
        mov!(R0, "#0"),
        label!("div_loop"),
        add!(R3, R3, R2),
        cmp!(R3, R1),
        b!("div_end", Ge),
        add!(R0, R0, "#1"),
        b!("div_loop"),
        label!("div_end"),
        cmp!(R3, R1),
        add!(R0, R0, "#1", Eq),
        // R4 will have the return address
        bx!(R4)
    ]
}
