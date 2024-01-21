use super::assembly::Arm32Ins;

pub const DIV_START: &'static str = "div_start";
pub const EXIT_FAIL: &'static str = "exit_fail";
pub const EXIT: &'static str = "exit";

pub fn get_exit_instructions() -> Vec<Arm32Ins> {
    vec![b!(EXIT), label!(EXIT_FAIL), mov!(R0, "#1"), label!(EXIT)]
}

/// Required, since there isn't `sdiv` or 'udiv` instructions on arm32 :(
pub fn goto_div_instructions() -> Vec<Arm32Ins> {
    vec![mov!(R4, PC), b!(DIV_START)]
}

/// Required, since there isn't `sdiv` or 'udiv` instructions on arm32 :(
pub fn div_instructions() -> Vec<Arm32Ins> {
    vec![
        // R1 / R0
        // R0 will hold the result
        label!(DIV_START),
        mov!(R5, "#0"),
        // Negate R1 if negative number
        cmp!(R1, "#0"),
        neg!(R1, R1, Lt),
        add!(R5, R5, "#1", Lt),
        // Negate R0 if negative number and goto exit fail if it is 0
        cmp!(R0, "#0"),
        neg!(R0, R0, Lt),
        add!(R5, R5, "#1", Lt),
        b!(EXIT_FAIL, Eq), // This causes an illegal hardware instruction, but it will go on an
                           // infinte loop otherwise.
        //  Start division
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
        // Negate result if one of the terms is negative
        cmp!(R5, "#1"),
        neg!(R0, R0, Eq),
        // R4 will have the return address
        bx!(R4),
    ]
}
