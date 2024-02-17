use super::assembly::Arm32Ins;

pub const EXIT_FAIL: &'static str = "exit_fail";
pub const EXIT: &'static str = "exit";

pub fn get_exit_instructions() -> Vec<Arm32Ins> {
    vec![b!(EXIT), label!(EXIT_FAIL), mov!(R0, #1), label!(EXIT)]
}

/// Required, since there isn't `sdiv` or 'udiv` instructions on arm32 :(
pub fn goto_divide_instructions() -> Vec<Arm32Ins> {
    vec![b!("divide")]
}

/// Required, since there isn't `sdiv` or 'udiv` instructions on arm32 :(
pub fn divide_instructions() -> Vec<Arm32Ins> {
    let zero = "0";
    let one = "1";
    let minus12 = "-12";
    let minus16 = "-16";
    vec![
        label!("divide"),
        push!(FP, LR),
        mov!(FP, SP),
        push!(R0, R1),
        ldr!(R0, zero),
        sub!(SP, SP, #8),
        str!(R0, [SP, 4]),
        ldr!(R0, zero),
        str!(R0, [SP, 0]),
        label!(".L_divide__whileStart"),
        ldr!(R0, [FP, #-12]),
        push!(R0, IP),
        ldr!(R0, [FP, #-4]),
        pop!(R1, IP),
        cmp!(R1, R0),
        mov!(R0, #1, Lt),
        mov!(R0, #0, Ge),
        cmp!(R0, #0),
        b!(".L_divide__whileEnd", Eq),
        ldr!(R0, [FP, #-12]),
        push!(R0, IP),
        ldr !(R0, [FP, #-8]),
        pop!(R1, IP),
        add!(R0, R0, R1),
        str!(R0, [FP, minus12]),
        ldr!(R0, [FP, #-16]),
        push!(R0, IP),
        ldr!(R0, one),
        pop!(R1, IP),
        add!(R0, R0, R1),
        str!(R0, [FP, minus16]),
        add!(SP, SP, #0),
        b!(".L_divide__whileStart"),
        label!(".L_divide__whileEnd"),
        ldr!(R0, [FP, #-12]),
        push!(R0, IP),
        ldr!(R0, [FP, #-4]),
        pop!(R1, IP),
        cmp!(R1, R0),
        mov!(R0, #1, Gt),
        mov!(R0, #0, Le),
        cmp!(R0, #0),
        b!(".L_divide__ifEnd", Eq),
        ldr!(R0, [FP, #-16]),
        push!(R0, IP),
        ldr!(R0, one),
        pop!(R1, IP),
        sub!(R0, R1, R0),
        mov!(SP, FP),
        pop!(FP, PC),
        add!(SP, SP, #0),
        label!(".L_divide__ifEnd"),
        ldr!(R0, [FP, #-16]),
        mov!(SP, FP),
        pop!(FP, PC),
        add!(SP, SP, #8),
        mov!(SP, FP),
        pop!(FP, PC),
    ]
}
