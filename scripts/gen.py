l = [
    ("MulReg", "mul"),
    ("SDigReg", "sdiv"),
    ("UDivReg", "udiv"),
    ("BicReg", "bic"),
    ("AndReg", "and"),
    ("OrrReg", "orr"),
    ("EorReg", "eor"),
]

for (e, i) in l:
    print("Arm32Ins::"+e+"Reg(into, reg1, reg2) => write!(f, \"" + i +" {}, {}, {}\", into, reg1, reg2),")
    print("Arm32Ins::"+e+"Imd(into, reg, imd) => write!(f, \"" + i +" {}, {}, {}\", into, reg, imd),")
