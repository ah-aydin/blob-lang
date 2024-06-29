use blobbc::instructions::OpCode;

pub struct VM {
    registers: [i32; 32],
    program: Vec<u8>,
    pc: usize,
    remainder: u32,
    cmp_flag: bool,
}

impl VM {
    pub fn new() -> Self {
        VM {
            registers: [0; 32],
            program: vec![],
            pc: 0,
            remainder: 0,
            cmp_flag: false,
        }
    }

    pub fn run(&mut self) {
        while self.execute_instruction() {}
    }

    pub fn execute_instruction(&mut self) -> bool {
        if self.pc >= self.program.len() {
            return false;
        }
        match self.decode_opcode() {
            OpCode::HLT => false,

            OpCode::LOADIMD => {
                let register = self.next_8_bits() as usize;
                let number = self.next_16_bits() as u16;
                self.registers[register] = number as i32;
                true
            }
            OpCode::LOADREG => {
                let dest_reg = self.next_8_bits() as usize;
                let src_reg = self.next_8_bits() as usize;
                self.registers[dest_reg] = self.registers[src_reg];
                self.next_8_bits();
                true
            }

            OpCode::ADD => {
                let dest_reg = self.next_8_bits() as usize;
                let left_operand = self.registers[self.next_8_bits() as usize];
                let right_operand = self.registers[self.next_8_bits() as usize];
                self.registers[dest_reg] = left_operand + right_operand;
                true
            }
            OpCode::SUB => {
                let dest_reg = self.next_8_bits() as usize;
                let left_operand = self.registers[self.next_8_bits() as usize];
                let right_operand = self.registers[self.next_8_bits() as usize];
                self.registers[dest_reg] = left_operand - right_operand;
                true
            }
            OpCode::MUL => {
                let dest_reg = self.next_8_bits() as usize;
                let left_operand = self.registers[self.next_8_bits() as usize];
                let right_operand = self.registers[self.next_8_bits() as usize];
                self.registers[dest_reg] = left_operand * right_operand;
                true
            }
            OpCode::DIV => {
                let dest_reg = self.next_8_bits() as usize;
                let left_operand = self.registers[self.next_8_bits() as usize];
                let right_operand = self.registers[self.next_8_bits() as usize];
                self.registers[dest_reg] = left_operand / right_operand;
                self.remainder = (left_operand % right_operand) as u32;
                true
            }

            OpCode::JMP => {
                let target = self.registers[self.next_8_bits() as usize];
                self.pc = target as usize;
                true
            }
            OpCode::JMPF => {
                self.pc += self.registers[self.next_8_bits() as usize] as usize;
                true
            }
            OpCode::JMPB => {
                let jmp = self.registers[self.next_8_bits() as usize] as isize;
                self.pc = (self.pc as isize - jmp) as usize;
                true
            }

            OpCode::EQ => {
                let left_operant = self.registers[self.next_8_bits() as usize];
                let right_operant = self.registers[self.next_8_bits() as usize];
                self.cmp_flag = left_operant == right_operant;
                self.next_8_bits();
                true
            }
            OpCode::NEQ => {
                let left_operant = self.registers[self.next_8_bits() as usize];
                let right_operant = self.registers[self.next_8_bits() as usize];
                self.cmp_flag = left_operant != right_operant;
                self.next_8_bits();
                true
            }
            OpCode::GT => {
                let left_operant = self.registers[self.next_8_bits() as usize];
                let right_operant = self.registers[self.next_8_bits() as usize];
                self.cmp_flag = left_operant > right_operant;
                self.next_8_bits();
                true
            }
            OpCode::LT => {
                let left_operant = self.registers[self.next_8_bits() as usize];
                let right_operant = self.registers[self.next_8_bits() as usize];
                self.cmp_flag = left_operant < right_operant;
                self.next_8_bits();
                true
            }
            OpCode::GE => {
                let left_operant = self.registers[self.next_8_bits() as usize];
                let right_operant = self.registers[self.next_8_bits() as usize];
                self.cmp_flag = left_operant >= right_operant;
                self.next_8_bits();
                true
            }
            OpCode::LE => {
                let left_operant = self.registers[self.next_8_bits() as usize];
                let right_operant = self.registers[self.next_8_bits() as usize];
                self.cmp_flag = left_operant <= right_operant;
                self.next_8_bits();
                true
            }

            OpCode::IGL => {
                unreachable!("Got IGL OpCode in pc={}", self.pc);
            }
        }
    }

    fn decode_opcode(&mut self) -> OpCode {
        let op_code = OpCode::from(self.program[self.pc]);
        self.pc += 1;
        return op_code;
    }

    fn next_8_bits(&mut self) -> u8 {
        let result = self.program[self.pc];
        self.pc += 1;
        return result;
    }

    fn next_16_bits(&mut self) -> u16 {
        let result = ((self.program[self.pc] as u16) << 8) | self.program[self.pc + 1] as u16;
        self.pc += 2;
        result
    }

    fn next_24_bits(&mut self) -> u32 {
        let result = ((self.program[self.pc] as u32) << 16)
            | ((self.program[self.pc + 1] as u32) << 8)
            | self.program[self.pc + 2] as u32;
        self.pc += 3;
        result
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_loadimd_op_code() {
        let mut vm = VM::new();
        vm.program = vec![OpCode::LOADIMD as u8, 0, 1, 244, OpCode::HLT as u8];
        vm.run();

        assert_eq!(vm.registers[0], 500);
    }

    #[test]
    fn test_loadreg_op_code() {
        let mut vm = VM::new();
        let dest_reg = 0;
        let src_reg: u8 = 1;
        let value = 80;
        vm.registers[src_reg as usize] = value;
        vm.program = vec![
            OpCode::LOADREG as u8,
            dest_reg,
            src_reg,
            0,
            OpCode::HLT as u8,
        ];
        vm.run();

        assert_eq!(vm.registers[dest_reg as usize], value);
    }

    #[test]
    fn test_add_op_code() {
        let mut vm = VM::new();
        let dest_reg = 0 as u8;
        let left_reg = 1 as u8;
        let right_reg = 2 as u8;
        vm.program = vec![
            OpCode::LOADIMD as u8,
            left_reg,
            0,
            2,
            OpCode::LOADIMD as u8,
            right_reg,
            0,
            4,
            OpCode::ADD as u8,
            dest_reg,
            left_reg,
            right_reg,
            OpCode::HLT as u8,
        ];
        vm.run();

        assert_eq!(vm.registers[dest_reg as usize], 6);
    }

    #[test]
    fn test_sub_op_code() {
        let mut vm = VM::new();
        let dest_reg = 0 as u8;
        let left_reg = 1 as u8;
        let right_reg = 2 as u8;
        vm.program = vec![
            OpCode::LOADIMD as u8,
            left_reg,
            0,
            10,
            OpCode::LOADIMD as u8,
            right_reg,
            0,
            4,
            OpCode::SUB as u8,
            dest_reg,
            left_reg,
            right_reg,
            OpCode::HLT as u8,
        ];
        vm.run();

        assert_eq!(vm.registers[dest_reg as usize], 6);
    }

    #[test]
    fn test_mul_op_code() {
        let mut vm = VM::new();
        let dest_reg = 0 as u8;
        let left_reg = 1 as u8;
        let right_reg = 2 as u8;
        vm.program = vec![
            OpCode::LOADIMD as u8,
            left_reg,
            0,
            10,
            OpCode::LOADIMD as u8,
            right_reg,
            0,
            4,
            OpCode::MUL as u8,
            dest_reg,
            left_reg,
            right_reg,
            OpCode::HLT as u8,
        ];
        vm.run();

        assert_eq!(vm.registers[dest_reg as usize], 40);
    }

    #[test]
    fn test_div_op_code() {
        let mut vm = VM::new();
        let dest_reg = 0 as u8;
        let left_reg = 1 as u8;
        let right_reg = 2 as u8;
        vm.program = vec![
            OpCode::LOADIMD as u8,
            left_reg,
            0,
            10,
            OpCode::LOADIMD as u8,
            right_reg,
            0,
            3,
            OpCode::DIV as u8,
            dest_reg,
            left_reg,
            right_reg,
            OpCode::HLT as u8,
        ];
        vm.run();

        assert_eq!(vm.registers[dest_reg as usize], 3);
        assert_eq!(vm.remainder, 1);
    }

    #[test]
    fn test_jmp_op_code() {
        let mut vm = VM::new();
        vm.registers[1] = 5;
        vm.program = vec![
            OpCode::JMP as u8,
            1,
            OpCode::HLT as u8,
            OpCode::HLT as u8,
            OpCode::HLT as u8,
            OpCode::LOADIMD as u8, // Should jump here
            0,
            0,
            10,
            OpCode::HLT as u8,
        ];
        vm.run();

        assert_eq!(vm.registers[0], 10);
    }

    #[test]
    fn test_jmpf_op_code() {
        let mut vm = VM::new();
        vm.registers[1] = 3;
        vm.program = vec![
            OpCode::JMPF as u8,
            1,
            OpCode::HLT as u8,
            OpCode::HLT as u8,
            OpCode::HLT as u8,
            OpCode::LOADIMD as u8, // Should jump here
            0,
            0,
            10,
            OpCode::HLT as u8,
        ];
        vm.run();

        assert_eq!(vm.registers[0], 10);
    }

    #[test]
    fn test_jmpb_op_code() {
        let mut vm = VM::new();
        vm.pc = 5;
        vm.registers[1] = 7;
        vm.program = vec![
            OpCode::LOADIMD as u8, // Should jump here
            0,
            0,
            10,
            OpCode::HLT as u8,
            OpCode::JMPB as u8, // Starts here
            1,
            OpCode::HLT as u8,
        ];
        vm.run();

        assert_eq!(vm.registers[0], 10);
    }

    #[test]
    fn test_eq_op_code() {
        let mut vm = VM::new();
        vm.program = vec![OpCode::EQ as u8, 0, 1, 0, OpCode::HLT as u8];

        vm.registers[0] = 2;
        vm.registers[1] = 2;
        vm.run();
        assert_eq!(vm.cmp_flag, true);

        vm.registers[0] = 3;
        vm.registers[1] = 2;
        vm.pc = 0;
        vm.run();
        assert_eq!(vm.cmp_flag, false);
    }

    #[test]
    fn test_neq_op_code() {
        let mut vm = VM::new();
        vm.program = vec![OpCode::NEQ as u8, 0, 1, 0, OpCode::HLT as u8];

        vm.registers[0] = 2;
        vm.registers[1] = 3;
        vm.run();
        assert_eq!(vm.cmp_flag, true);

        vm.registers[0] = 2;
        vm.registers[1] = 2;
        vm.pc = 0;
        vm.run();
        assert_eq!(vm.cmp_flag, false);
    }

    #[test]
    fn test_gt_op_code() {
        let mut vm = VM::new();
        vm.program = vec![OpCode::GT as u8, 0, 1, 0, OpCode::HLT as u8];

        vm.registers[0] = 3;
        vm.registers[1] = 2;
        vm.run();
        assert_eq!(vm.cmp_flag, true);

        vm.registers[0] = 2;
        vm.registers[1] = 3;
        vm.pc = 0;
        vm.run();
        assert_eq!(vm.cmp_flag, false);
    }

    #[test]
    fn test_lt_op_code() {
        let mut vm = VM::new();
        vm.program = vec![OpCode::LT as u8, 0, 1, 0, OpCode::HLT as u8];

        vm.registers[0] = 2;
        vm.registers[1] = 3;
        vm.run();
        assert_eq!(vm.cmp_flag, true);

        vm.registers[0] = 3;
        vm.registers[1] = 2;
        vm.pc = 0;
        vm.run();
        assert_eq!(vm.cmp_flag, false);
    }

    #[test]
    fn test_ge_op_code() {
        let mut vm = VM::new();
        vm.program = vec![OpCode::GE as u8, 0, 1, 0, OpCode::HLT as u8];

        vm.registers[0] = 3;
        vm.registers[1] = 3;
        vm.run();
        assert_eq!(vm.cmp_flag, true);

        vm.registers[0] = 3;
        vm.registers[1] = 2;
        vm.pc = 0;
        vm.run();
        assert_eq!(vm.cmp_flag, true);

        vm.registers[0] = 3;
        vm.registers[1] = 4;
        vm.pc = 0;
        vm.run();
        assert_eq!(vm.cmp_flag, false);
    }

    #[test]
    fn test_le_op_code() {
        let mut vm = VM::new();
        vm.program = vec![OpCode::LE as u8, 0, 1, 0, OpCode::HLT as u8];

        vm.registers[0] = 3;
        vm.registers[1] = 3;
        vm.run();
        assert_eq!(vm.cmp_flag, true);

        vm.registers[0] = 2;
        vm.registers[1] = 3;
        vm.pc = 0;
        vm.run();
        assert_eq!(vm.cmp_flag, true);

        vm.registers[0] = 3;
        vm.registers[1] = 2;
        vm.pc = 0;
        vm.run();
        assert_eq!(vm.cmp_flag, false);
    }
}
