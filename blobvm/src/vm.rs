use blobbc::instructions::OpCode;

pub struct VM {
    registers: [i32; 32],
    program: Vec<u8>,
    pc: usize,
    remainder: u32,
}

impl VM {
    pub fn new() -> Self {
        VM {
            registers: [0; 32],
            program: vec![],
            pc: 0,
            remainder: 0,
        }
    }

    pub fn run(&mut self) {
        loop {
            if self.pc >= self.program.len() {
                break;
            }
            match self.decode_opcode() {
                OpCode::HLT => {
                    println!("HLT encountered");
                    break;
                }
                OpCode::LOAD => {
                    let register = self.next_8_bits() as usize;
                    let number = self.next_16_bits() as u16;
                    self.registers[register] = number as i32;
                }
                OpCode::ADD => {
                    let destination_register = self.next_8_bits() as usize;
                    let left_operand = self.registers[self.next_8_bits() as usize];
                    let right_operand = self.registers[self.next_8_bits() as usize];
                    self.registers[destination_register] = left_operand + right_operand;
                }
                OpCode::SUB => {
                    let destination_register = self.next_8_bits() as usize;
                    let left_operand = self.registers[self.next_8_bits() as usize];
                    let right_operand = self.registers[self.next_8_bits() as usize];
                    self.registers[destination_register] = left_operand - right_operand;
                }
                OpCode::MUL => {
                    let destination_register = self.next_8_bits() as usize;
                    let left_operand = self.registers[self.next_8_bits() as usize];
                    let right_operand = self.registers[self.next_8_bits() as usize];
                    self.registers[destination_register] = left_operand * right_operand;
                }
                OpCode::DIV => {
                    let destination_register = self.next_8_bits() as usize;
                    let left_operand = self.registers[self.next_8_bits() as usize];
                    let right_operand = self.registers[self.next_8_bits() as usize];
                    self.registers[destination_register] = left_operand / right_operand;
                    self.remainder = (left_operand % right_operand) as u32;
                }
                OpCode::IGL => {
                    unreachable!("Got IGL OpCode in pc={}", self.pc);
                }
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
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn test_load_op_code() {
        let mut vm = VM::new();
        vm.program = vec![OpCode::LOAD as u8, 0, 1, 244, OpCode::HLT as u8];
        vm.run();

        assert_eq!(vm.registers[0], 500);
    }

    #[test]
    fn test_add_op_code() {
        let mut vm = VM::new();
        let destination_register = 0 as u8;
        let left_register = 1 as u8;
        let right_register = 2 as u8;
        vm.program = vec![
            OpCode::LOAD as u8,
            left_register,
            0,
            2,
            OpCode::LOAD as u8,
            right_register,
            0,
            4,
            OpCode::ADD as u8,
            destination_register,
            left_register,
            right_register,
            OpCode::HLT as u8,
        ];
        vm.run();

        assert_eq!(vm.registers[destination_register as usize], 6);
    }

    #[test]
    fn test_sub_op_code() {
        let mut vm = VM::new();
        let destination_register = 0 as u8;
        let left_register = 1 as u8;
        let right_register = 2 as u8;
        vm.program = vec![
            OpCode::LOAD as u8,
            left_register,
            0,
            10,
            OpCode::LOAD as u8,
            right_register,
            0,
            4,
            OpCode::SUB as u8,
            destination_register,
            left_register,
            right_register,
            OpCode::HLT as u8,
        ];
        vm.run();

        assert_eq!(vm.registers[destination_register as usize], 6);
    }

    #[test]
    fn test_mul_op_code() {
        let mut vm = VM::new();
        let destination_register = 0 as u8;
        let left_register = 1 as u8;
        let right_register = 2 as u8;
        vm.program = vec![
            OpCode::LOAD as u8,
            left_register,
            0,
            10,
            OpCode::LOAD as u8,
            right_register,
            0,
            4,
            OpCode::MUL as u8,
            destination_register,
            left_register,
            right_register,
            OpCode::HLT as u8,
        ];
        vm.run();

        assert_eq!(vm.registers[destination_register as usize], 40);
    }

    #[test]
    fn test_div_op_code() {
        let mut vm = VM::new();
        let destination_register = 0 as u8;
        let left_register = 1 as u8;
        let right_register = 2 as u8;
        vm.program = vec![
            OpCode::LOAD as u8,
            left_register,
            0,
            10,
            OpCode::LOAD as u8,
            right_register,
            0,
            3,
            OpCode::DIV as u8,
            destination_register,
            left_register,
            right_register,
            OpCode::HLT as u8,
        ];
        vm.run();

        assert_eq!(vm.registers[destination_register as usize], 3);
        assert_eq!(vm.remainder, 1);
    }
}
