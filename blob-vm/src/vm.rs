use blob_bc::OpCode;
use blob_executable::BlobExecutable;

pub struct VM {
    registers: [i32; 32],
    program: Vec<u8>,
    heap: Vec<u8>,
    stack: Vec<u8>,
    stack_start: usize,
    pc: usize,
    remainder: u32,
    cmp_flag: bool,
}

impl VM {
    pub fn new() -> VM {
        VM {
            registers: [0; 32],
            program: vec![],
            heap: vec![],
            stack: vec![],
            stack_start: 0,
            pc: 0,
            remainder: 0,
            cmp_flag: false,
        }
    }

    pub fn run(&mut self) {
        while self.execute_instruction() {}
    }

    pub fn set_program(&mut self, executable: BlobExecutable) {
        self.pc = 0;
        self.program = executable.get_program().clone();
        self.stack = executable.get_global_data().clone();
        self.stack_start = self.stack.len();
    }

    pub fn set_and_run_program(&mut self, executable: BlobExecutable) {
        self.set_program(executable);
        self.run();
    }

    pub fn execute_instruction(&mut self) -> bool {
        if self.pc >= self.program.len() {
            return false;
        }
        match self.decode_opcode() {
            OpCode::HLT => false,

            OpCode::LOAD => {
                let dest_reg = self.next_8_bits() as usize;
                let src_reg = self.next_8_bits() as usize;
                self.registers[dest_reg] = self.registers[src_reg];
                true
            }
            OpCode::LOADIMD => {
                let register = self.next_8_bits() as usize;
                let number = self.next_16_bits() as u16;
                self.registers[register] = number as i32;
                true
            }

            OpCode::ADD => {
                let dest_reg = self.next_8_bits() as usize;
                let left_operand = self.registers[self.next_8_bits() as usize];
                let right_operand = self.registers[self.next_8_bits() as usize];
                self.registers[dest_reg] = left_operand + right_operand;
                true
            }
            OpCode::ADDIMD => {
                let dest_reg = self.next_8_bits() as usize;
                let left_operand = self.registers[self.next_8_bits() as usize];
                let right_operand = self.next_16_bits() as i32;
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
            OpCode::SUBIMD => {
                let dest_reg = self.next_8_bits() as usize;
                let left_operand = self.registers[self.next_8_bits() as usize];
                let right_operand = self.next_16_bits() as i32;
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
            OpCode::MULIMD => {
                let dest_reg = self.next_8_bits() as usize;
                let left_operand = self.registers[self.next_8_bits() as usize];
                let right_operand = self.next_16_bits() as i32;
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
            OpCode::DIVIMD => {
                let dest_reg = self.next_8_bits() as usize;
                let left_operand = self.registers[self.next_8_bits() as usize];
                let right_operand = self.next_16_bits() as i32;
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
            OpCode::JMPFIMD => {
                self.pc += self.next_16_bits() as usize;
                true
            }
            OpCode::JMPB => {
                let jmp = self.registers[self.next_8_bits() as usize] as isize;
                self.pc = (self.pc as isize - jmp) as usize;
                true
            }
            OpCode::JMPBIMD => {
                let jmp = self.next_16_bits() as isize;
                self.pc = (self.pc as isize - jmp) as usize;
                true
            }
            OpCode::JCMP => {
                let register = self.next_8_bits() as usize;
                if self.cmp_flag {
                    self.pc = self.registers[register] as usize;
                }
                true
            }
            OpCode::JCMPF => {
                let register = self.next_8_bits() as usize;
                if self.cmp_flag {
                    self.pc += self.registers[register] as usize;
                }
                true
            }
            OpCode::JCMPFIMD => {
                let jmp = self.next_16_bits() as usize;
                if self.cmp_flag {
                    self.pc += jmp;
                }
                true
            }
            OpCode::JCMPB => {
                let register = self.next_8_bits() as usize;
                if self.cmp_flag {
                    let jmp = self.registers[register] as isize;
                    self.pc = (self.pc as isize - jmp) as usize;
                }
                true
            }
            OpCode::JCMPBIMD => {
                let jmp = self.next_16_bits() as isize;
                if self.cmp_flag {
                    self.pc = (self.pc as isize - jmp) as usize;
                }
                true
            }

            OpCode::EQ => {
                let left_operant = self.registers[self.next_8_bits() as usize];
                let right_operant = self.registers[self.next_8_bits() as usize];
                self.cmp_flag = left_operant == right_operant;
                true
            }
            OpCode::EQIMD => {
                let left_operant = self.registers[self.next_8_bits() as usize];
                let right_operant = self.next_16_bits() as i32;
                self.cmp_flag = left_operant == right_operant;
                true
            }
            OpCode::NEQ => {
                let left_operant = self.registers[self.next_8_bits() as usize];
                let right_operant = self.registers[self.next_8_bits() as usize];
                self.cmp_flag = left_operant != right_operant;
                true
            }
            OpCode::NEQIMD => {
                let left_operant = self.registers[self.next_8_bits() as usize];
                let right_operant = self.next_16_bits() as i32;
                self.cmp_flag = left_operant != right_operant;
                true
            }
            OpCode::GT => {
                let left_operant = self.registers[self.next_8_bits() as usize];
                let right_operant = self.registers[self.next_8_bits() as usize];
                self.cmp_flag = left_operant > right_operant;
                true
            }
            OpCode::GTIMD => {
                let left_operant = self.registers[self.next_8_bits() as usize];
                let right_operant = self.next_16_bits() as i32;
                self.cmp_flag = left_operant > right_operant;
                true
            }
            OpCode::LT => {
                let left_operant = self.registers[self.next_8_bits() as usize];
                let right_operant = self.registers[self.next_8_bits() as usize];
                self.cmp_flag = left_operant < right_operant;
                true
            }
            OpCode::LTIMD => {
                let left_operant = self.registers[self.next_8_bits() as usize];
                let right_operant = self.next_16_bits() as i32;
                self.cmp_flag = left_operant < right_operant;
                true
            }
            OpCode::GE => {
                let left_operant = self.registers[self.next_8_bits() as usize];
                let right_operant = self.registers[self.next_8_bits() as usize];
                self.cmp_flag = left_operant >= right_operant;
                true
            }
            OpCode::GEIMD => {
                let left_operant = self.registers[self.next_8_bits() as usize];
                let right_operant = self.next_16_bits() as i32;
                self.cmp_flag = left_operant >= right_operant;
                true
            }
            OpCode::LE => {
                let left_operant = self.registers[self.next_8_bits() as usize];
                let right_operant = self.registers[self.next_8_bits() as usize];
                self.cmp_flag = left_operant <= right_operant;
                true
            }
            OpCode::LEIMD => {
                let left_operant = self.registers[self.next_8_bits() as usize];
                let right_operant = self.next_16_bits() as i32;
                self.cmp_flag = left_operant <= right_operant;
                true
            }

            OpCode::PUSH => {
                let data = self.registers[self.next_8_bits() as usize];
                self.stack.extend_from_slice(&data.to_be_bytes());
                true
            }
            OpCode::POP => {
                let bytes: [u8; 4] = self.stack[self.stack.len() - 4..self.stack.len()]
                    .try_into()
                    .unwrap();
                self.stack.resize(self.stack.len() - 4, 0);

                self.registers[self.next_8_bits() as usize] = i32::from_be_bytes(bytes);
                true
            }

            OpCode::ALOC => {
                let bytes = self.registers[self.next_8_bits() as usize];
                let new_size = self.heap.len() as i32 + bytes;
                self.heap.resize(new_size as usize, 0);
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

    pub fn print_regs(&self) {
        self.registers
            .iter()
            .enumerate()
            .for_each(|(i, reg)| println!("R{:0>2}: {reg}", i));
    }
}

#[cfg(test)]
mod test {
    use super::*;

    #[test]
    fn load_op_code() {
        let mut vm = VM::new();
        let dest_reg = 0;
        let src_reg: u8 = 1;
        let value = 80;
        vm.registers[src_reg as usize] = value;
        vm.program = vec![OpCode::LOAD as u8, dest_reg, src_reg, OpCode::HLT as u8];
        vm.run();

        assert_eq!(vm.registers[dest_reg as usize], value);
    }

    #[test]
    fn loadimd_op_code() {
        let mut vm = VM::new();
        vm.program = vec![OpCode::LOADIMD as u8, 0, 1, 244, OpCode::HLT as u8];
        vm.run();

        assert_eq!(vm.registers[0], 500);
    }

    #[test]
    fn add_op_code() {
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
    fn addimd_op_code() {
        let mut vm = VM::new();
        let dest_reg = 0 as u8;
        let left_reg = 1 as u8;
        vm.program = vec![
            OpCode::LOADIMD as u8,
            left_reg,
            0,
            2,
            OpCode::ADDIMD as u8,
            dest_reg,
            left_reg,
            1, // Imediate 500 value
            244,
            OpCode::HLT as u8,
        ];
        vm.run();

        assert_eq!(vm.registers[dest_reg as usize], 502);
    }

    #[test]
    fn sub_op_code() {
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
    fn subimd_op_code() {
        let mut vm = VM::new();
        let dest_reg = 0 as u8;
        let left_reg = 1 as u8;
        vm.program = vec![
            OpCode::LOADIMD as u8,
            left_reg,
            0,
            2,
            OpCode::SUBIMD as u8,
            dest_reg,
            left_reg,
            1, // Imediate 500 value
            244,
            OpCode::HLT as u8,
        ];
        vm.run();

        assert_eq!(vm.registers[dest_reg as usize], -498);
    }

    #[test]
    fn mul_op_code() {
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
    fn mulimd_op_code() {
        let mut vm = VM::new();
        let dest_reg = 0 as u8;
        let left_reg = 1 as u8;
        vm.program = vec![
            OpCode::LOADIMD as u8,
            left_reg,
            0,
            2,
            OpCode::MULIMD as u8,
            dest_reg,
            left_reg,
            1, // Imediate 500 value
            244,
            OpCode::HLT as u8,
        ];
        vm.run();

        assert_eq!(vm.registers[dest_reg as usize], 1000);
    }

    #[test]
    fn div_op_code() {
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
    fn divimd_op_code() {
        let mut vm = VM::new();
        let dest_reg = 0 as u8;
        let left_reg = 1 as u8;
        vm.program = vec![
            OpCode::LOADIMD as u8,
            left_reg,
            0,
            10,
            OpCode::DIVIMD as u8,
            dest_reg,
            left_reg,
            0,
            3,
            OpCode::HLT as u8,
        ];
        vm.run();

        assert_eq!(vm.registers[dest_reg as usize], 3);
        assert_eq!(vm.remainder, 1);
    }

    #[test]
    fn jmp_op_code() {
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
    fn jmpf_op_code() {
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
    fn jmpfimd_op_code() {
        let mut vm = VM::new();
        vm.program = vec![
            OpCode::JMPFIMD as u8,
            0,
            3,
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
    fn jmpb_op_code() {
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
    fn jmpbimd_op_code() {
        let mut vm = VM::new();
        vm.pc = 5;
        vm.program = vec![
            OpCode::LOADIMD as u8, // Should jump here
            0,
            0,
            10,
            OpCode::HLT as u8,
            OpCode::JMPBIMD as u8, // Starts here
            0,
            8,
            OpCode::HLT as u8,
        ];
        vm.run();

        assert_eq!(vm.registers[0], 10);
    }

    #[test]
    fn jcmp_op_code() {
        let mut vm = VM::new();
        vm.program = vec![
            OpCode::JCMP as u8,
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

        vm.registers[1] = 5;
        vm.cmp_flag = true;
        vm.run();
        assert_eq!(vm.registers[0], 10);

        vm.registers[0] = 0;
        vm.pc = 0;
        vm.cmp_flag = false;
        vm.run();
        assert_eq!(vm.registers[0], 0);
    }

    #[test]
    fn jcmpf_op_code() {
        let mut vm = VM::new();
        vm.program = vec![
            OpCode::JCMPF as u8,
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

        vm.registers[1] = 3;
        vm.cmp_flag = true;
        vm.run();
        assert_eq!(vm.registers[0], 10);

        vm.registers[0] = 0;
        vm.pc = 0;
        vm.cmp_flag = false;
        vm.run();
        assert_eq!(vm.registers[0], 0);
    }

    #[test]
    fn jcmpfimd_op_code() {
        let mut vm = VM::new();
        vm.program = vec![
            OpCode::JCMPFIMD as u8,
            0,
            3,
            OpCode::HLT as u8,
            OpCode::HLT as u8,
            OpCode::HLT as u8,
            OpCode::LOADIMD as u8, // Should jump here
            0,
            0,
            10,
            OpCode::HLT as u8,
        ];

        vm.cmp_flag = true;
        vm.run();
        assert_eq!(vm.registers[0], 10);

        vm.registers[0] = 0;
        vm.pc = 0;
        vm.cmp_flag = false;
        vm.run();
        assert_eq!(vm.registers[0], 0);
    }

    #[test]
    fn jcmpb_op_code() {
        let mut vm = VM::new();
        vm.program = vec![
            OpCode::LOADIMD as u8, // Should jump here
            0,
            0,
            10,
            OpCode::HLT as u8,
            OpCode::JCMPB as u8, // Starts here
            1,
            OpCode::HLT as u8,
        ];

        vm.registers[1] = 7;
        vm.pc = 5;
        vm.cmp_flag = true;
        vm.run();
        assert_eq!(vm.registers[0], 10);

        vm.registers[0] = 0;
        vm.pc = 5;
        vm.cmp_flag = false;
        vm.run();
        assert_eq!(vm.registers[0], 0);
    }

    #[test]
    fn jcmpbimd_op_code() {
        let mut vm = VM::new();
        vm.program = vec![
            OpCode::LOADIMD as u8, // Should jump here
            0,
            0,
            10,
            OpCode::HLT as u8,
            OpCode::JCMPBIMD as u8, // Starts here
            0,
            8,
            OpCode::HLT as u8,
        ];

        vm.pc = 5;
        vm.cmp_flag = true;
        vm.run();
        assert_eq!(vm.registers[0], 10);

        vm.registers[0] = 0;
        vm.pc = 5;
        vm.cmp_flag = false;
        vm.run();
        assert_eq!(vm.registers[0], 0);
    }

    #[test]
    fn eq_op_code() {
        let mut vm = VM::new();
        vm.program = vec![OpCode::EQ as u8, 0, 1, OpCode::HLT as u8];

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
    fn eqimd_op_code() {
        let mut vm = VM::new();
        vm.program = vec![OpCode::EQIMD as u8, 0, 0, 2, OpCode::HLT as u8];

        vm.registers[0] = 2;
        vm.run();
        assert_eq!(vm.cmp_flag, true);

        vm.registers[0] = 3;
        vm.pc = 0;
        vm.run();
        assert_eq!(vm.cmp_flag, false);
    }

    #[test]
    fn neq_op_code() {
        let mut vm = VM::new();
        vm.program = vec![OpCode::NEQ as u8, 0, 1, OpCode::HLT as u8];

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
    fn neqimd_op_code() {
        let mut vm = VM::new();
        vm.program = vec![OpCode::NEQIMD as u8, 0, 0, 3, OpCode::HLT as u8];

        vm.registers[0] = 2;
        vm.run();
        assert_eq!(vm.cmp_flag, true);

        vm.registers[0] = 3;
        vm.pc = 0;
        vm.run();
        assert_eq!(vm.cmp_flag, false);
    }

    #[test]
    fn gt_op_code() {
        let mut vm = VM::new();
        vm.program = vec![OpCode::GT as u8, 0, 1, OpCode::HLT as u8];

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
    fn gtimd_op_code() {
        let mut vm = VM::new();
        vm.program = vec![OpCode::GTIMD as u8, 0, 0, 2, OpCode::HLT as u8];

        vm.registers[0] = 3;
        vm.run();
        assert_eq!(vm.cmp_flag, true);

        vm.registers[0] = 1;
        vm.pc = 0;
        vm.run();
        assert_eq!(vm.cmp_flag, false);
    }

    #[test]
    fn lt_op_code() {
        let mut vm = VM::new();
        vm.program = vec![OpCode::LT as u8, 0, 1, OpCode::HLT as u8];

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
    fn ltimd_op_code() {
        let mut vm = VM::new();
        vm.program = vec![OpCode::LTIMD as u8, 0, 0, 3, OpCode::HLT as u8];

        vm.registers[0] = 2;
        vm.run();
        assert_eq!(vm.cmp_flag, true);

        vm.registers[0] = 4;
        vm.pc = 0;
        vm.run();
        assert_eq!(vm.cmp_flag, false);
    }

    #[test]
    fn ge_op_code() {
        let mut vm = VM::new();
        vm.program = vec![OpCode::GE as u8, 0, 1, OpCode::HLT as u8];

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
    fn geimd_op_code() {
        let mut vm = VM::new();
        vm.program = vec![OpCode::GEIMD as u8, 0, 0, 3, OpCode::HLT as u8];

        vm.registers[0] = 3;
        vm.run();
        assert_eq!(vm.cmp_flag, true);

        vm.registers[0] = 4;
        vm.pc = 0;
        vm.run();
        assert_eq!(vm.cmp_flag, true);

        vm.registers[0] = 2;
        vm.pc = 0;
        vm.run();
        assert_eq!(vm.cmp_flag, false);
    }

    #[test]
    fn le_op_code() {
        let mut vm = VM::new();
        vm.program = vec![OpCode::LE as u8, 0, 1, OpCode::HLT as u8];

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

    #[test]
    fn leimd_op_code() {
        let mut vm = VM::new();
        vm.program = vec![OpCode::LEIMD as u8, 0, 0, 3, OpCode::HLT as u8];

        vm.registers[0] = 3;
        vm.run();
        assert_eq!(vm.cmp_flag, true);

        vm.registers[0] = 2;
        vm.pc = 0;
        vm.run();
        assert_eq!(vm.cmp_flag, true);

        vm.registers[0] = 4;
        vm.pc = 0;
        vm.run();
        assert_eq!(vm.cmp_flag, false);
    }

    #[test]
    fn push_op_code() {
        let mut vm = VM::new();
        vm.registers[0] = 257;
        vm.registers[1] = 255;
        vm.program = vec![
            OpCode::PUSH as u8,
            0,
            OpCode::PUSH as u8,
            1,
            OpCode::HLT as u8,
        ];
        vm.run();

        assert_eq!(vm.stack[0..4], [0, 0, 1, 1]);
        assert_eq!(vm.stack[4..8], [0, 0, 0, 255]);
    }

    #[test]
    fn pop_op_code() {
        let mut vm = VM::new();
        vm.registers[0] = 1078;
        vm.registers[1] = 255;
        vm.program = vec![
            OpCode::PUSH as u8,
            0,
            OpCode::PUSH as u8,
            1,
            OpCode::POP as u8,
            2,
            OpCode::POP as u8,
            3,
            OpCode::HLT as u8,
        ];
        vm.run();

        assert_eq!(vm.stack.len(), 0);
        assert_eq!(vm.registers[2], 255);
        assert_eq!(vm.registers[3], 1078);
    }

    #[test]
    fn aloc_op_code() {
        let mut vm = VM::new();
        vm.program = vec![OpCode::ALOC as u8, 0, OpCode::HLT as u8];

        vm.registers[0] = 4096;
        vm.run();
        assert_eq!(vm.heap.len(), 4096);
    }
}
