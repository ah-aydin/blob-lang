use std::usize;

use blob_asmlib::{LR_REG, REG_COUNT, SP_REG};
use blob_bc::OpCode;
use blob_executable::BlobExecutable;

const INITIAL_MEMORY_SIZE_IN_BYTES: usize = 4096;

pub struct VM {
    registers: [i64; REG_COUNT],
    hp: usize,
    pc: usize,
    program: Vec<u8>,
    memory: Vec<u8>,
    stack_start: usize,
    remainder: u32,
    cmp_flag: bool,
}

impl VM {
    pub fn new() -> VM {
        VM {
            registers: [0; REG_COUNT],
            hp: INITIAL_MEMORY_SIZE_IN_BYTES - 1,
            pc: 0,
            program: vec![],
            memory: vec![0; INITIAL_MEMORY_SIZE_IN_BYTES],
            stack_start: 0,
            remainder: 0,
            cmp_flag: false,
        }
    }

    pub fn run(&mut self) {
        loop {
            match self.decode_opcode() {
                OpCode::HLT => break,

                OpCode::LOAD => {
                    let dest_reg = self.get_reg();
                    let src_reg = self.get_reg();
                    self.registers[dest_reg] = self.registers[src_reg];
                }
                OpCode::LOADIMD => {
                    let register = self.get_reg();
                    let number = self.get_imd_val();
                    self.registers[register] = number;
                }

                OpCode::ADD => {
                    let dest_reg = self.get_reg();
                    let left_operand = self.registers[self.get_reg()];
                    let right_operand = self.registers[self.get_reg()];
                    self.registers[dest_reg] = left_operand + right_operand;
                }
                OpCode::ADDIMD => {
                    let dest_reg = self.get_reg();
                    let left_operand = self.registers[self.get_reg()];
                    let right_operand = self.get_imd_val();
                    self.registers[dest_reg] = left_operand + right_operand;
                }
                OpCode::SUB => {
                    let dest_reg = self.get_reg();
                    let left_operand = self.registers[self.get_reg()];
                    let right_operand = self.registers[self.get_reg()];
                    self.registers[dest_reg] = left_operand - right_operand;
                }
                OpCode::SUBIMD => {
                    let dest_reg = self.get_reg();
                    let left_operand = self.registers[self.get_reg()];
                    let right_operand = self.get_imd_val();
                    self.registers[dest_reg] = left_operand - right_operand;
                }
                OpCode::MUL => {
                    let dest_reg = self.get_reg();
                    let left_operand = self.registers[self.get_reg()];
                    let right_operand = self.registers[self.get_reg()];
                    self.registers[dest_reg] = left_operand * right_operand;
                }
                OpCode::MULIMD => {
                    let dest_reg = self.get_reg();
                    let left_operand = self.registers[self.get_reg()];
                    let right_operand = self.get_imd_val();
                    self.registers[dest_reg] = left_operand * right_operand;
                }
                OpCode::DIV => {
                    let dest_reg = self.get_reg();
                    let left_operand = self.registers[self.get_reg()];
                    let right_operand = self.registers[self.get_reg()];
                    self.registers[dest_reg] = left_operand / right_operand;
                    self.remainder = (left_operand % right_operand) as u32;
                }
                OpCode::DIVIMD => {
                    let dest_reg = self.get_reg();
                    let left_operand = self.registers[self.get_reg()];
                    let right_operand = self.get_imd_val();
                    self.registers[dest_reg] = left_operand / right_operand;
                    self.remainder = (left_operand % right_operand) as u32;
                }

                OpCode::JMP => {
                    let target = self.registers[self.get_reg()] as usize;
                    self.pc = target;
                }
                OpCode::JMPF => {
                    self.pc += self.registers[self.get_reg()] as usize;
                }
                OpCode::JMPFIMD => {
                    let jmp = self.get_imd_val() as usize;
                    self.pc += jmp;
                }
                OpCode::JMPB => {
                    let jmp = self.registers[self.get_reg()] as usize;
                    self.pc = self.pc - jmp;
                }
                OpCode::JMPBIMD => {
                    let jmp = self.get_imd_val() as usize;
                    self.pc = self.pc - jmp;
                }
                OpCode::JCMP => {
                    let register = self.get_reg();
                    if self.cmp_flag {
                        self.pc = self.registers[register] as usize;
                    }
                }
                OpCode::JCMPF => {
                    let register = self.get_reg();
                    if self.cmp_flag {
                        self.pc += self.registers[register] as usize;
                    }
                }
                OpCode::JCMPFIMD => {
                    let jmp = self.get_imd_val() as usize;
                    if self.cmp_flag {
                        self.pc += jmp;
                    }
                }
                OpCode::JCMPB => {
                    let register = self.get_reg();
                    if self.cmp_flag {
                        let jmp = self.registers[register] as usize;
                        self.pc = self.pc - jmp;
                    }
                }
                OpCode::JCMPBIMD => {
                    let jmp = self.get_imd_val() as usize;
                    if self.cmp_flag {
                        self.pc = self.pc - jmp;
                    }
                }

                OpCode::EQ => {
                    let left_operant = self.registers[self.get_reg()];
                    let right_operant = self.registers[self.get_reg()];
                    self.cmp_flag = left_operant == right_operant;
                }
                OpCode::EQIMD => {
                    let left_operant = self.registers[self.get_reg()];
                    let right_operant = self.get_imd_val();
                    self.cmp_flag = left_operant == right_operant;
                }
                OpCode::NEQ => {
                    let left_operant = self.registers[self.get_reg()];
                    let right_operant = self.registers[self.get_reg()];
                    self.cmp_flag = left_operant != right_operant;
                }
                OpCode::NEQIMD => {
                    let left_operant = self.registers[self.get_reg()];
                    let right_operant = self.get_imd_val();
                    self.cmp_flag = left_operant != right_operant;
                }
                OpCode::GT => {
                    let left_operant = self.registers[self.get_reg()];
                    let right_operant = self.registers[self.get_reg()];
                    self.cmp_flag = left_operant > right_operant;
                }
                OpCode::GTIMD => {
                    let left_operant = self.registers[self.get_reg()];
                    let right_operant = self.get_imd_val();
                    self.cmp_flag = left_operant > right_operant;
                }
                OpCode::LT => {
                    let left_operant = self.registers[self.get_reg()];
                    let right_operant = self.registers[self.get_reg()];
                    self.cmp_flag = left_operant < right_operant;
                }
                OpCode::LTIMD => {
                    let left_operant = self.registers[self.get_reg()];
                    let right_operant = self.get_imd_val();
                    self.cmp_flag = left_operant < right_operant;
                }
                OpCode::GE => {
                    let left_operant = self.registers[self.get_reg()];
                    let right_operant = self.registers[self.get_reg()];
                    self.cmp_flag = left_operant >= right_operant;
                }
                OpCode::GEIMD => {
                    let left_operant = self.registers[self.get_reg()];
                    let right_operant = self.get_imd_val();
                    self.cmp_flag = left_operant >= right_operant;
                }
                OpCode::LE => {
                    let left_operant = self.registers[self.get_reg()];
                    let right_operant = self.registers[self.get_reg()];
                    self.cmp_flag = left_operant <= right_operant;
                }
                OpCode::LEIMD => {
                    let left_operant = self.registers[self.get_reg()];
                    let right_operant = self.get_imd_val();
                    self.cmp_flag = left_operant <= right_operant;
                }

                OpCode::PUSH => {
                    let data = self.registers[self.get_reg()];
                    let sp = self.registers[SP_REG] as usize;

                    if sp + 8 > self.hp || sp + 8 > self.memory.len() {
                        todo!("Must expand stack. Don't have enough memory. Implement memeory expansion");
                    }

                    self.memory[sp..sp + 8].copy_from_slice(&data.to_be_bytes());
                    self.registers[SP_REG] += 8;
                }
                OpCode::POP => {
                    let sp = self.registers[SP_REG] as usize;
                    let bytes: [u8; 8] = self.memory[sp - 8..sp].try_into().unwrap();

                    self.registers[self.get_reg() as usize] = i64::from_be_bytes(bytes);
                    self.registers[SP_REG] -= 8;
                }

                OpCode::ALOC => {
                    let bytes = self.registers[self.get_reg()];
                    let new_hp = self.hp - bytes as usize;
                    if new_hp < self.registers[SP_REG] as usize {
                        todo!(
                            "Must expand heap. Don't have enough memory. Implement memory expansion"
                        );
                    }
                    self.hp = new_hp;
                }

                OpCode::IGL => {
                    unreachable!("Got IGL OpCode in pc={}", self.pc);
                }
            }
        }
    }

    pub fn set_program(&mut self, executable: BlobExecutable) {
        self.program = executable.get_program().clone();

        self.memory.fill(0);
        self.memory[0..executable.get_global_data().len()]
            .copy_from_slice(&executable.get_global_data());
        self.stack_start = executable.get_global_data().len();

        self.registers[SP_REG] = self.stack_start as i64;
        self.registers[LR_REG] = self.stack_start as i64;
        self.pc = 0;
    }

    pub fn set_and_run_program(&mut self, executable: BlobExecutable) {
        self.set_program(executable);
        self.run();
    }

    fn decode_opcode(&mut self) -> OpCode {
        let op_code = OpCode::from(self.program[self.pc as usize]);
        self.pc += 1;
        return op_code;
    }

    fn get_reg(&mut self) -> usize {
        let reg = self.program[self.pc as usize];
        self.pc += 1;
        reg as usize
    }

    fn get_imd_val(&mut self) -> i64 {
        let pc = self.pc as usize;
        let imd = i16::from_be_bytes([self.program[pc], self.program[pc + 1]]);
        self.pc += 2;
        imd as i64
    }

    pub fn print_regs(&self) {
        self.registers.iter().enumerate().for_each(|(i, reg)| {
            if i == LR_REG {
                println!("LR : {reg}");
            } else if i == SP_REG {
                println!("SP : {reg}");
            } else {
                println!("R{:0>2}: {reg}", i);
            }
        });
        println!("PC : {}", self.pc);
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

        assert_eq!(vm.memory[0..8], [0, 0, 0, 0, 0, 0, 1, 1]);
        assert_eq!(vm.memory[8..16], [0, 0, 0, 0, 0, 0, 0, 255]);
        assert_eq!(vm.registers[SP_REG], 16);
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

        assert_eq!(vm.registers[2], 255);
        assert_eq!(vm.registers[3], 1078);
        assert_eq!(vm.registers[SP_REG], 0);
    }

    #[test]
    fn aloc_op_code() {
        let mut vm = VM::new();
        vm.program = vec![OpCode::ALOC as u8, 0, OpCode::HLT as u8];

        vm.registers[0] = 128;
        vm.run();
        assert_eq!(vm.memory.len() - 128 - 1, vm.hp);
    }
}
