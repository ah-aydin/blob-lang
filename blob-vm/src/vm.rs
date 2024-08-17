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
                OpCode::Hlt => break,

                OpCode::Load => {
                    let dest_reg = self.get_reg();
                    let src_reg = self.get_reg();
                    self.registers[dest_reg] = self.registers[src_reg];
                }
                OpCode::LoadImd => {
                    let dest_reg = self.get_reg();
                    let number = self.get_imd_val();
                    self.registers[dest_reg] = number;
                }
                OpCode::LoadWord => {
                    let dest_reg = self.get_reg();
                    let mem = self.get_word();
                    self.registers[dest_reg] = mem as i64;
                }
                OpCode::LoadMemByte => {
                    let dest_reg = self.get_reg();
                    let mem = self.registers[self.get_reg()] as usize;
                    self.registers[dest_reg] =
                        i64::from_be_bytes([0, 0, 0, 0, 0, 0, 0, self.memory[mem]]);
                }
                OpCode::LoadMemQuaterWord => {
                    let dest_reg = self.get_reg();
                    let mem = self.registers[self.get_reg()] as usize;
                    self.registers[dest_reg] = i64::from_be_bytes([
                        0,
                        0,
                        0,
                        0,
                        0,
                        0,
                        self.memory[mem],
                        self.memory[mem + 1],
                    ]);
                }
                OpCode::LoadMemHalfWord => {
                    let dest_reg = self.get_reg();
                    let mem = self.registers[self.get_reg()] as usize;
                    self.registers[dest_reg] = i64::from_be_bytes([
                        0,
                        0,
                        0,
                        0,
                        self.memory[mem],
                        self.memory[mem + 1],
                        self.memory[mem + 2],
                        self.memory[mem + 3],
                    ]);
                }
                OpCode::LoadMemWord => {
                    let dest_reg = self.get_reg();
                    let mem = self.registers[self.get_reg()] as usize;
                    self.registers[dest_reg] = i64::from_be_bytes([
                        self.memory[mem],
                        self.memory[mem + 1],
                        self.memory[mem + 2],
                        self.memory[mem + 3],
                        self.memory[mem + 4],
                        self.memory[mem + 5],
                        self.memory[mem + 6],
                        self.memory[mem + 7],
                    ]);
                }

                OpCode::Add => {
                    let dest_reg = self.get_reg();
                    let left_operand = self.registers[self.get_reg()];
                    let right_operand = self.registers[self.get_reg()];
                    self.registers[dest_reg] = left_operand + right_operand;
                }
                OpCode::AddImd => {
                    let dest_reg = self.get_reg();
                    let left_operand = self.registers[self.get_reg()];
                    let right_operand = self.get_imd_val();
                    self.registers[dest_reg] = left_operand + right_operand;
                }
                OpCode::Sub => {
                    let dest_reg = self.get_reg();
                    let left_operand = self.registers[self.get_reg()];
                    let right_operand = self.registers[self.get_reg()];
                    self.registers[dest_reg] = left_operand - right_operand;
                }
                OpCode::SubImd => {
                    let dest_reg = self.get_reg();
                    let left_operand = self.registers[self.get_reg()];
                    let right_operand = self.get_imd_val();
                    self.registers[dest_reg] = left_operand - right_operand;
                }
                OpCode::Mul => {
                    let dest_reg = self.get_reg();
                    let left_operand = self.registers[self.get_reg()];
                    let right_operand = self.registers[self.get_reg()];
                    self.registers[dest_reg] = left_operand * right_operand;
                }
                OpCode::MulImd => {
                    let dest_reg = self.get_reg();
                    let left_operand = self.registers[self.get_reg()];
                    let right_operand = self.get_imd_val();
                    self.registers[dest_reg] = left_operand * right_operand;
                }
                OpCode::Div => {
                    let dest_reg = self.get_reg();
                    let left_operand = self.registers[self.get_reg()];
                    let right_operand = self.registers[self.get_reg()];
                    self.registers[dest_reg] = left_operand / right_operand;
                    self.remainder = (left_operand % right_operand) as u32;
                }
                OpCode::DivImd => {
                    let dest_reg = self.get_reg();
                    let left_operand = self.registers[self.get_reg()];
                    let right_operand = self.get_imd_val();
                    self.registers[dest_reg] = left_operand / right_operand;
                    self.remainder = (left_operand % right_operand) as u32;
                }

                OpCode::Jmp => {
                    let target = self.registers[self.get_reg()] as usize;
                    self.pc = target;
                }
                OpCode::JmpF => {
                    self.pc += self.registers[self.get_reg()] as usize;
                }
                OpCode::JmpFImd => {
                    let jmp = self.get_imd_val() as usize;
                    self.pc += jmp;
                }
                OpCode::JmpB => {
                    let jmp = self.registers[self.get_reg()] as usize;
                    self.pc = self.pc - jmp;
                }
                OpCode::JmpBImd => {
                    let jmp = self.get_imd_val() as usize;
                    self.pc = self.pc - jmp;
                }
                OpCode::JCmp => {
                    let register = self.get_reg();
                    if self.cmp_flag {
                        self.pc = self.registers[register] as usize;
                    }
                }
                OpCode::JCmpF => {
                    let register = self.get_reg();
                    if self.cmp_flag {
                        self.pc += self.registers[register] as usize;
                    }
                }
                OpCode::JCmpFImd => {
                    let jmp = self.get_imd_val() as usize;
                    if self.cmp_flag {
                        self.pc += jmp;
                    }
                }
                OpCode::JCmpB => {
                    let register = self.get_reg();
                    if self.cmp_flag {
                        let jmp = self.registers[register] as usize;
                        self.pc = self.pc - jmp;
                    }
                }
                OpCode::JCmpBImd => {
                    let jmp = self.get_imd_val() as usize;
                    if self.cmp_flag {
                        self.pc = self.pc - jmp;
                    }
                }

                OpCode::Eq => {
                    let left_operant = self.registers[self.get_reg()];
                    let right_operant = self.registers[self.get_reg()];
                    self.cmp_flag = left_operant == right_operant;
                }
                OpCode::EqImd => {
                    let left_operant = self.registers[self.get_reg()];
                    let right_operant = self.get_imd_val();
                    self.cmp_flag = left_operant == right_operant;
                }
                OpCode::NEq => {
                    let left_operant = self.registers[self.get_reg()];
                    let right_operant = self.registers[self.get_reg()];
                    self.cmp_flag = left_operant != right_operant;
                }
                OpCode::NEqImd => {
                    let left_operant = self.registers[self.get_reg()];
                    let right_operant = self.get_imd_val();
                    self.cmp_flag = left_operant != right_operant;
                }
                OpCode::Gt => {
                    let left_operant = self.registers[self.get_reg()];
                    let right_operant = self.registers[self.get_reg()];
                    self.cmp_flag = left_operant > right_operant;
                }
                OpCode::GtImd => {
                    let left_operant = self.registers[self.get_reg()];
                    let right_operant = self.get_imd_val();
                    self.cmp_flag = left_operant > right_operant;
                }
                OpCode::Lt => {
                    let left_operant = self.registers[self.get_reg()];
                    let right_operant = self.registers[self.get_reg()];
                    self.cmp_flag = left_operant < right_operant;
                }
                OpCode::LtImd => {
                    let left_operant = self.registers[self.get_reg()];
                    let right_operant = self.get_imd_val();
                    self.cmp_flag = left_operant < right_operant;
                }
                OpCode::Ge => {
                    let left_operant = self.registers[self.get_reg()];
                    let right_operant = self.registers[self.get_reg()];
                    self.cmp_flag = left_operant >= right_operant;
                }
                OpCode::GeImd => {
                    let left_operant = self.registers[self.get_reg()];
                    let right_operant = self.get_imd_val();
                    self.cmp_flag = left_operant >= right_operant;
                }
                OpCode::Le => {
                    let left_operant = self.registers[self.get_reg()];
                    let right_operant = self.registers[self.get_reg()];
                    self.cmp_flag = left_operant <= right_operant;
                }
                OpCode::LeiImd => {
                    let left_operant = self.registers[self.get_reg()];
                    let right_operant = self.get_imd_val();
                    self.cmp_flag = left_operant <= right_operant;
                }

                OpCode::Push => {
                    let data = self.registers[self.get_reg()];
                    let sp = self.registers[SP_REG] as usize;

                    if sp + 8 > self.hp || sp + 8 > self.memory.len() {
                        todo!("Must expand stack. Don't have enough memory. Implement memeory expansion");
                    }

                    self.memory[sp..sp + 8].copy_from_slice(&data.to_be_bytes());
                    self.registers[SP_REG] += 8;
                }
                OpCode::Pop => {
                    let sp = self.registers[SP_REG] as usize;
                    let bytes: [u8; 8] = self.memory[sp - 8..sp].try_into().unwrap();

                    self.registers[self.get_reg() as usize] = i64::from_be_bytes(bytes);
                    self.registers[SP_REG] -= 8;
                }

                OpCode::Aloc => {
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

    fn get_word(&mut self) -> usize {
        let pc = self.pc as usize;
        let mem = usize::from_be_bytes([
            self.program[pc],
            self.program[pc + 1],
            self.program[pc + 2],
            self.program[pc + 3],
            self.program[pc + 4],
            self.program[pc + 5],
            self.program[pc + 6],
            self.program[pc + 7],
        ]);
        self.pc += 8;
        mem
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
        vm.program = vec![OpCode::Load as u8, dest_reg, src_reg, OpCode::Hlt as u8];
        vm.run();

        assert_eq!(vm.registers[dest_reg as usize], value);
    }

    #[test]
    fn loadimd_op_code() {
        let mut vm = VM::new();
        vm.program = vec![OpCode::LoadImd as u8, 0, 1, 244, OpCode::Hlt as u8];
        vm.run();

        assert_eq!(vm.registers[0], 500);
    }

    #[test]
    fn load_word_op_code() {
        let mut vm = VM::new();
        vm.program = vec![
            OpCode::LoadWord as u8,
            0,
            0,
            0,
            0,
            0,
            0,
            0,
            1,
            244,
            OpCode::Hlt as u8,
        ];
        vm.run();

        assert_eq!(vm.registers[0], 500);
    }

    #[test]
    fn load_mem_byte_op_code() {
        let mut vm = VM::new();
        vm.program = vec![OpCode::LoadMemByte as u8, 0, 1, OpCode::Hlt as u8];
        vm.registers[1] = 4;
        vm.memory[4] = 244;
        vm.run();

        assert_eq!(vm.registers[0], 244);
    }

    #[test]
    fn load_mem_quater_word_op_code() {
        let mut vm = VM::new();
        vm.program = vec![OpCode::LoadMemQuaterWord as u8, 0, 1, OpCode::Hlt as u8];
        vm.registers[1] = 4;
        vm.memory[4] = 1;
        vm.memory[5] = 244;
        vm.run();

        assert_eq!(vm.registers[0], 500);
    }

    #[test]
    fn load_mem_hafl_word_op_code() {
        let mut vm = VM::new();
        vm.program = vec![OpCode::LoadMemHalfWord as u8, 0, 1, OpCode::Hlt as u8];
        vm.registers[1] = 4;
        vm.memory[4] = 0;
        vm.memory[5] = 0;
        vm.memory[6] = 1;
        vm.memory[7] = 244;
        vm.run();

        assert_eq!(vm.registers[0], 500);
    }

    #[test]
    fn load_mem_word_op_code() {
        let mut vm = VM::new();
        vm.program = vec![OpCode::LoadMemWord as u8, 0, 1, OpCode::Hlt as u8];
        vm.registers[1] = 4;
        vm.memory[4] = 0;
        vm.memory[5] = 0;
        vm.memory[6] = 0;
        vm.memory[7] = 0;
        vm.memory[8] = 0;
        vm.memory[9] = 0;
        vm.memory[10] = 1;
        vm.memory[11] = 244;
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
            OpCode::LoadImd as u8,
            left_reg,
            0,
            2,
            OpCode::LoadImd as u8,
            right_reg,
            0,
            4,
            OpCode::Add as u8,
            dest_reg,
            left_reg,
            right_reg,
            OpCode::Hlt as u8,
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
            OpCode::LoadImd as u8,
            left_reg,
            0,
            2,
            OpCode::AddImd as u8,
            dest_reg,
            left_reg,
            1, // Imediate 500 value
            244,
            OpCode::Hlt as u8,
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
            OpCode::LoadImd as u8,
            left_reg,
            0,
            10,
            OpCode::LoadImd as u8,
            right_reg,
            0,
            4,
            OpCode::Sub as u8,
            dest_reg,
            left_reg,
            right_reg,
            OpCode::Hlt as u8,
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
            OpCode::LoadImd as u8,
            left_reg,
            0,
            2,
            OpCode::SubImd as u8,
            dest_reg,
            left_reg,
            1, // Imediate 500 value
            244,
            OpCode::Hlt as u8,
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
            OpCode::LoadImd as u8,
            left_reg,
            0,
            10,
            OpCode::LoadImd as u8,
            right_reg,
            0,
            4,
            OpCode::Mul as u8,
            dest_reg,
            left_reg,
            right_reg,
            OpCode::Hlt as u8,
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
            OpCode::LoadImd as u8,
            left_reg,
            0,
            2,
            OpCode::MulImd as u8,
            dest_reg,
            left_reg,
            1, // Imediate 500 value
            244,
            OpCode::Hlt as u8,
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
            OpCode::LoadImd as u8,
            left_reg,
            0,
            10,
            OpCode::LoadImd as u8,
            right_reg,
            0,
            3,
            OpCode::Div as u8,
            dest_reg,
            left_reg,
            right_reg,
            OpCode::Hlt as u8,
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
            OpCode::LoadImd as u8,
            left_reg,
            0,
            10,
            OpCode::DivImd as u8,
            dest_reg,
            left_reg,
            0,
            3,
            OpCode::Hlt as u8,
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
            OpCode::Jmp as u8,
            1,
            OpCode::Hlt as u8,
            OpCode::Hlt as u8,
            OpCode::Hlt as u8,
            OpCode::LoadImd as u8, // Should jump here
            0,
            0,
            10,
            OpCode::Hlt as u8,
        ];
        vm.run();

        assert_eq!(vm.registers[0], 10);
    }

    #[test]
    fn jmpf_op_code() {
        let mut vm = VM::new();
        vm.registers[1] = 3;
        vm.program = vec![
            OpCode::JmpF as u8,
            1,
            OpCode::Hlt as u8,
            OpCode::Hlt as u8,
            OpCode::Hlt as u8,
            OpCode::LoadImd as u8, // Should jump here
            0,
            0,
            10,
            OpCode::Hlt as u8,
        ];
        vm.run();

        assert_eq!(vm.registers[0], 10);
    }

    #[test]
    fn jmpfimd_op_code() {
        let mut vm = VM::new();
        vm.program = vec![
            OpCode::JmpFImd as u8,
            0,
            3,
            OpCode::Hlt as u8,
            OpCode::Hlt as u8,
            OpCode::Hlt as u8,
            OpCode::LoadImd as u8, // Should jump here
            0,
            0,
            10,
            OpCode::Hlt as u8,
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
            OpCode::LoadImd as u8, // Should jump here
            0,
            0,
            10,
            OpCode::Hlt as u8,
            OpCode::JmpB as u8, // Starts here
            1,
            OpCode::Hlt as u8,
        ];
        vm.run();

        assert_eq!(vm.registers[0], 10);
    }

    #[test]
    fn jmpbimd_op_code() {
        let mut vm = VM::new();
        vm.pc = 5;
        vm.program = vec![
            OpCode::LoadImd as u8, // Should jump here
            0,
            0,
            10,
            OpCode::Hlt as u8,
            OpCode::JmpBImd as u8, // Starts here
            0,
            8,
            OpCode::Hlt as u8,
        ];
        vm.run();

        assert_eq!(vm.registers[0], 10);
    }

    #[test]
    fn jcmp_op_code() {
        let mut vm = VM::new();
        vm.program = vec![
            OpCode::JCmp as u8,
            1,
            OpCode::Hlt as u8,
            OpCode::Hlt as u8,
            OpCode::Hlt as u8,
            OpCode::LoadImd as u8, // Should jump here
            0,
            0,
            10,
            OpCode::Hlt as u8,
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
            OpCode::JCmpF as u8,
            1,
            OpCode::Hlt as u8,
            OpCode::Hlt as u8,
            OpCode::Hlt as u8,
            OpCode::LoadImd as u8, // Should jump here
            0,
            0,
            10,
            OpCode::Hlt as u8,
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
            OpCode::JCmpFImd as u8,
            0,
            3,
            OpCode::Hlt as u8,
            OpCode::Hlt as u8,
            OpCode::Hlt as u8,
            OpCode::LoadImd as u8, // Should jump here
            0,
            0,
            10,
            OpCode::Hlt as u8,
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
            OpCode::LoadImd as u8, // Should jump here
            0,
            0,
            10,
            OpCode::Hlt as u8,
            OpCode::JCmpB as u8, // Starts here
            1,
            OpCode::Hlt as u8,
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
            OpCode::LoadImd as u8, // Should jump here
            0,
            0,
            10,
            OpCode::Hlt as u8,
            OpCode::JCmpBImd as u8, // Starts here
            0,
            8,
            OpCode::Hlt as u8,
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
        vm.program = vec![OpCode::Eq as u8, 0, 1, OpCode::Hlt as u8];

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
        vm.program = vec![OpCode::EqImd as u8, 0, 0, 2, OpCode::Hlt as u8];

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
        vm.program = vec![OpCode::NEq as u8, 0, 1, OpCode::Hlt as u8];

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
        vm.program = vec![OpCode::NEqImd as u8, 0, 0, 3, OpCode::Hlt as u8];

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
        vm.program = vec![OpCode::Gt as u8, 0, 1, OpCode::Hlt as u8];

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
        vm.program = vec![OpCode::GtImd as u8, 0, 0, 2, OpCode::Hlt as u8];

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
        vm.program = vec![OpCode::Lt as u8, 0, 1, OpCode::Hlt as u8];

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
        vm.program = vec![OpCode::LtImd as u8, 0, 0, 3, OpCode::Hlt as u8];

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
        vm.program = vec![OpCode::Ge as u8, 0, 1, OpCode::Hlt as u8];

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
        vm.program = vec![OpCode::GeImd as u8, 0, 0, 3, OpCode::Hlt as u8];

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
        vm.program = vec![OpCode::Le as u8, 0, 1, OpCode::Hlt as u8];

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
        vm.program = vec![OpCode::LeiImd as u8, 0, 0, 3, OpCode::Hlt as u8];

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
            OpCode::Push as u8,
            0,
            OpCode::Push as u8,
            1,
            OpCode::Hlt as u8,
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
            OpCode::Push as u8,
            0,
            OpCode::Push as u8,
            1,
            OpCode::Pop as u8,
            2,
            OpCode::Pop as u8,
            3,
            OpCode::Hlt as u8,
        ];
        vm.run();

        assert_eq!(vm.registers[2], 255);
        assert_eq!(vm.registers[3], 1078);
        assert_eq!(vm.registers[SP_REG], 0);
    }

    #[test]
    fn aloc_op_code() {
        let mut vm = VM::new();
        vm.program = vec![OpCode::Aloc as u8, 0, OpCode::Hlt as u8];

        vm.registers[0] = 128;
        vm.run();
        assert_eq!(vm.memory.len() - 128 - 1, vm.hp);
    }
}
