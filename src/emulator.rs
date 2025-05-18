#![allow(dead_code)]

use crate::shared::{Instruction, Register};
use std::collections::HashMap;

#[derive(Debug, PartialEq)]
struct Cpu(HashMap<Register, u32>);

impl Default for Cpu {
    fn default() -> Self {
        let hm = [Register::SI].iter().map(|r| (r.clone(), 0)).collect();
        Cpu(hm)
    }
}

impl Cpu {
    pub fn execute(&self, instr: Instruction) -> Cpu {
        let mut new_cpu = self.0.clone();
        let si = new_cpu
            .get_mut(&Register::SI)
            .expect("all registers are inside the cpu");
        *si = 1;
        Cpu(new_cpu)
    }

    pub fn get_register(&self, reg: Register) -> u32 {
        *self.0.get(&reg).expect("all registers are inside the cpu")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_mov_immediate() {
        let instruction = Instruction::Mov(
            crate::shared::Operand::Register(Register::SI),
            crate::shared::Operand::Immediate(crate::shared::Immediate::SixteenBit(1)),
        );
        let cpu = Cpu::default();
        let result = cpu.execute(instruction);
        let expected = 1;
        assert_eq!(expected, result.get_register(Register::SI));
    }
}
