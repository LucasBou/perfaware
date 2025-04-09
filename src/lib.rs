#![allow(dead_code)]
use std::ops::BitAnd;

enum InParsingInstruction {
    Start,
    MovMemReg1(bool, bool),
    MovRegReg2(bool, bool, u8, u8, u8),
    MovRegReg3(bool, bool, u8, u8, u8, u8),
    MovRegReg4(bool, bool, u8, u8, u8, u8, u8),
}

struct InstructionParser {
    state: InParsingInstruction,
}

impl InstructionParser {
    fn new() -> Self {
        InstructionParser {
            state: InParsingInstruction::Start,
        }
    }

    fn parse(&mut self, byte: u8) -> Option<Instruction> {
        match self.state {
            InParsingInstruction::Start => {
                if byte.bitand(0b11111100) == 0b10001000 {
                    self.state = InParsingInstruction::MovMemReg1(
                        (byte.bitand(2) >> 1) == 1,
                        byte.bitand(1) == 1,
                    );
                    None
                } else {
                    None
                }
            }
            InParsingInstruction::MovMemReg1(d_flag, w_flag) => {
                let mod_field = byte.bitand(0b11000000) >> 6;
                let reg_field = byte.bitand(0b00111000) >> 3;
                let rm_field = byte.bitand(0b00000111);

                if (mod_field == 0b11) | ((mod_field == 0b00) & (rm_field == 0b110)) {
                    self.state = InParsingInstruction::Start;
                    let (dest_reg_field, from_reg_field) = if d_flag {
                        (reg_field, rm_field)
                    } else {
                        (rm_field, reg_field)
                    };
                    return Some(Instruction::Mov(
                        reg_field_to_reg(dest_reg_field, w_flag),
                        reg_field_to_reg(from_reg_field, w_flag),
                    ));
                }

                self.state = InParsingInstruction::MovRegReg2(
                    d_flag, w_flag, mod_field, reg_field, rm_field,
                );
                None
            }
            InParsingInstruction::MovRegReg2(d_flag, w_flag, mod_field, reg_field, rm_field) => {
                unreachable!()
            }
            InParsingInstruction::MovRegReg3(
                d_flag,
                w_flag,
                mod_field,
                reg_field,
                rm_field,
                disp_low,
            ) => {
                unreachable!()
            }
            InParsingInstruction::MovRegReg4(
                d_flag,
                w_flag,
                mod_field,
                reg_field,
                rm_field,
                disp_low,
                disp_high,
            ) => {
                unreachable!()
            }
        }
    }
}

#[derive(Debug, PartialEq)]
enum Instruction {
    Mov(Register, Register),
}

impl Instruction {
    fn to_asm(&self) -> String {
        match self {
            Instruction::Mov(dest_register, from_register) => format!(
                "mov {}, {}",
                dest_register.to_asm_label(),
                from_register.to_asm_label()
            ),
        }
    }
}

#[derive(Debug, PartialEq)]
enum Register {
    A(RegisterPart),
    B(RegisterPart),
    BI,
    BP,
    C(RegisterPart),
    D(RegisterPart),
    DI,
    SI,
    SP,
}

impl Register {
    fn to_asm_label(&self) -> String {
        match self {
            Register::A(register_part) => format!("a{}", register_part.to_asm_repr()),
            Register::B(register_part) => format!("b{}", register_part.to_asm_repr()),
            Register::BI => "bi".to_string(),
            Register::BP => "bp".to_string(),
            Register::C(register_part) => format!("c{}", register_part.to_asm_repr()),
            Register::D(register_part) => format!("d{}", register_part.to_asm_repr()),
            Register::DI => "di".to_string(),
            Register::SI => "si".to_string(),
            Register::SP => "sp".to_string(),
        }
    }
}

#[derive(Debug, PartialEq)]
enum RegisterPart {
    High,
    Low,
    All,
}

impl RegisterPart {
    fn to_asm_repr(&self) -> String {
        match self {
            RegisterPart::High => "h",
            RegisterPart::Low => "l",
            RegisterPart::All => "x",
        }
        .to_string()
    }
}

fn dissassemble(machine_code: &[u8]) -> Vec<Instruction> {
    let mut parser = InstructionParser::new();
    machine_code
        .iter()
        .filter_map(|byte| parser.parse(*byte))
        .collect()
}

pub fn generate_assembly(machine_code: &[u8]) -> String {
    dissassemble(machine_code)
        .iter()
        .map(|instr| instr.to_asm())
        .collect::<Vec<_>>()
        .join("\n")
}

fn reg_field_to_reg(regfield: u8, w_flag: bool) -> Register {
    match regfield {
        0 => {
            if !w_flag {
                Register::A(RegisterPart::Low)
            } else {
                Register::A(RegisterPart::All)
            }
        }
        1 => {
            if !w_flag {
                Register::C(RegisterPart::Low)
            } else {
                Register::C(RegisterPart::All)
            }
        }
        2 => {
            if !w_flag {
                Register::D(RegisterPart::Low)
            } else {
                Register::D(RegisterPart::All)
            }
        }
        3 => {
            if !w_flag {
                Register::B(RegisterPart::Low)
            } else {
                Register::B(RegisterPart::All)
            }
        }
        4 => {
            if !w_flag {
                Register::A(RegisterPart::High)
            } else {
                Register::SP
            }
        }
        5 => {
            if !w_flag {
                Register::C(RegisterPart::High)
            } else {
                Register::BP
            }
        }
        6 => {
            if !w_flag {
                Register::D(RegisterPart::High)
            } else {
                Register::SI
            }
        }
        7 => {
            if !w_flag {
                Register::B(RegisterPart::High)
            } else {
                Register::DI
            }
        }
        _ => unreachable!(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_asm_serialisation_basic() {
        let instruction = Instruction::Mov(
            Register::C(RegisterPart::All),
            Register::B(RegisterPart::All),
        );
        assert_eq!(instruction.to_asm(), "mov cx, bx".to_string());
    }

    #[test]
    fn test_instruction_parser_basic() {
        let machine_code = &[0b10001011_u8, 0b11001011_u8];
        assert_eq!(
            dissassemble(machine_code)[0],
            Instruction::Mov(
                Register::C(RegisterPart::All),
                Register::B(RegisterPart::All),
            )
        );
    }
}
