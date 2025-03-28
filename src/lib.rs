use std::ops::BitAnd;

#[derive(Debug, PartialEq)]
enum Instruction {
    MOV(Register, Register),
}

impl Instruction {
    fn to_asm(&self) -> String {
        match self {
            Instruction::MOV(dest_register, from_register) => format!(
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
    machine_code
        .chunks(2)
        .map(parse_mov)
        .map(|o| o.unwrap())
        .collect()
}

fn parse_mov(machine_code: &[u8]) -> Option<Instruction> {
    let high_byte = machine_code[0];
    let low_byte = machine_code[1];
    let opcode = high_byte.bitand(0b11111100);
    if opcode != 0b10001000 {
        return None;
    }
    let d_flag = high_byte.bitand(0b00000010).eq(&2);
    let w_flag = high_byte.bitand(0b00000001).eq(&1);

    let mod_field = low_byte.bitand(0b11000000) >> 6;
    let reg_field = low_byte.bitand(0b00111000) >> 3;
    let rm_field = low_byte.bitand(0b00000111);

    let (dest_reg_field, from_reg_field) = if d_flag {
        (reg_field, rm_field)
    } else {
        (rm_field, reg_field)
    };

    Some(Instruction::MOV(
        reg_field_to_reg(dest_reg_field, w_flag),
        reg_field_to_reg(from_reg_field, w_flag),
    ))
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
        let instruction = Instruction::MOV(
            Register::C(RegisterPart::All),
            Register::B(RegisterPart::All),
        );
        assert_eq!(instruction.to_asm(), "mov cx, bx".to_string());
    }

    #[test]
    fn test_instruction_parser_basic() {
        let machine_code = &[0b10001011_u8, 0b00001011_u8];
        assert_eq!(
            parse_mov(machine_code),
            Some(Instruction::MOV(
                Register::C(RegisterPart::All),
                Register::B(RegisterPart::All),
            ))
        );
    }
}
