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

enum Register {
    A(RegisterPart),
    B(RegisterPart),
    BI,
    C(RegisterPart),
    D(RegisterPart),
    DI,
    SI,
}

impl Register {
    fn to_asm_label(&self) -> String {
        match self {
            Register::A(register_part) => format!("a{}", register_part.to_asm_repr()),
            Register::B(register_part) => format!("b{}", register_part.to_asm_repr()),
            Register::BI => "bi".to_string(),
            Register::C(register_part) => format!("c{}", register_part.to_asm_repr()),
            Register::D(register_part) => format!("d{}", register_part.to_asm_repr()),
            Register::DI => "di".to_string(),
            Register::SI => "si".to_string(),
        }
    }
}

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

fn dissassemble_single_line(l: &[u8]) -> Instruction {
    todo!()
}

pub fn add(left: u64, right: u64) -> u64 {
    left + right
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
}
