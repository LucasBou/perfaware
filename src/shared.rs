#[derive(Debug, PartialEq, Hash, Eq, Clone)]
pub enum Register {
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
    pub fn to_asm_label(&self) -> String {
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

#[derive(Debug, PartialEq, Hash, Eq, Clone)]
pub enum RegisterPart {
    High,
    Low,
    All,
}

impl RegisterPart {
    pub fn to_asm_repr(&self) -> String {
        match self {
            RegisterPart::High => "h",
            RegisterPart::Low => "l",
            RegisterPart::All => "x",
        }
        .to_string()
    }
}

#[derive(Debug, PartialEq)]
pub enum Operand {
    Register(Register),
    Immediate(Immediate),
    EffAddCalculation(EffAddCalculation),
    EffAddCalculationWithDisplacement(EffAddCalculation, Displacement),
}

#[derive(Debug, PartialEq)]
pub enum Displacement {
    EightBit(u8),
    SixteenBit(u16),
}

impl Displacement {
    fn to_asm_label(&self) -> String {
        match self {
            Displacement::EightBit(n) => format!("{n}"),
            Displacement::SixteenBit(n) => format!("{n}"),
        }
    }

    fn is_zero(&self) -> bool {
        matches!(
            self,
            Displacement::EightBit(0) | Displacement::SixteenBit(0)
        )
    }
}

impl Operand {
    #[allow(clippy::to_string_in_format_args)]
    fn to_asm_label(&self) -> String {
        match self {
            Operand::Register(register) => register.to_asm_label(),
            Operand::Immediate(immediate) => immediate.to_asm_label(),
            Operand::EffAddCalculation(calc) => calc.to_asm_label(),
            Operand::EffAddCalculationWithDisplacement(eff_add_calculation, displacement) => {
                if displacement.is_zero() {
                    eff_add_calculation.to_asm_label()
                } else {
                    format!(
                        "{} + {}]",
                        eff_add_calculation.to_asm_label()
                            [..eff_add_calculation.to_asm_label().len() - 1]
                            .to_string(),
                        displacement.to_asm_label()
                    )
                }
            }
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum Immediate {
    EightBit(u8),
    SixteenBit(u16),
}

impl Immediate {
    fn to_asm_label(&self) -> String {
        match self {
            Immediate::EightBit(value) => format!("{}", value),
            Immediate::SixteenBit(value) => format!("{}", value),
        }
    }
}

#[derive(Debug, PartialEq)]
pub enum EffAddCalculation {
    BxSi,
    BxDi,
    BpSi,
    BpDi,
    Si,
    Di,
    Bp,
    Bx,
}

impl EffAddCalculation {
    fn to_asm_label(&self) -> String {
        let addends = match self {
            EffAddCalculation::BxSi => "bx + si",
            EffAddCalculation::BxDi => "bx + di",
            EffAddCalculation::BpSi => "bp + si",
            EffAddCalculation::BpDi => "bp + di",
            EffAddCalculation::Si => "si",
            EffAddCalculation::Di => "di",
            EffAddCalculation::Bp => "bp",
            EffAddCalculation::Bx => "bx",
        };
        format!("[{}]", addends)
    }
}
#[derive(Debug, PartialEq)]
pub enum Instruction {
    Mov(Operand, Operand),
    Add(Operand, Operand),
}

impl Instruction {
    pub fn to_asm(&self) -> String {
        match self {
            Instruction::Mov(left_operand, right_operand) => format!(
                "mov {}, {}",
                left_operand.to_asm_label(),
                right_operand.to_asm_label()
            ),
            Instruction::Add(left_operand, right_operand) => format!(
                "add {},{}",
                left_operand.to_asm_label(),
                right_operand.to_asm_label()
            ),
        }
    }
}
