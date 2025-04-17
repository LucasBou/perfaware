#![allow(dead_code)]
use std::ops::BitAnd;

#[derive(Debug)]
enum InParsingInstruction {
    Start,
    MovMemReg1(bool, bool),
    MovRegReg2(bool, bool, u8, u8, u8),
    MovRegReg3(bool, bool, u8, u8, u8, u8),
    MovImmtoReg1(bool, u8),
    MovImmtoReg2(bool, u8, u8),
    AddRMwReg1(bool, bool),
    AddRMwReg2(bool, bool, u8, u8, u8),
    AddRMwReg3(bool, bool, u8, u8, u8, u8),
    AddImmediateToRM1(bool, bool),
    AddImmediateToRM2(bool, bool, u8, u8, u8),
    AddImmediateToRM3(bool, bool, u8, u8, u8, u8),
    AddImmediateToRM4(bool, bool, u8, u8, u8, u8, u8),
    AddImmediateToRM5(bool, bool, u8, u8, u8, u8, u8, u8),
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
                } else if byte.bitand(0b11110000) == 0b10110000 {
                    self.state =
                        InParsingInstruction::MovImmtoReg1(byte.bitand(8) == 8, byte.bitand(7));
                    None
                } else if byte.bitand(0b11111100) == 0 {
                    self.state =
                        InParsingInstruction::AddRMwReg1(byte.bitand(2) == 2, byte.bitand(1) == 1);
                    None
                } else if byte.bitand(0b11111100) == 0b10000000 {
                    self.state = InParsingInstruction::AddImmediateToRM1(
                        byte.bitand(2) == 2,
                        byte.bitand(1) == 1,
                    );
                    None
                } else {
                    panic!(
                        "Unknown starting byte sequence {:08b} with state {:?}",
                        byte, self.state
                    )
                }
            }
            InParsingInstruction::MovMemReg1(d_flag, w_flag) => {
                let (mod_field, reg_field, rm_field) = mod_reg_rm_from_byte(byte);
                match operands_from_mod_reg_rm(d_flag, w_flag, mod_field, reg_field, rm_field) {
                    OperandsCalculationProcess::NoMore(left_operand, right_operand) => {
                        self.state = InParsingInstruction::Start;
                        Some(Instruction::Mov(left_operand, right_operand))
                    }
                    OperandsCalculationProcess::NeedsMore => {
                        self.state = InParsingInstruction::MovRegReg2(
                            d_flag, w_flag, mod_field, reg_field, rm_field,
                        );
                        None
                    }
                }
            }
            InParsingInstruction::MovRegReg2(d_flag, w_flag, mod_field, reg_field, rm_field) => {
                match operands_from_first_displacement_byte(
                    d_flag, w_flag, mod_field, reg_field, rm_field, byte,
                ) {
                    OperandsCalculationProcess::NoMore(left_operand, right_operand) => {
                        self.state = InParsingInstruction::Start;
                        Some(Instruction::Mov(left_operand, right_operand))
                    }
                    OperandsCalculationProcess::NeedsMore => {
                        self.state = InParsingInstruction::MovRegReg3(
                            d_flag, w_flag, mod_field, reg_field, rm_field, byte,
                        );
                        None
                    }
                }
            }
            InParsingInstruction::MovRegReg3(
                _d_flag,
                w_flag,
                _mod_field,
                reg_field,
                rm_field,
                disp_low,
            ) => {
                self.state = InParsingInstruction::Start;
                match operands_from_second_displacement_byte(
                    _d_flag, w_flag, _mod_field, reg_field, rm_field, disp_low, byte,
                ) {
                    OperandsCalculationProcess::NoMore(left_operand, right_operand) => {
                        Some(Instruction::Mov(left_operand, right_operand))
                    }
                    OperandsCalculationProcess::NeedsMore => unreachable!(),
                }
            }
            InParsingInstruction::MovImmtoReg1(w_flag, reg_field) => {
                if !w_flag {
                    self.state = InParsingInstruction::Start;
                    Some(Instruction::Mov(
                        Operand::Register(reg_field_to_reg(reg_field, w_flag)),
                        Operand::Immediate(Immediate::EightBit(byte)),
                    ))
                } else {
                    self.state = InParsingInstruction::MovImmtoReg2(w_flag, reg_field, byte);
                    None
                }
            }
            InParsingInstruction::MovImmtoReg2(w_flag, reg_field, low_byte) => {
                self.state = InParsingInstruction::Start;
                Some(Instruction::Mov(
                    Operand::Register(reg_field_to_reg(reg_field, w_flag)),
                    Operand::Immediate(Immediate::SixteenBit(u16_from_two_switched_u8(
                        low_byte, byte,
                    ))),
                ))
            }
            InParsingInstruction::AddRMwReg1(d_flag, w_flag) => {
                let (mod_field, reg_field, rm_field) = mod_reg_rm_from_byte(byte);
                match operands_from_mod_reg_rm(d_flag, w_flag, mod_field, reg_field, rm_field) {
                    OperandsCalculationProcess::NoMore(left_operand, right_operand) => {
                        self.state = InParsingInstruction::Start;
                        Some(Instruction::Add(left_operand, right_operand))
                    }
                    OperandsCalculationProcess::NeedsMore => {
                        self.state = InParsingInstruction::AddRMwReg2(
                            d_flag, w_flag, mod_field, reg_field, rm_field,
                        );
                        None
                    }
                }
            }
            InParsingInstruction::AddRMwReg2(d_flag, w_flag, mod_field, reg_field, rm_field) => {
                match operands_from_first_displacement_byte(
                    d_flag, w_flag, mod_field, reg_field, rm_field, byte,
                ) {
                    OperandsCalculationProcess::NoMore(left_operand, right_operand) => {
                        self.state = InParsingInstruction::Start;
                        Some(Instruction::Add(left_operand, right_operand))
                    }
                    OperandsCalculationProcess::NeedsMore => {
                        self.state = InParsingInstruction::AddRMwReg3(
                            d_flag, w_flag, mod_field, reg_field, rm_field, byte,
                        );
                        None
                    }
                }
            }
            InParsingInstruction::AddRMwReg3(
                _d_flag,
                w_flag,
                _mod_field,
                reg_field,
                rm_field,
                disp_low,
            ) => {
                self.state = InParsingInstruction::Start;
                match operands_from_second_displacement_byte(
                    _d_flag, w_flag, _mod_field, reg_field, rm_field, disp_low, byte,
                ) {
                    OperandsCalculationProcess::NoMore(left_operand, right_operand) => {
                        Some(Instruction::Add(left_operand, right_operand))
                    }
                    OperandsCalculationProcess::NeedsMore => unreachable!(),
                }
            }
            InParsingInstruction::AddImmediateToRM1(s_flag, w_flag) => {
                let (mod_field, reg_field, rm_field) = mod_reg_rm_from_byte(byte);
                assert_eq!(reg_field, 0);

                if mod_field == 0 {
                    self.state = InParsingInstruction::AddImmediateToRM4(
                        s_flag, w_flag, mod_field, reg_field, rm_field, 0, 0,
                    );
                    return None;
                }

                self.state = InParsingInstruction::AddImmediateToRM2(
                    s_flag, w_flag, mod_field, reg_field, rm_field,
                );
                None
            }
            InParsingInstruction::AddImmediateToRM2(
                s_flag,
                w_flag,
                mod_field,
                reg_field,
                rm_field,
            ) => {
                self.state = InParsingInstruction::AddImmediateToRM3(
                    s_flag, w_flag, mod_field, reg_field, rm_field, byte,
                );
                None
            }
            InParsingInstruction::AddImmediateToRM3(
                s_flag,
                w_flag,
                mod_field,
                reg_field,
                rm_field,
                disp_low,
            ) => {
                self.state = InParsingInstruction::AddImmediateToRM4(
                    s_flag, w_flag, mod_field, reg_field, rm_field, disp_low, byte,
                );
                None
            }
            InParsingInstruction::AddImmediateToRM4(
                s_flag,
                w_flag,
                mod_field,
                reg_field,
                rm_field,
                disp_low,
                disp_high,
            ) => {
                if (!s_flag) & w_flag {
                    self.state = InParsingInstruction::AddImmediateToRM5(
                        s_flag, w_flag, mod_field, reg_field, rm_field, disp_low, disp_high, byte,
                    );
                    None
                } else {
                    self.state = InParsingInstruction::Start;
                    Some(Instruction::Add(
                        Operand::EffAddCalculationWithDisplacement(
                            rm_field_to_effective_address_calculation(rm_field),
                            Displacement::SixteenBit(u16_from_two_switched_u8(disp_low, disp_high)), //* not sure about that one, the byte/word differnce might be for one side only */
                        ),
                        Operand::Immediate(Immediate::EightBit(byte)),
                    ))
                }
            }
            InParsingInstruction::AddImmediateToRM5(
                _s_flag,
                _w_flag,
                _mod_field,
                _reg_field,
                rm_field,
                disp_low,
                disp_high,
                data_low,
            ) => {
                self.state = InParsingInstruction::Start;
                Some(Instruction::Add(
                    Operand::EffAddCalculationWithDisplacement(
                        rm_field_to_effective_address_calculation(rm_field),
                        Displacement::SixteenBit(u16_from_two_switched_u8(disp_low, disp_high)),
                    ),
                    Operand::Immediate(Immediate::SixteenBit(u16_from_two_switched_u8(
                        data_low, byte,
                    ))),
                ))
            }
        }
    }
}

enum OperandsCalculationProcess {
    NoMore(Operand, Operand),
    NeedsMore,
}

fn operands_from_mod_reg_rm(
    d_flag: bool,
    w_flag: bool,
    mod_field: u8,
    reg_field: u8,
    rm_field: u8,
) -> OperandsCalculationProcess {
    if (mod_field == 0b11) | ((mod_field == 0b00) & (rm_field == 0b110)) {
        let (dest_reg_field, from_reg_field) = if d_flag {
            (reg_field, rm_field)
        } else {
            (rm_field, reg_field)
        };
        return OperandsCalculationProcess::NoMore(
            Operand::Register(reg_field_to_reg(dest_reg_field, w_flag)),
            Operand::Register(reg_field_to_reg(from_reg_field, w_flag)),
        );
    }

    if mod_field == 0 {
        let eff_add_calc = rm_field_to_effective_address_calculation(rm_field);
        let register = reg_field_to_reg(reg_field, w_flag);
        let (left_operand, right_operand) = if !d_flag {
            (
                Operand::EffAddCalculation(eff_add_calc),
                Operand::Register(register),
            )
        } else {
            (
                Operand::Register(register),
                Operand::EffAddCalculation(eff_add_calc),
            )
        };
        return OperandsCalculationProcess::NoMore(left_operand, right_operand);
    }

    OperandsCalculationProcess::NeedsMore
}

fn operands_from_first_displacement_byte(
    d_flag: bool,
    w_flag: bool,
    mod_field: u8,
    reg_field: u8,
    rm_field: u8,
    disp_low: u8,
) -> OperandsCalculationProcess {
    if mod_field == 1 {
        let register = Operand::Register(reg_field_to_reg(reg_field, w_flag));
        let eff_add_calculation = Operand::EffAddCalculationWithDisplacement(
            rm_field_to_effective_address_calculation(rm_field),
            Displacement::EightBit(disp_low),
        );
        let (dest, source) = if d_flag {
            (register, eff_add_calculation)
        } else {
            (eff_add_calculation, register)
        };
        return OperandsCalculationProcess::NoMore(dest, source);
    }

    if mod_field == 2 {
        return OperandsCalculationProcess::NeedsMore;
    }
    unreachable!()
}

fn operands_from_second_displacement_byte(
    _d_flag: bool,
    w_flag: bool,
    _mod_field: u8,
    reg_field: u8,
    rm_field: u8,
    disp_low: u8,
    disp_high: u8,
) -> OperandsCalculationProcess {
    OperandsCalculationProcess::NoMore(
        Operand::Register(reg_field_to_reg(reg_field, w_flag)),
        Operand::EffAddCalculationWithDisplacement(
            rm_field_to_effective_address_calculation(rm_field),
            Displacement::SixteenBit(u16_from_two_switched_u8(disp_low, disp_high)),
        ),
    )
}

fn mod_reg_rm_from_byte(byte: u8) -> (u8, u8, u8) {
    let mod_field = byte.bitand(0b11000000) >> 6;
    let reg_field = byte.bitand(0b00111000) >> 3;
    let rm_field = byte.bitand(0b00000111);
    (mod_field, reg_field, rm_field)
}

fn u16_from_two_switched_u8(low_byte: u8, high_byte: u8) -> u16 {
    ((high_byte as u16) << 8) | (low_byte as u16)
}

#[derive(Debug, PartialEq)]
enum Operand {
    Register(Register),
    Immediate(Immediate),
    EffAddCalculation(EffAddCalculation),
    EffAddCalculationWithDisplacement(EffAddCalculation, Displacement),
}

#[derive(Debug, PartialEq)]
enum Displacement {
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
enum Immediate {
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
enum EffAddCalculation {
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

fn rm_field_to_effective_address_calculation(rm_field: u8) -> EffAddCalculation {
    match rm_field {
        0 => EffAddCalculation::BxSi,
        1 => EffAddCalculation::BxDi,
        2 => EffAddCalculation::BpSi,
        3 => EffAddCalculation::BpDi,
        4 => EffAddCalculation::Si,
        5 => EffAddCalculation::Di,
        6 => EffAddCalculation::Bp,
        7 => EffAddCalculation::Bx,
        _ => unreachable!("rm field cannot be higher than 7. Was that field parsed correctly ?"),
    }
}

#[derive(Debug, PartialEq)]
enum Instruction {
    Mov(Operand, Operand),
    Add(Operand, Operand),
}

impl Instruction {
    fn to_asm(&self) -> String {
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

    fn get_single_dissasembled_instruction(machine_code: &[u8]) -> Instruction {
        dissassemble(machine_code).into_iter().next().unwrap()
    }

    #[test]
    fn test_instruction_stringify_registers() {
        let instruction = Instruction::Mov(
            Operand::Register(Register::C(RegisterPart::All)),
            Operand::Register(Register::B(RegisterPart::All)),
        );
        assert_eq!(instruction.to_asm(), "mov cx, bx".to_string());
    }

    #[test]
    fn test_parse_register_to_register_mov() {
        let machine_code = &[0b10001011_u8, 0b11001011_u8];
        assert_eq!(
            get_single_dissasembled_instruction(machine_code),
            Instruction::Mov(
                Operand::Register(Register::C(RegisterPart::All)),
                Operand::Register(Register::B(RegisterPart::All)),
            )
        );
    }

    #[test]
    fn test_parse_special_register_to_register_mov() {
        let machine_code = &[0x89, 0xDE];
        assert_eq!(
            get_single_dissasembled_instruction(machine_code),
            Instruction::Mov(
                Operand::Register(Register::SI),
                Operand::Register(Register::B(RegisterPart::All))
            )
        )
    }

    #[test]
    fn test_parse_mov_8bit_immediate_to_register() {
        let machine_code = &[0xB1, 0x0C];
        assert_eq!(
            get_single_dissasembled_instruction(machine_code),
            Instruction::Mov(
                Operand::Register(Register::C(RegisterPart::Low)),
                Operand::Immediate(Immediate::EightBit(12))
            )
        )
    }
    #[test]
    fn test_parse_mov_16bit_immediate_to_register() {
        let machine_code = &[0xB9, 0x0C, 0x00];
        assert_eq!(
            get_single_dissasembled_instruction(machine_code),
            Instruction::Mov(
                Operand::Register(Register::C(RegisterPart::All)),
                Operand::Immediate(Immediate::SixteenBit(12))
            )
        )
    }
    #[test]
    fn test_parse_mov_source_address_calculation() {
        let machine_code = &[0x8A, 0x00];
        assert_eq!(
            get_single_dissasembled_instruction(machine_code),
            Instruction::Mov(
                Operand::Register(Register::A(RegisterPart::Low)),
                Operand::EffAddCalculation(EffAddCalculation::BxSi)
            )
        )
    }
    #[test]
    fn test_parse_mov_source_address_calculation_with_8_bit_displacement() {
        let machine_code = &[0x8A, 0x60, 0x04];
        assert_eq!(
            get_single_dissasembled_instruction(machine_code),
            Instruction::Mov(
                Operand::Register(Register::A(RegisterPart::High)),
                Operand::EffAddCalculationWithDisplacement(
                    EffAddCalculation::BxSi,
                    Displacement::EightBit(4)
                )
            )
        )
    }
    #[test]
    fn test_parse_mov_source_address_calculation_with_16_bit_displacement() {
        let machine_code = &[0x8A, 0x80, 0x87, 0x13];
        assert_eq!(
            get_single_dissasembled_instruction(machine_code),
            Instruction::Mov(
                Operand::Register(Register::A(RegisterPart::Low)),
                Operand::EffAddCalculationWithDisplacement(
                    EffAddCalculation::BxSi,
                    Displacement::SixteenBit(4999)
                )
            )
        )
    }
    #[test]
    fn test_parse_mov_destination_address_calculation() {
        let machine_code = &[0x89, 0x09];
        assert_eq!(
            get_single_dissasembled_instruction(machine_code),
            Instruction::Mov(
                Operand::EffAddCalculation(EffAddCalculation::BxDi),
                Operand::Register(Register::C(RegisterPart::All))
            )
        )
    }
    #[test]
    fn test_parse_mov_destination_address_calculation_with_bp() {
        let machine_code = &[0x88, 0x6E, 0x00];
        assert_eq!(
            get_single_dissasembled_instruction(machine_code),
            Instruction::Mov(
                Operand::EffAddCalculationWithDisplacement(
                    EffAddCalculation::Bp,
                    Displacement::EightBit(0)
                ),
                Operand::Register(Register::C(RegisterPart::High))
            )
        )
    }

    #[test]
    fn test_parse_add_with_eff_add_calc() {
        let machine_code = &[0x03, 0x18];
        assert_eq!(
            get_single_dissasembled_instruction(machine_code),
            Instruction::Add(
                Operand::Register(Register::B(RegisterPart::All)),
                Operand::EffAddCalculation(EffAddCalculation::BxSi)
            )
        )
    }
    #[test]
    fn test_parse_add_with_eff_add_calc_bp() {
        let machine_code = &[0x03, 0x5E, 0x00];
        assert_eq!(
            get_single_dissasembled_instruction(machine_code),
            Instruction::Add(
                Operand::Register(Register::B(RegisterPart::All)),
                Operand::EffAddCalculationWithDisplacement(
                    EffAddCalculation::Bp,
                    Displacement::EightBit(0)
                )
            )
        )
    }
    #[test]
    fn test_parse_last_add_before_byte_word_immediate() {
        let machine_code = &[0x01, 0x7B, 0x06];
        assert_eq!(
            get_single_dissasembled_instruction(machine_code),
            Instruction::Add(
                Operand::EffAddCalculationWithDisplacement(
                    EffAddCalculation::BpDi,
                    Displacement::EightBit(6)
                ),
                Operand::Register(Register::DI)
            )
        )
    }
    #[test]
    fn test_parse_immediate_add_byte() {
        //add byte [bx], 34
        let machine_code = &[0x80, 0x07, 0x22];
        assert_eq!(
            get_single_dissasembled_instruction(machine_code),
            Instruction::Add(
                Operand::EffAddCalculationWithDisplacement(
                    EffAddCalculation::Bx,
                    Displacement::SixteenBit(0)
                ),
                Operand::Immediate(Immediate::EightBit(34))
            )
        )
    }
    #[test]
    fn test_parse_immediate_add_byte_wide() {
        //add word [bp + si + 1000], 29
        let machine_code = &[0x83, 0x82, 0xE8, 0x03, 0x1D];
        assert_eq!(
            get_single_dissasembled_instruction(machine_code),
            Instruction::Add(
                Operand::EffAddCalculationWithDisplacement(
                    EffAddCalculation::BpSi,
                    Displacement::SixteenBit(1000)
                ),
                Operand::Immediate(Immediate::EightBit(29))
            )
        )
    }
}
