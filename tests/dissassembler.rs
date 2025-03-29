use perfaware::generate_assembly;

fn read_asm_file_just_instructions(fp: &str) -> String {
    let contents = std::fs::read_to_string(fp).unwrap();
    contents.lines().skip(18).collect::<Vec<_>>().join("\n")
}

#[test]
fn test_first_listing() {
    let input_file = "tests/data/listing_0037_single_register_mov";
    let contents = std::fs::read(input_file).unwrap();
    let output = generate_assembly(&contents);
    let expected_output = "mov cx, bx".to_string();
    assert_eq!(output, expected_output);
}
#[test]
fn test_second_listing() {
    let input_file = "tests/data/listing_0038_many_register_mov";
    let contents = std::fs::read(input_file).unwrap();
    let output = generate_assembly(&contents);
    let expected_output = read_asm_file_just_instructions(&(input_file.to_string() + ".asm"));
    assert_eq!(output, expected_output);
}
