use std::{
    fs::{self, File},
    io::Write,
    path::{Path, PathBuf},
};

use perfaware::generate_assembly;

fn read_asm_file_just_instructions(fp: &str) -> String {
    let contents = std::fs::read_to_string(fp).unwrap();
    contents.lines().skip(18).collect::<Vec<_>>().join("\n")
}

fn create_file_in_temp_dir<P: AsRef<Path>>(path: P) -> std::io::Result<(File, PathBuf)> {
    let temp_dir = Path::new("/tmp");

    let file = temp_dir.join(path);
    let handle = File::create(&file)?;
    Ok((handle, file))
}

fn assert_equal_after_dissassemble_and_reassemble_using_nasm(input_file: &str) {
    let contents = std::fs::read(input_file).unwrap();
    let output = generate_assembly(&contents);
    let output_file_name = input_file.split("/").last().unwrap().to_string() + ".asm";
    let (mut output_file_handle, output_file_path) =
        create_file_in_temp_dir(&output_file_name).unwrap();
    output_file_handle.write_all(output.as_bytes()).unwrap();
    let nasm_output = std::process::Command::new("nasm")
        .args(&[
            "-f",
            "bin",
            "-o",
            "/dev/stdout",
            output_file_path.as_os_str().to_str().unwrap(),
        ])
        .output()
        .unwrap();
    let _ = fs::remove_file(output_file_path);
    assert_eq!(nasm_output.stdout, contents)
}

#[test]
fn test_first_listing_with_nasm() {
    let input_file = "tests/data/listing_0037_single_register_mov";
    assert_equal_after_dissassemble_and_reassemble_using_nasm(input_file);
}
#[test]
fn test_second_listing_with_nasm() {
    let input_file = "tests/data/listing_0038_many_register_mov";
    assert_equal_after_dissassemble_and_reassemble_using_nasm(input_file);
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
