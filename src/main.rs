fn machine_code_from_instruction_using_nasm(instruction: String) -> Vec<u8> {
    let contents = format!("bits 16\n{instruction}");
    let temp_filename = "instruction_code.txt";
    let (mut output_file_handle, output_file_path) = create_file_in_temp_dir(temp_filename)
        .expect("Failed to create the temporary output file that will be used by nasm");
    std::io::Write::write_all(&mut output_file_handle, contents.as_bytes())
        .expect("Failed to write asm code into the temporary file");
    let nasm_output = std::process::Command::new("nasm")
        .args([
            "-f",
            "bin",
            "-o",
            "/dev/stdout",
            output_file_path.as_os_str().to_str().unwrap(),
        ])
        .output()
        .expect("Failed running the nasm command");
    let _ = std::fs::remove_file(output_file_path);
    nasm_output.stdout
}

fn create_file_in_temp_dir<P: AsRef<std::path::Path>>(
    path: P,
) -> std::io::Result<(std::fs::File, std::path::PathBuf)> {
    let temp_dir = std::path::Path::new("/tmp");

    let file = temp_dir.join(path);
    let handle = std::fs::File::create(&file)?;
    Ok((handle, file))
}
fn print_as_hex(bytes: &[u8]) {
    bytes.iter().for_each(|b| println!("{b:0X}"));
}
fn print_as_bin(bytes: &[u8]) {
    bytes.iter().for_each(|b| println!("{b:0b}"));
}

fn print_compiled_machine_code(asm_instruction: String) {
    let mc = machine_code_from_instruction_using_nasm(asm_instruction);
    println!("Machine code in Hex");
    print_as_hex(&mc);
    println!("Machine code in binary");
    print_as_bin(&mc);
}
fn main() {
    let args = std::env::args();
    let first_arg = args
        .into_iter()
        .nth(1)
        .expect("No arguments were passed : please provide an asm instruction.");

    println!("Given instruction was : ");
    println!("{first_arg}");

    print_compiled_machine_code(first_arg);
}
