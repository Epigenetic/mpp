/*
 * Copyright (c) 2021, Jonah Shafran.
 *
 * SPDX-License-Identifier: GPL-3.0-only
 */

use std::env;
use std::fs::File;
use std::io::Read;
use std::path::Path;
use std::process::Command;
use std::str;

pub fn run_tests(file_name: &str) {
    let path = Path::new(file_name);
    let mut test_file = match File::open(&path) {
        Err(e) => panic!("Unable to open file {}: {}", file_name, e),
        Ok(file) => file,
    };

    let mut test_content = String::new();
    if let Err(e) = test_file.read_to_string(&mut test_content) {
        panic!("Unable to read file {}: {}", file_name, e)
    }

    for line in test_content.lines() {
        if line.len() == 0 || line.starts_with(";;") {
            continue;
        }
        execute_test(line);
    }
}

fn execute_test(test: &str) {
    let test_pieces: Vec<&str> = test.split(" ; ").collect();
    assert_eq!(
        test_pieces.len(),
        2,
        "Tests should have 2 pieces split by a ' ; '"
    );
    let test_body = test_pieces[0];
    let test_assertion = test_pieces[1];

    let mut command = Command::new(format!("./target/debug/mpp{}", env::consts::EXE_SUFFIX));
    command.arg("-r").arg(test_body.to_owned() + "\n");
    println!("Test: {}", test);
    match command.output() {
        Err(e) => panic!("Error running compiler: {}", e),
        Ok(output) => assert_eq!(
            str::from_utf8(&output.stdout).expect("unable to parse output to string"),
            test_assertion
        ),
    }
}

pub fn run_error_test(file_name: &str) {
    let path = Path::new(file_name);
    let mut test_file = match File::open(&path) {
        Err(e) => panic!("Unable to open file {}: {}", file_name, e),
        Ok(file) => file,
    };

    let mut test_content = String::new();
    if let Err(e) = test_file.read_to_string(&mut test_content) {
        panic!("Unable to read file {}: {}", file_name, e)
    }

    let lines: Vec<&str> = test_content.lines().collect();
    let mut index = 0;
    while index < lines.len() {
        if lines[index].len() == 0 || lines[index].starts_with(";;") {
            index += 1;
            continue;
        }
        let mut expected = String::from("");
        let mut offset = 1;
        while index + offset < lines.len() && lines[index + offset].starts_with(";;;") {
            if expected.len() > 0 {
                expected.push('\n');
            }
            expected.push_str(lines[index + offset].strip_prefix(";;;").unwrap());
            offset += 1;
        }
        expected.push('\n');
        execute_error_test(lines[index], expected);
        index += offset;
    }

    fn execute_error_test(test: &str, expected: String) {
        let mut test_string = String::from(test);
        test_string.push('\n');
        let mut command = Command::new(format!("./target/debug/mpp{}", env::consts::EXE_SUFFIX));
        command.arg("-r").arg(test_string);
        println!("Test: {}", test);
        match command.output() {
            Err(e) => panic!("Error running compiler: {}", e),
            Ok(output) => assert_eq!(
                str::from_utf8(&output.stderr).expect("unable to parse output to string"),
                expected
            ),
        }
    }
}
