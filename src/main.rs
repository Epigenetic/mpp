/*
 * Copyright (c) 2021, Jonah Shafran.
 *
 * SPDX-License-Identifier: GPL-3.0-only
 */

use crate::lexer::print_tokenize_error;
use crate::parser::{print_parse_error, VariableDefinition};
use crate::runtime::print_program;
use crate::runtime::vm::VM;
use crate::Flags::{ExecutionOutput, File, LexerOutput, ParserOutput, PrintProgram, Test};
use std::collections::HashMap;
use std::error::Error;
use std::{env, io};

mod lexer;
mod parser;
mod runtime;

fn main() {
    let args: Vec<String> = env::args().collect();
    let flags = parse_args(&args);

    if flags.len() > 0 {
        if let Test(index) = flags[0] {
            execute_line(args[index].clone(), &flags)
        } else if let File(index) = flags[0] {
            match execute_file(args[index].clone(), &flags) {
                Ok(_) => {}
                Err(error) => {
                    println!("Error reading from file: {}", error)
                }
            }
        } else {
            loop {
                let mut input = String::new();
                match io::stdin().read_line(&mut input) {
                    Ok(_) => {
                        if input.eq("h\r\n") || input.eq("h\n") {
                            break;
                        }
                        execute_line(input, &flags)
                    }
                    Err(err) => println!("{}", err),
                }
            }
        }
    } else {
        loop {
            let mut input = String::new();
            match io::stdin().read_line(&mut input) {
                Ok(_) => {
                    if input.eq("h\r\n") || input.eq("h\n") {
                        break;
                    }
                    execute_line(input, &flags)
                }
                Err(err) => println!("{}", err),
            }
        }
    }
}

fn execute_line(input: String, flags: &Vec<Flags>) {
    let input_copy = input.clone();
    let mut tokenizer = lexer::Tokenizer::new(input);
    let tokens = tokenizer.tokenize();

    match tokens {
        Ok(tokens) => {
            if flags.contains(&LexerOutput) {
                for token in &tokens {
                    println!("{}", token);
                }
            }

            let parser = parser::Parser::new(tokens);
            match parser.parse() {
                Err(error) => print_parse_error(input_copy, error),
                Ok(parse_tree) => match parse_tree {
                    None => (),
                    Some(root) => {
                        if flags.contains(&ParserOutput) {
                            parser::print_parse_tree(&root);
                        }
                        let mut program: Vec<u8> = Vec::new();
                        let mut variable_map = HashMap::<String, VariableDefinition>::new();
                        root.to_bytes(&mut program, &mut variable_map);
                        if flags.contains(&PrintProgram) {
                            println!("{:?}", program);
                            print_program(&program);
                        }

                        let mut vm = VM::new(program, flags.contains(&ExecutionOutput));
                        vm.execute();
                    }
                },
            }
        }
        Err(error) => {
            print_tokenize_error(error, &input_copy);
            return;
        }
    }
}

fn execute_file(input: String, flags: &Vec<Flags>) -> Result<(), Box<dyn Error>> {
    let program: String = std::fs::read_to_string(input)?.parse()?;
    let program_copy = program.clone();
    let mut tokenizer = lexer::Tokenizer::new(program);
    let tokens = tokenizer.tokenize();

    match tokens {
        Ok(tokens) => {
            if flags.contains(&LexerOutput) {
                for token in &tokens {
                    println!("{}", token);
                }
            }

            let parser = parser::Parser::new(tokens);
            match parser.parse() {
                Err(error) => print_parse_error(program_copy, error),
                Ok(parse_tree) => match parse_tree {
                    None => (),
                    Some(root) => {
                        if flags.contains(&ParserOutput) {
                            parser::print_parse_tree(&root);
                        }
                        let mut program: Vec<u8> = Vec::new();
                        let mut variable_map = HashMap::<String, VariableDefinition>::new();
                        root.to_bytes(&mut program, &mut variable_map);
                        if flags.contains(&PrintProgram) {
                            println!("{:?}", program);
                            print_program(&program);
                        }

                        let mut vm = VM::new(program, flags.contains(&ExecutionOutput));
                        vm.execute();
                    }
                },
            }
        }
        Err(error) => {
            print_tokenize_error(error, &program_copy);
            return Ok(());
        }
    }
    return Ok(());
}

fn parse_args<'a>(args: &Vec<String>) -> Vec<Flags> {
    let mut flags: Vec<Flags> = Vec::new();
    let mut i = 0;

    let mut has_run = false;
    let mut has_file = false;

    while i < args[1..].len() {
        match &args[i + 1][0..] {
            "--lexer" | "-l" => flags.push(LexerOutput),
            "--parser" | "-p" => flags.push(ParserOutput),
            "--program" | "-P" => flags.push(PrintProgram),
            "--execution" | "-e" => flags.push(ExecutionOutput),
            "--run" | "-r" => {
                flags.push(Test(i + 2));
                i += 1;
                has_run = true;
            }
            "--file" | "-f" => {
                flags.push(File(i + 2));
                i += 1;
                has_file = true;
            }
            _ => panic!("Unrecognized argument {}", args[i]),
        }
        i += 1
    }

    if has_run && has_file {
        panic!("Cannot specify file to run and line to execute")
    }

    return flags;
}

#[derive(PartialEq, Debug)]
enum Flags {
    LexerOutput,
    ParserOutput,
    PrintProgram,
    ExecutionOutput,
    Test(usize),
    File(usize),
}
