/*
 * Copyright (c) 2021, Jonah Shafran.
 *
 * SPDX-License-Identifier: GPL-3.0-only
 */

use crate::lexer::print_tokenize_error;
use crate::runtime::print_program;
use crate::runtime::vm::VM;
use crate::Flags::{ExecutionOutput, LexerOutput, ParserOutput, PrintProgram, Test};
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
            let parse_tree = parser.parse();
            match parse_tree {
                None => (),
                Some(root) => {
                    if flags.contains(&ParserOutput) {
                        parser::print_parse_tree(&root);
                    }
                    let mut program: Vec<u8> = Vec::new();
                    root.to_bytes(&mut program);
                    if flags.contains(&PrintProgram) {
                        println!("{:?}", program);
                        print_program(&program);
                    }

                    let mut vm = VM::new(program, flags.contains(&ExecutionOutput));
                    vm.execute();
                }
            }
        }
        Err(error) => {
            print_tokenize_error(error, &input_copy);
            return;
        }
    }
}

fn parse_args<'a>(args: &Vec<String>) -> Vec<Flags> {
    let mut flags: Vec<Flags> = Vec::new();
    let mut i = 1;
    while i < args[1..].len() {
        match &args[i][0..] {
            "--lexer" | "-l" => flags.push(LexerOutput),
            "--parser" | "-p" => flags.push(ParserOutput),
            "--program" | "-P" => flags.push(PrintProgram),
            "--execution" | "-e" => flags.push(ExecutionOutput),
            "--run" | "-r" => {
                flags.push(Test(i + 1));
                i += 1
            }
            _ => panic!("Unrecognized argument {}", args[i]),
        }
        i += 1
    }

    return flags;
}

#[derive(PartialEq)]
enum Flags {
    LexerOutput,
    ParserOutput,
    PrintProgram,
    ExecutionOutput,
    Test(usize),
}
