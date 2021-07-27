/*
 * Copyright (c) 2021, Jonah Shafran.
 *
 * SPDX-License-Identifier: GPL-3.0-only
 */

use crate::runtime::print_program;
use crate::runtime::vm::VM;
use crate::Flags::{ExecutionOutput, LexerOutput, ParserOutput, PrintProgram};
use std::{env, io};

mod lexer;
mod parser;
mod runtime;

fn main() {
    let args: Vec<String> = env::args().collect();
    let flags = parse_args(args);

    loop {
        let mut input = String::new();
        match io::stdin().read_line(&mut input) {
            Ok(_n) => {
                if input.eq("h\r\n") || input.eq("h\n") {
                    break;
                }
                let mut tokenizer = lexer::Tokenizer::new(input);
                let tokens = tokenizer.tokenize();

                if flags.contains(&LexerOutput) {
                    for token in &tokens {
                        println!("{}", token);
                    }
                }

                let parser = parser::Parser::new(tokens);
                let parse_tree = parser.parse();
                match parse_tree {
                    None => println!("Parse unsuccessful"),
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
            Err(err) => println!("{}", err),
        }
    }
}

fn parse_args(args: Vec<String>) -> Vec<Flags> {
    let mut flags: Vec<Flags> = Vec::new();
    for arg in &args[1..] {
        match &arg[0..] {
            "--lexer" | "-l" => flags.push(LexerOutput),
            "--parser" | "-p" => flags.push(ParserOutput),
            "--program" | "-P" => flags.push(PrintProgram),
            "--execution" | "-e" => flags.push(ExecutionOutput),
            _ => panic!("Unrecognized argument {}", arg),
        }
    }

    return flags;
}

#[derive(PartialEq)]
enum Flags {
    LexerOutput,
    ParserOutput,
    PrintProgram,
    ExecutionOutput,
}
