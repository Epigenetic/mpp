/*
 * Copyright (c) 2021, Jonah Shafran.
 *
 * SPDX-License-Identifier: GPL-3.0-only
 */

use crate::runtime::print_program;
use std::io;

mod lexer;
mod parser;
mod runtime;

fn main() {
    loop {
        let mut input = String::new();
        match io::stdin().read_line(&mut input) {
            Ok(_n) => {
                if input.eq("h\r\n") || input.eq("h\n") {
                    break;
                }
                let mut tokenizer = lexer::Tokenizer::new(input);
                let tokens = tokenizer.tokenize();
                for token in &tokens {
                    println!("{}", token);
                }

                let parser = parser::Parser::new(tokens);
                let parse_tree = parser.parse();
                match parse_tree {
                    None => println!("Parse unsuccessful"),
                    Some(root) => {
                        parser::print_parse_tree(&root);
                        let mut program: Vec<u8> = Vec::new();
                        root.to_bytes(&mut program);
                        print_program(&program);
                    }
                }
            }
            Err(err) => println!("{}", err),
        }
    }
}
