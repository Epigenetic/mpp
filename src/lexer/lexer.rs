/*
 * Copyright (c) 2021, Jonah Shafran.
 *
 * SPDX-License-Identifier: GPL-3.0-only
 */

use crate::lexer::TokenType;

pub struct Tokenizer {
    input: String,
    pub position: usize,
}

impl Tokenizer {
    pub fn new(input: String) -> Tokenizer {
        Tokenizer {
            input: input,
            position: 0,
        }
    }

    pub fn tokenize(&mut self) -> Vec<TokenType> {
        let mut tokens = Vec::new();
        let str_array: Vec<char> = self.input.chars().collect();

        while self.position < self.input.len() {
            match str_array[self.position] {
                '+' => {
                    tokens.push(TokenType::Plus {
                        position: self.position,
                    });
                    self.position += 1;
                }
                '-' => {
                    tokens.push(TokenType::Minus {
                        position: self.position,
                    });
                    self.position += 1;
                }
                '0'..='9' => {
                    let result = tokenize_number(&self.input[self.position..], self.position);
                    tokens.push(result.0);
                    self.position += result.1;
                }
                c if c.is_whitespace() => (self.position += 1), // Skip white space, for now
                unknown => panic!("Unrecognized token pattern {}", unknown),
            }
        }

        return tokens;
    }
}

fn tokenize_number<'a>(input: &'a str, position: usize) -> (TokenType<'a>, usize) {
    let mut end = 0;
    let str_array: Vec<char> = input.chars().collect();

    while end < input.len() {
        let c = str_array[end];
        if c.is_digit(10) || c == '.' {
            end += 1;
            continue;
        }
        break;
    }
    return (
        TokenType::NumLit {
            value: &input[0..end],
            start: position,
            end: end - 1 + position,
        },
        end,
    );
}
