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
        Tokenizer { input, position: 0 }
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
                '*' => {
                    tokens.push(TokenType::Times {
                        position: self.position,
                    });
                    self.position += 1;
                }
                '/' => {
                    tokens.push(TokenType::Divide {
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

fn tokenize_number(input: &str, position: usize) -> (TokenType, usize) {
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lex_plus() {
        let input = String::from("+");
        let mut tokenizer = Tokenizer::new(input);

        let tokens = tokenizer.tokenize();

        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0], TokenType::Plus { position: 0 });
    }

    #[test]
    fn test_lex_minus() {
        let input = String::from("-");
        let mut tokenizer = Tokenizer::new(input);

        let tokens = tokenizer.tokenize();

        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0], TokenType::Minus { position: 0 });
    }

    #[test]
    fn test_lex_times() {
        let input = String::from("*");
        let mut tokenizer = Tokenizer::new(input);

        let tokens = tokenizer.tokenize();

        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0], TokenType::Times { position: 0 });
    }

    #[test]
    fn test_lex_divide() {
        let input = String::from("/");
        let mut tokenizer = Tokenizer::new(input);

        let tokens = tokenizer.tokenize();

        assert_eq!(tokens.len(), 1);
        assert_eq!(tokens[0], TokenType::Divide { position: 0 });
    }

    #[test]
    fn test_lex_number() {
        let input = "123";
        let (token, position) = tokenize_number(input, 0);

        assert_eq!(
            token,
            TokenType::NumLit {
                value: "123",
                start: 0,
                end: 2
            }
        );
        assert_eq!(position, 3)
    }

    #[test]
    fn test_lex_number_mid_string() {
        let input = "baz 123.456 foobar";
        let (token, position) = tokenize_number(&input[4..], 5);

        assert_eq!(
            token,
            TokenType::NumLit {
                value: "123.456",
                start: 5,
                end: 11
            }
        );
        assert_eq!(position, 7);
    }
}
