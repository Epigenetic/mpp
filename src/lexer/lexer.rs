/*
 * Copyright (c) 2021, Jonah Shafran.
 *
 * SPDX-License-Identifier: GPL-3.0-only
 */

use crate::lexer::token::ReservedToken;
use crate::lexer::{Token, TokenType};

pub struct Tokenizer {
    input: String,
    pub position: usize,
    pub line: usize,
    pub row: usize,
}

impl Tokenizer {
    pub fn new(input: String) -> Tokenizer {
        Tokenizer {
            input,
            position: 0,
            line: 0,
            row: 0,
        }
    }

    pub fn tokenize(&mut self) -> Result<Vec<Token>, TokenizeError> {
        let mut tokens = Vec::new();
        let str_array: Vec<char> = self.input.chars().collect();

        while self.position < self.input.len() {
            match str_array[self.position] {
                '+' => {
                    tokens.push(Token::new(
                        TokenType::Plus,
                        self.position,
                        self.position,
                        &self.input[self.position..self.position + 1],
                    ));
                    self.position += 1;
                    self.row += 1;
                }
                '-' => {
                    tokens.push(Token::new(
                        TokenType::Minus,
                        self.position,
                        self.position,
                        &self.input[self.position..self.position + 1],
                    ));
                    self.position += 1;
                    self.row += 1;
                }
                '*' => {
                    if self.position + 1 < self.input.len() && str_array[self.position + 1] == '*' {
                        tokens.push(Token::new(
                            TokenType::Power,
                            self.position,
                            self.position,
                            &self.input[self.position..self.position + 2],
                        ));
                        self.position += 2;
                        self.row += 2;
                    } else {
                        tokens.push(Token::new(
                            TokenType::Times,
                            self.position,
                            self.position,
                            &self.input[self.position..self.position + 1],
                        ));
                        self.position += 1;
                        self.row += 1;
                    }
                }
                '/' => {
                    tokens.push(Token::new(
                        TokenType::Divide,
                        self.position,
                        self.position,
                        &self.input[self.position..self.position + 1],
                    ));
                    self.position += 1;
                    self.row += 1;
                }
                '#' => {
                    tokens.push(Token::new(
                        TokenType::Hash,
                        self.position,
                        self.position,
                        &self.input[self.position..self.position + 1],
                    ));
                    self.position += 1;
                    self.row += 1;
                }
                '\\' => {
                    tokens.push(Token::new(
                        TokenType::IntDivide,
                        self.position,
                        self.position,
                        &self.input[self.position..self.position + 1],
                    ));
                    self.position += 1;
                    self.row += 1;
                }
                '(' => {
                    tokens.push(Token::new(
                        TokenType::LParen,
                        self.position,
                        self.position,
                        &self.input[self.position..self.position + 1],
                    ));
                    self.position += 1;
                    self.row += 1;
                }
                ')' => {
                    tokens.push(Token::new(
                        TokenType::RParen,
                        self.position,
                        self.position,
                        &self.input[self.position..self.position + 1],
                    ));
                    self.position += 1;
                    self.row += 1;
                }
                ',' => {
                    tokens.push(Token::new(
                        TokenType::Comma,
                        self.position,
                        self.position,
                        &self.input[self.position..self.position + 1],
                    ));
                    self.position += 1;
                    self.row += 1;
                }
                '!' => {
                    tokens.push(Token::new(
                        TokenType::Bang,
                        self.position,
                        self.position,
                        &self.input[self.position..self.position + 1],
                    ));
                    self.position += 1;
                    self.row += 1;
                }
                '?' => {
                    tokens.push(Token::new(
                        TokenType::QuestionMark,
                        self.position,
                        self.position,
                        &self.input[self.position..self.position + 1],
                    ));
                    self.position += 1;
                    self.row += 1
                }
                'w' | 'W' => {
                    let (token, size) = tokenize_write(&self.input[self.position..], self.position);
                    tokens.push(token);
                    self.position += size;
                    self.row += size;
                }
                '0'..='9' => {
                    let (token, size) =
                        tokenize_number(&self.input[self.position..], self.position);
                    tokens.push(token);
                    self.position += size;
                    self.row += size;
                }
                '.' => {
                    if self.position + 1 < self.input.len()
                        && str_array[self.position + 1].is_digit(10)
                    {
                        let (token, size) =
                            tokenize_number(&self.input[self.position..], self.position);
                        tokens.push(token);
                        self.position += size;
                        self.row += size;
                    } else {
                        todo!()
                    }
                }
                '"' => {
                    let (token, size) =
                        tokenize_string(&self.input[self.position..], self.position)?;
                    tokens.push(token);
                    self.position += size;
                    self.row += size;
                }
                '\n' => {
                    self.position += 1;
                    self.row = 0;
                    self.line += 1;
                }
                c if c.is_whitespace() => {
                    // Skip white space, for now
                    self.position += 1;
                    self.row += 1;
                }
                _ => {
                    return Err(TokenizeError {
                        line: self.line,
                        row: self.position,
                        message: "Unrecognized token pattern",
                    });
                }
            }
        }

        return Ok(tokens);
    }
}

pub fn print_tokenize_error(error: TokenizeError, input: &str) {
    let lines: Vec<&str> = input.lines().collect();

    eprintln!("{}:{}", error.line, error.row);
    eprintln!("{}", lines[error.line]);
    for _ in 0..error.row {
        eprint!(" ");
    }
    eprintln!("^ {}", error.message);
}

fn tokenize_number(input: &str, position: usize) -> (Token, usize) {
    let mut end = 0;
    let str_array: Vec<char> = input.chars().collect();
    let mut found_dot = false;

    while end < input.len() {
        let c = str_array[end];
        if c.is_digit(10) || (c == '.' && !found_dot) {
            if c == '.' {
                found_dot = true;
            }
            end += 1;
            continue;
        }
        break;
    }
    return (
        Token::new(
            TokenType::NumLit,
            position,
            end - 1 + position,
            &input[0..end],
        ),
        end,
    );
}

fn tokenize_string(input: &str, position: usize) -> Result<(Token, usize), TokenizeError> {
    let mut end = 1;
    let str_array: Vec<char> = input.chars().collect();
    let mut found_end = false;

    while end < input.len() && !found_end {
        let c = str_array[end];
        end += 1;
        if c == '"' {
            if end < input.len() && str_array[end] == '"' {
                end += 1;
                continue;
            }
            found_end = true;
            break;
        }
    }

    if !found_end {
        return Err(TokenizeError {
            line: 0,
            row: end,
            message: "Did not find closing quotation mark",
        });
    }

    return Ok((
        Token::new(
            TokenType::StrLit,
            position + 1,
            end - 2 + position,
            &input[1..end - 1],
        ),
        end,
    ));
}

fn tokenize_write(input: &str, position: usize) -> (Token, usize) {
    let str_array: Vec<char> = input.chars().collect();
    //One character write command (w or W)
    if str_array[1].is_whitespace() {
        return (
            Token::new(
                TokenType::Reserved(ReservedToken::Write),
                position,
                position + 1,
                &input[0..1],
            ),
            1,
        );
    }

    // Full write command
    if &input[..5] == "write" || &input[..5] == "WRITE" {
        return (
            Token::new(
                TokenType::Reserved(ReservedToken::Write),
                position + 0,
                position + 5,
                &input[..5],
            ),
            5,
        );
    }

    //Identifier
    todo!()
}

pub struct TokenizeError<'a> {
    line: usize,
    row: usize,
    message: &'a str,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_lex_plus() {
        let input = String::from("+");
        let mut tokenizer = Tokenizer::new(input);

        let tokens = tokenizer.tokenize();

        assert!(tokens.is_ok());
        if let Ok(tokens_ok) = tokens {
            assert_eq!(tokens_ok.len(), 1);
            assert_eq!(tokens_ok[0], Token::new(TokenType::Plus, 0, 0, "+"));
        }
    }

    #[test]
    fn test_lex_minus() {
        let input = String::from("-");
        let mut tokenizer = Tokenizer::new(input);

        let tokens = tokenizer.tokenize();

        assert!(tokens.is_ok());
        if let Ok(tokens_ok) = tokens {
            assert_eq!(tokens_ok.len(), 1);
            assert_eq!(tokens_ok[0], Token::new(TokenType::Minus, 0, 0, "-"));
        }
    }

    #[test]
    fn test_lex_times() {
        let input = String::from("*");
        let mut tokenizer = Tokenizer::new(input);

        let tokens = tokenizer.tokenize();

        assert!(tokens.is_ok());
        if let Ok(tokens_ok) = tokens {
            assert_eq!(tokens_ok.len(), 1);
            assert_eq!(tokens_ok[0], Token::new(TokenType::Times, 0, 0, "*"));
        }
    }

    #[test]
    fn test_lex_divide() {
        let input = String::from("/");
        let mut tokenizer = Tokenizer::new(input);
        let tokens = tokenizer.tokenize();

        assert!(tokens.is_ok());
        if let Ok(tokens_ok) = tokens {
            assert_eq!(tokens_ok.len(), 1);
            assert_eq!(tokens_ok[0], Token::new(TokenType::Divide, 0, 0, "/"));
        }
    }

    #[test]
    fn test_lex_number() {
        let input = "123";
        let (token, position) = tokenize_number(input, 0);

        assert_eq!(token, Token::new(TokenType::NumLit, 0, 2, "123",));
        assert_eq!(position, 3)
    }

    #[test]
    fn test_lex_number_mid_string() {
        let input = "baz 123.456 foobar";
        let (token, position) = tokenize_number(&input[4..], 5);

        assert_eq!(token, Token::new(TokenType::NumLit, 5, 11, "123.456",));
        assert_eq!(position, 7);
    }

    #[test]
    fn test_lex_l_paren() {
        let input = "(";
        let mut tokenizer = Tokenizer::new(input.to_string());
        let tokens = tokenizer.tokenize();

        assert!(tokens.is_ok());
        if let Ok(tokens_ok) = tokens {
            assert_eq!(tokens_ok.len(), 1);
            assert_eq!(tokens_ok[0], Token::new(TokenType::LParen, 0, 0, "("));
        }
    }

    #[test]
    fn test_lex_r_paren() {
        let input = ")";
        let mut tokenizer = Tokenizer::new(input.to_string());
        let tokens = tokenizer.tokenize();

        assert!(tokens.is_ok());
        if let Ok(tokens_ok) = tokens {
            assert_eq!(tokens_ok.len(), 1);
            assert_eq!(tokens_ok[0], Token::new(TokenType::RParen, 0, 0, ")"));
        }
    }
}
