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
                        self.row,
                        self.row + 1,
                        self.line,
                        &self.input[self.position..self.position + 1],
                    ));
                    self.position += 1;
                    self.row += 1;
                }
                '-' => {
                    tokens.push(Token::new(
                        TokenType::Minus,
                        self.row,
                        self.row + 1,
                        self.line,
                        &self.input[self.position..self.position + 1],
                    ));
                    self.position += 1;
                    self.row += 1;
                }
                '*' => {
                    if self.position + 1 < self.input.len() && str_array[self.position + 1] == '*' {
                        tokens.push(Token::new(
                            TokenType::Power,
                            self.row,
                            self.row + 2,
                            self.line,
                            &self.input[self.position..self.position + 2],
                        ));
                        self.position += 2;
                        self.row += 2;
                    } else {
                        tokens.push(Token::new(
                            TokenType::Times,
                            self.row,
                            self.row + 1,
                            self.line,
                            &self.input[self.position..self.position + 1],
                        ));
                        self.position += 1;
                        self.row += 1;
                    }
                }
                '/' => {
                    tokens.push(Token::new(
                        TokenType::Divide,
                        self.row,
                        self.row + 1,
                        self.line,
                        &self.input[self.position..self.position + 1],
                    ));
                    self.position += 1;
                    self.row += 1;
                }
                '#' => {
                    tokens.push(Token::new(
                        TokenType::Hash,
                        self.row,
                        self.row + 1,
                        self.line,
                        &self.input[self.position..self.position + 1],
                    ));
                    self.position += 1;
                    self.row += 1;
                }
                '\\' => {
                    tokens.push(Token::new(
                        TokenType::IntDivide,
                        self.row,
                        self.row + 1,
                        self.line,
                        &self.input[self.position..self.position + 1],
                    ));
                    self.position += 1;
                    self.row += 1;
                }
                '(' => {
                    tokens.push(Token::new(
                        TokenType::LParen,
                        self.row,
                        self.row + 1,
                        self.line,
                        &self.input[self.position..self.position + 1],
                    ));
                    self.position += 1;
                    self.row += 1;
                }
                ')' => {
                    tokens.push(Token::new(
                        TokenType::RParen,
                        self.row,
                        self.row + 1,
                        self.line,
                        &self.input[self.position..self.position + 1],
                    ));
                    self.position += 1;
                    self.row += 1;
                }
                ',' => {
                    tokens.push(Token::new(
                        TokenType::Comma,
                        self.row,
                        self.row + 1,
                        self.line,
                        &self.input[self.position..self.position + 1],
                    ));
                    self.position += 1;
                    self.row += 1;
                }
                '!' => {
                    tokens.push(Token::new(
                        TokenType::Bang,
                        self.row,
                        self.row + 1,
                        self.line,
                        &self.input[self.position..self.position + 1],
                    ));
                    self.position += 1;
                    self.row += 1;
                }
                '?' => {
                    tokens.push(Token::new(
                        TokenType::QuestionMark,
                        self.row,
                        self.row + 1,
                        self.line,
                        &self.input[self.position..self.position + 1],
                    ));
                    self.position += 1;
                    self.row += 1
                }
                '>' => {
                    if self.position + 1 < self.input.len() && str_array[self.position + 1] == '=' {
                        tokens.push(Token::new(
                            TokenType::GreaterThanOrEqualTo,
                            self.row,
                            self.row + 2,
                            self.line,
                            &self.input[self.position..self.position + 2],
                        ));
                        self.position += 2;
                        self.row += 2;
                    } else {
                        tokens.push(Token::new(
                            TokenType::GreaterThan,
                            self.row,
                            self.row + 1,
                            self.line,
                            &self.input[self.position..self.position + 1],
                        ));
                        self.position += 1;
                        self.row += 1;
                    }
                }
                '<' => {
                    if self.position + 1 < self.input.len() && str_array[self.position + 1] == '=' {
                        tokens.push(Token::new(
                            TokenType::LessThanOrEqualTo,
                            self.row,
                            self.row + 2,
                            self.line,
                            &self.input[self.position..self.position + 2],
                        ));
                        self.position += 2;
                        self.row += 2;
                    } else {
                        tokens.push(Token::new(
                            TokenType::LessThan,
                            self.row,
                            self.row + 1,
                            self.line,
                            &self.input[self.position..self.position + 1],
                        ));
                        self.position += 1;
                        self.row += 1;
                    }
                }
                '\'' => {
                    if str_array.len() >= self.position && str_array[self.position + 1] == '=' {
                        tokens.push(Token::new(
                            TokenType::NotEquals,
                            self.row,
                            self.row + 2,
                            self.line,
                            &self.input[self.position..self.position + 2],
                        ));
                        self.position += 2;
                        self.row += 1;
                    } else {
                        tokens.push(Token::new(
                            TokenType::Not,
                            self.row,
                            self.row + 1,
                            self.line,
                            &self.input[self.position..self.position + 1],
                        ));
                        self.position += 1;
                        self.row += 1;
                    }
                }
                '=' => {
                    tokens.push(Token::new(
                        TokenType::Equals,
                        self.row,
                        self.row + 1,
                        self.line,
                        &self.input[self.position..self.position + 1],
                    ));
                    self.position += 1;
                    self.row += 1;
                }
                ':' => {
                    tokens.push(Token::new(
                        TokenType::Colon,
                        self.row,
                        self.row + 1,
                        self.line,
                        &self.input[self.position..self.position + 1],
                    ));
                    self.position += 1;
                    self.row += 1;
                }
                '{' => {
                    tokens.push(Token::new(
                        TokenType::LCurly,
                        self.row,
                        self.row + 1,
                        self.line,
                        &self.input[self.position..self.position + 1],
                    ));
                    self.position += 1;
                    self.row += 1;
                }
                '}' => {
                    tokens.push(Token::new(
                        TokenType::RCurly,
                        self.row,
                        self.row + 1,
                        self.line,
                        &self.input[self.position..self.position + 1],
                    ));
                    self.position += 1;
                    self.row += 1;
                }
                '$' => {
                    if str_array[self.position + 1] == '$' {
                        tokens.push(Token::new(
                            TokenType::DollarDollar,
                            self.row,
                            self.row + 2,
                            self.line,
                            &self.input[self.position..self.position + 2],
                        ));
                        self.position += 2;
                        self.row += 2;
                    }
                }
                'w' | 'W' => {
                    let (token, size) =
                        tokenize_write(&self.input[self.position..], self.row, self.line)?;
                    tokens.push(token);
                    self.position += size;
                    self.row += size;
                }
                'n' | 'N' => {
                    let (token, size) =
                        tokenize_new(&self.input[self.position..], self.row, self.line)?;
                    tokens.push(token);
                    self.position += size;
                    self.row += size;
                }
                's' | 'S' => {
                    let (token, size) =
                        tokenize_set_or_string(&self.input[self.position..], self.row, self.line)?;
                    tokens.push(token);
                    self.position += size;
                    self.row += size;
                }
                'i' | 'I' => {
                    let (token, size) =
                        tokenize_if_or_int(&self.input[self.position..], self.row, self.line)?;
                    tokens.push(token);
                    self.position += size;
                    self.row += size;
                }
                'e' | 'E' => {
                    let (token, size) =
                        tokenize_else(&self.input[self.position..], self.row, self.line)?;
                    tokens.push(token);
                    self.position += size;
                    self.row += size;
                }
                'f' | 'F' => {
                    let (token, size) =
                        tokenize_for(&self.input[self.position..], self.row, self.line)?;
                    tokens.push(token);
                    self.position += size;
                    self.row += size;
                }
                'd' | 'D' => {
                    let (token, size) =
                        tokenize_do_or_double(&self.input[self.position..], self.row, self.line)?;
                    tokens.push(token);
                    self.position += size;
                    self.row += size;
                }
                '0'..='9' => {
                    let (token, size) =
                        tokenize_number(&self.input[self.position..], self.row, self.line);
                    tokens.push(token);
                    self.position += size;
                    self.row += size;
                }
                '.' => {
                    if self.position + 1 < self.input.len()
                        && str_array[self.position + 1].is_digit(10)
                    {
                        let (token, size) =
                            tokenize_number(&self.input[self.position..], self.row, self.line);
                        tokens.push(token);
                        self.position += size;
                        self.row += size;
                    } else {
                        todo!("Lex dots")
                    }
                }
                '"' => {
                    let (token, size) =
                        tokenize_string(&self.input[self.position..], self.row, self.line)?;
                    tokens.push(token);
                    self.position += size;
                    self.row += size;
                }
                c if c.is_ascii_alphabetic() || c == '%' => {
                    let (token, size) = tokenize_identifier(
                        &self.input[self.position..],
                        self.position,
                        self.line,
                    )?;
                    tokens.push(token);
                    self.position += size;
                    self.row += size;
                }
                '\n' => {
                    tokens.push(Token::new(
                        TokenType::NewLine,
                        self.row,
                        self.row + 1,
                        self.line,
                        &self.input[self.position..self.position + 1],
                    ));
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
                        message: "Unrecognized token pattern.",
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

fn tokenize_number(input: &str, position: usize, line: usize) -> (Token, usize) {
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
            end + position,
            line,
            &input[0..end],
        ),
        end,
    );
}

fn tokenize_string(
    input: &str,
    position: usize,
    line: usize,
) -> Result<(Token, usize), TokenizeError> {
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
            message: "Did not find closing quotation mark.",
        });
    }

    return Ok((
        Token::new(
            TokenType::StrLit,
            position,
            end + position,
            line,
            &input[1..end - 1],
        ),
        end,
    ));
}

fn tokenize_write(input: &str, row: usize, line: usize) -> Result<(Token, usize), TokenizeError> {
    let str_array: Vec<char> = input.chars().collect();

    //One character write command (w or W)
    if str_array.len() == 1 || str_array[1].is_whitespace() || !str_array[1].is_ascii_alphabetic() {
        return Ok((
            Token::new(
                TokenType::Reserved(ReservedToken::Write),
                row,
                row + 1,
                line,
                &input[0..1],
            ),
            1,
        ));
    }

    // Full write command
    if input[..5].eq_ignore_ascii_case("write")
        && (str_array.len() == 5
            || str_array[5].is_whitespace()
            || !str_array[5].is_ascii_alphabetic())
    {
        return Ok((
            Token::new(
                TokenType::Reserved(ReservedToken::Write),
                row,
                row + 5,
                line,
                &input[..5],
            ),
            5,
        ));
    }

    //Identifier
    return tokenize_identifier(input, row, line);
}

fn tokenize_new(input: &str, row: usize, line: usize) -> Result<(Token, usize), TokenizeError> {
    let str_array: Vec<char> = input.chars().collect();

    // One character new command (n or N)
    if str_array.len() == 1 || str_array[1].is_whitespace() || !str_array[1].is_ascii_alphabetic() {
        return Ok((
            Token::new(
                TokenType::Reserved(ReservedToken::New),
                row,
                row + 1,
                line,
                &input[..1],
            ),
            1,
        ));
    }

    // Full new command
    if input[..3].eq_ignore_ascii_case("new")
        && (str_array.len() == 3
            || str_array[3].is_whitespace()
            || !str_array[3].is_ascii_alphabetic())
    {
        return Ok((
            Token::new(
                TokenType::Reserved(ReservedToken::New),
                row,
                row + 3,
                line,
                &input[..3],
            ),
            3,
        ));
    }

    // Identifier
    return tokenize_identifier(input, row, line);
}

fn tokenize_set_or_string(
    input: &str,
    row: usize,
    line: usize,
) -> Result<(Token, usize), TokenizeError> {
    let str_array: Vec<char> = input.chars().collect();

    // One character set command (s or S)
    if str_array.len() == 1 || str_array[1].is_whitespace() || !str_array[1].is_ascii_alphabetic() {
        return Ok((
            Token::new(
                TokenType::Reserved(ReservedToken::Set),
                row,
                row + 1,
                line,
                &input[..1],
            ),
            1,
        ));
    }

    // Full set command
    if input[..3].eq_ignore_ascii_case("set")
        && (str_array.len() == 3
            || str_array[3].is_whitespace()
            || !str_array[3].is_ascii_alphabetic())
    {
        return Ok((
            Token::new(
                TokenType::Reserved(ReservedToken::Set),
                row,
                row + 3,
                line,
                &input[..3],
            ),
            3,
        ));
    }

    // Short string keyword (str or STR)
    if input[..3].eq_ignore_ascii_case("str")
        && (str_array.len() == 3
            || str_array[3].is_whitespace()
            || !str_array[3].is_ascii_alphabetic())
    {
        return Ok((
            Token::new(
                TokenType::Reserved(ReservedToken::String),
                row,
                row + 3,
                line,
                &input[..3],
            ),
            3,
        ));
    }

    // Full string keyword
    if input[..6].eq_ignore_ascii_case("string")
        && (str_array.len() == 6
            || str_array[6].is_whitespace()
            || !str_array[6].is_ascii_alphabetic())
    {
        return Ok((
            Token::new(
                TokenType::Reserved(ReservedToken::String),
                row,
                row + 6,
                line,
                &input[..6],
            ),
            6,
        ));
    }

    // Identifier
    return tokenize_identifier(input, row, line);
}

fn tokenize_if_or_int(
    input: &str,
    row: usize,
    line: usize,
) -> Result<(Token, usize), TokenizeError> {
    let str_array: Vec<char> = input.chars().collect();

    // One character if command (i or I)
    if str_array.len() == 1 || str_array[1].is_whitespace() || !str_array[1].is_ascii_alphabetic() {
        return Ok((
            Token::new(
                TokenType::Reserved(ReservedToken::If),
                row,
                row + 1,
                line,
                &input[..1],
            ),
            1,
        ));
    }

    // Full if command
    if input[..2].eq_ignore_ascii_case("if")
        && (str_array.len() == 2
            || str_array[2].is_whitespace()
            || !str_array[2].is_ascii_alphabetic())
    {
        return Ok((
            Token::new(
                TokenType::Reserved(ReservedToken::If),
                row,
                row + 2,
                line,
                &input[..2],
            ),
            2,
        ));
    }

    // Short integer keyword (int or INT)
    if input[..3].eq_ignore_ascii_case("int")
        && (str_array.len() == 3
            || str_array[3].is_whitespace()
            || !str_array[3].is_ascii_alphabetic())
    {
        return Ok((
            Token::new(
                TokenType::Reserved(ReservedToken::Integer),
                row,
                row + 3,
                line,
                &input[..3],
            ),
            3,
        ));
    }

    // Full integer keyword
    if input[..7].eq_ignore_ascii_case("integer")
        && (str_array.len() == 7
            || str_array[7].is_whitespace()
            || !str_array[7].is_ascii_alphabetic())
    {
        return Ok((
            Token::new(
                TokenType::Reserved(ReservedToken::Integer),
                row,
                row + 7,
                line,
                &input[..7],
            ),
            7,
        ));
    }

    // Identifier
    return tokenize_identifier(input, row, line);
}

fn tokenize_else(input: &str, row: usize, line: usize) -> Result<(Token, usize), TokenizeError> {
    let str_array: Vec<char> = input.chars().collect();

    // One character else command (e or E)
    if str_array.len() == 1 || str_array[1].is_whitespace() || !str_array[1].is_ascii_alphabetic() {
        return Ok((
            Token::new(
                TokenType::Reserved(ReservedToken::Else),
                row,
                row + 1,
                line,
                &input[..1],
            ),
            1,
        ));
    }

    // Full else command
    if input[..4].eq_ignore_ascii_case("else")
        && (str_array.len() == 4
            || str_array[4].is_whitespace()
            || !str_array[4].is_ascii_alphabetic())
    {
        return Ok((
            Token::new(
                TokenType::Reserved(ReservedToken::Else),
                row,
                row + 4,
                line,
                &input[..4],
            ),
            4,
        ));
    }

    // Identifier
    return tokenize_identifier(input, row, line);
}

fn tokenize_for(input: &str, row: usize, line: usize) -> Result<(Token, usize), TokenizeError> {
    let str_array: Vec<char> = input.chars().collect();

    // One character for command (f or F)
    if str_array.len() == 1 || str_array[1].is_whitespace() || !str_array[1].is_ascii_alphabetic() {
        return Ok((
            Token::new(
                TokenType::Reserved(ReservedToken::For),
                row,
                row + 1,
                line,
                &input[..1],
            ),
            1,
        ));
    }

    // Full for command
    if input[..3].eq_ignore_ascii_case("for")
        && (str_array.len() == 3
            || str_array[3].is_whitespace()
            || !str_array[3].is_ascii_alphabetic())
    {
        return Ok((
            Token::new(
                TokenType::Reserved(ReservedToken::For),
                row,
                row + 3,
                line,
                &input[..3],
            ),
            3,
        ));
    }

    // Identifier
    return tokenize_identifier(input, row, line);
}

fn tokenize_do_or_double(
    input: &str,
    row: usize,
    line: usize,
) -> Result<(Token, usize), TokenizeError> {
    let str_array: Vec<char> = input.chars().collect();

    //TODO Lex do

    // Short double keyword (dbl or DBL)
    if input[..3].eq_ignore_ascii_case("dbl")
        && (str_array.len() == 3
            || str_array[3].is_whitespace()
            || !str_array[3].is_ascii_alphabetic())
    {
        return Ok((
            Token::new(
                TokenType::Reserved(ReservedToken::Double),
                row,
                row + 3,
                line,
                &input[..3],
            ),
            3,
        ));
    }

    // Full double keyword
    if input[..6].eq_ignore_ascii_case("double")
        && (str_array.len() == 6
            || str_array[6].is_whitespace()
            || !str_array[6].is_ascii_alphabetic())
    {
        return Ok((
            Token::new(
                TokenType::Reserved(ReservedToken::Double),
                row,
                row + 6,
                line,
                &input[..6],
            ),
            6,
        ));
    }

    //Identifier
    return tokenize_identifier(input, row, line);
}

fn tokenize_identifier(
    input: &str,
    row: usize,
    line: usize,
) -> Result<(Token, usize), TokenizeError> {
    let str_array: Vec<char> = input.chars().collect();
    let mut end = 0;

    // Legal first character
    if str_array[end] == '%' || str_array[end].is_ascii_alphabetic() {
        end = 1;
    } else {
        unreachable!("Starting with % or letter should be checked before calling this")
    }

    while end < str_array.len() && !str_array[end].is_whitespace() {
        if str_array[end].is_ascii_alphanumeric() {
            end += 1;
        } else {
            return if str_array[end] == '%' {
                Err(TokenizeError {
                    line,
                    row: row + end,
                    message: "Percent can only be the first character in an identifier.",
                })
            } else {
                break;
            };
        }
    }

    return Ok((
        Token::new(TokenType::Identifier, row, row + end, line, &input[..end]),
        end,
    ));
}

pub struct TokenizeError<'a> {
    line: usize,
    row: usize,
    message: &'a str,
}

#[cfg(test)]
mod tests {
    use super::*;

    //region Lex Arithmetic Operators Tests
    #[test]
    fn test_lex_plus() {
        let input = String::from("+");
        let mut tokenizer = Tokenizer::new(input);

        let tokens = tokenizer.tokenize();

        assert!(tokens.is_ok());
        if let Ok(tokens_ok) = tokens {
            assert_eq!(tokens_ok.len(), 1);
            assert_eq!(tokens_ok[0], Token::new(TokenType::Plus, 0, 1, 0, "+"));
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
            assert_eq!(tokens_ok[0], Token::new(TokenType::Minus, 0, 1, 0, "-"));
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
            assert_eq!(tokens_ok[0], Token::new(TokenType::Times, 0, 1, 0, "*"));
        }
    }

    #[test]
    fn test_lex_power() {
        let input = String::from("**");
        let mut tokenizer = Tokenizer::new(input);

        let tokens = tokenizer.tokenize();

        assert!(tokens.is_ok());
        if let Ok(tokens_ok) = tokens {
            assert_eq!(tokens_ok.len(), 1);
            assert_eq!(tokens_ok[0], Token::new(TokenType::Power, 0, 2, 0, "**"));
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
            assert_eq!(tokens_ok[0], Token::new(TokenType::Divide, 0, 1, 0, "/"));
        }
    }
    //endregion

    //region Lex Number Tests
    #[test]
    fn test_lex_number() {
        let input = "123";
        let (token, position) = tokenize_number(input, 0, 0);

        assert_eq!(token, Token::new(TokenType::NumLit, 0, 3, 0, "123",));
        assert_eq!(position, 3)
    }

    #[test]
    fn test_lex_number_mid_string() {
        let input = "baz 123.456 foobar";
        let (token, position) = tokenize_number(&input[4..], 5, 0);

        assert_eq!(token, Token::new(TokenType::NumLit, 5, 12, 0, "123.456",));
        assert_eq!(position, 7);
    }
    //endregion

    //region Lex Parentheses Tests
    #[test]
    fn test_lex_l_paren() {
        let input = "(";
        let mut tokenizer = Tokenizer::new(input.to_string());
        let tokens = tokenizer.tokenize();

        assert!(tokens.is_ok());
        if let Ok(tokens_ok) = tokens {
            assert_eq!(tokens_ok.len(), 1);
            assert_eq!(tokens_ok[0], Token::new(TokenType::LParen, 0, 1, 0, "("));
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
            assert_eq!(tokens_ok[0], Token::new(TokenType::RParen, 0, 1, 0, ")"));
        }
    }
    //endregion

    //region Lex Write Tests
    #[test]
    fn test_lex_write_short() {
        let input = "w";
        let mut tokenizer = Tokenizer::new(input.to_string());
        let tokens = tokenizer.tokenize();

        assert!(tokens.is_ok());
        if let Ok(tokens_ok) = tokens {
            assert_eq!(tokens_ok.len(), 1);
            assert_eq!(
                tokens_ok[0],
                Token::new(TokenType::Reserved(ReservedToken::Write), 0, 1, 0, "w")
            )
        }
    }

    #[test]
    fn test_lex_write_short_cap() {
        let input = "W";
        let mut tokenizer = Tokenizer::new(input.to_string());
        let tokens = tokenizer.tokenize();

        assert!(tokens.is_ok());
        if let Ok(tokens_ok) = tokens {
            assert_eq!(tokens_ok.len(), 1);
            assert_eq!(
                tokens_ok[0],
                Token::new(TokenType::Reserved(ReservedToken::Write), 0, 1, 0, "W")
            )
        }
    }

    #[test]
    fn test_lex_write_long() {
        let input = "write";
        let mut tokenizer = Tokenizer::new(input.to_string());
        let tokens = tokenizer.tokenize();

        assert!(tokens.is_ok());
        if let Ok(tokens_ok) = tokens {
            assert_eq!(tokens_ok.len(), 1);
            assert_eq!(
                tokens_ok[0],
                Token::new(TokenType::Reserved(ReservedToken::Write), 0, 5, 0, "write")
            )
        }
    }

    #[test]
    fn test_lex_write_long_cap() {
        let input = "WRITE";
        let mut tokenizer = Tokenizer::new(input.to_string());
        let tokens = tokenizer.tokenize();

        assert!(tokens.is_ok());
        if let Ok(tokens_ok) = tokens {
            assert_eq!(tokens_ok.len(), 1);
            assert_eq!(
                tokens_ok[0],
                Token::new(TokenType::Reserved(ReservedToken::Write), 0, 5, 0, "WRITE")
            )
        }
    }
    //endregion

    //region Lex Write Format Tests
    #[test]
    fn test_lex_bang() {
        let input = "!";
        let mut tokenizer = Tokenizer::new(input.to_string());
        let tokens = tokenizer.tokenize();

        assert!(tokens.is_ok());
        if let Ok(tokens_ok) = tokens {
            assert_eq!(tokens_ok.len(), 1);
            assert_eq!(tokens_ok[0], Token::new(TokenType::Bang, 0, 1, 0, "!"))
        }
    }

    #[test]
    fn test_lex_hash() {
        let input = "#";
        let mut tokenizer = Tokenizer::new(input.to_string());
        let tokens = tokenizer.tokenize();

        assert!(tokens.is_ok());
        if let Ok(tokens_ok) = tokens {
            assert_eq!(tokens_ok.len(), 1);
            assert_eq!(tokens_ok[0], Token::new(TokenType::Hash, 0, 1, 0, "#"))
        }
    }

    #[test]
    fn test_lex_question_mark() {
        let input = "?";
        let mut tokenizer = Tokenizer::new(input.to_string());
        let tokens = tokenizer.tokenize();

        assert!(tokens.is_ok());
        if let Ok(tokens_ok) = tokens {
            assert_eq!(tokens_ok.len(), 1);
            assert_eq!(
                tokens_ok[0],
                Token::new(TokenType::QuestionMark, 0, 1, 0, "?")
            )
        }
    }
    //endregion

    //region Lex Comparison Operators Tests
    #[test]
    fn test_lex_greater_than() {
        let input = ">";
        let mut tokenizer = Tokenizer::new(input.to_string());
        let tokens = tokenizer.tokenize();

        assert!(tokens.is_ok());
        if let Ok(tokens_ok) = tokens {
            assert_eq!(tokens_ok.len(), 1);
            assert_eq!(
                tokens_ok[0],
                Token::new(TokenType::GreaterThan, 0, 1, 0, ">")
            )
        }
    }

    #[test]
    fn test_lex_greater_than_or_equal_to() {
        let input = ">=";
        let mut tokenizer = Tokenizer::new(input.to_string());
        let tokens = tokenizer.tokenize();

        assert!(tokens.is_ok());
        if let Ok(tokens_ok) = tokens {
            assert_eq!(tokens_ok.len(), 1);
            assert_eq!(
                tokens_ok[0],
                Token::new(TokenType::GreaterThanOrEqualTo, 0, 2, 0, ">=")
            )
        }
    }

    #[test]
    fn test_lex_less_than() {
        let input = "<";
        let mut tokenizer = Tokenizer::new(input.to_string());
        let tokens = tokenizer.tokenize();

        assert!(tokens.is_ok());
        if let Ok(tokens_ok) = tokens {
            assert_eq!(tokens_ok.len(), 1);
            assert_eq!(tokens_ok[0], Token::new(TokenType::LessThan, 0, 1, 0, "<"))
        }
    }

    #[test]
    fn test_lex_less_than_or_equal_to() {
        let input = "<=";
        let mut tokenizer = Tokenizer::new(input.to_string());
        let tokens = tokenizer.tokenize();

        assert!(tokens.is_ok());
        if let Ok(tokens_ok) = tokens {
            assert_eq!(tokens_ok.len(), 1);
            assert_eq!(
                tokens_ok[0],
                Token::new(TokenType::LessThanOrEqualTo, 0, 2, 0, "<=")
            )
        }
    }

    #[test]
    fn test_lex_equals() {
        let input = "=";
        let mut tokenizer = Tokenizer::new(input.to_string());
        let tokens = tokenizer.tokenize();

        assert!(tokens.is_ok());
        if let Ok(tokens_ok) = tokens {
            assert_eq!(tokens_ok.len(), 1);
            assert_eq!(tokens_ok[0], Token::new(TokenType::Equals, 0, 1, 0, "="))
        }
    }

    #[test]
    fn test_lex_not_equals() {
        let input = "'=";
        let mut tokenizer = Tokenizer::new(input.to_string());
        let tokens = tokenizer.tokenize();

        assert!(tokens.is_ok());
        if let Ok(tokens_ok) = tokens {
            assert_eq!(tokens_ok.len(), 1);
            assert_eq!(
                tokens_ok[0],
                Token::new(TokenType::NotEquals, 0, 2, 0, "'=")
            )
        }
    }
    //endregion

    //region Lex New Tests
    #[test]
    fn test_lex_new_short() {
        let input = "n";
        let mut tokenizer = Tokenizer::new(input.to_string());
        let tokens = tokenizer.tokenize();

        assert!(tokens.is_ok());
        if let Ok(tokens_ok) = tokens {
            assert_eq!(tokens_ok.len(), 1);
            assert_eq!(
                tokens_ok[0],
                Token::new(TokenType::Reserved(ReservedToken::New), 0, 1, 0, "n")
            )
        }
    }

    #[test]
    fn test_lex_new_short_cap() {
        let input = "N";
        let mut tokenizer = Tokenizer::new(input.to_string());
        let tokens = tokenizer.tokenize();

        assert!(tokens.is_ok());
        if let Ok(tokens_ok) = tokens {
            assert_eq!(tokens_ok.len(), 1);
            assert_eq!(
                tokens_ok[0],
                Token::new(TokenType::Reserved(ReservedToken::New), 0, 1, 0, "N")
            )
        }
    }

    #[test]
    fn test_lex_new_long() {
        let input = "new";
        let mut tokenizer = Tokenizer::new(input.to_string());
        let tokens = tokenizer.tokenize();

        assert!(tokens.is_ok());
        if let Ok(tokens_ok) = tokens {
            assert_eq!(tokens_ok.len(), 1);
            assert_eq!(
                tokens_ok[0],
                Token::new(TokenType::Reserved(ReservedToken::New), 0, 3, 0, "new")
            )
        }
    }

    #[test]
    fn test_lex_new_long_cap() {
        let input = "NEW";
        let mut tokenizer = Tokenizer::new(input.to_string());
        let tokens = tokenizer.tokenize();

        assert!(tokens.is_ok());
        if let Ok(tokens_ok) = tokens {
            assert_eq!(tokens_ok.len(), 1);
            assert_eq!(
                tokens_ok[0],
                Token::new(TokenType::Reserved(ReservedToken::New), 0, 3, 0, "NEW")
            )
        }
    }
    //endregion

    //region Lex Set Tests
    #[test]
    fn test_lex_set_short() {
        let input = "s";
        let mut tokenizer = Tokenizer::new(input.to_string());
        let tokens = tokenizer.tokenize();

        assert!(tokens.is_ok());
        if let Ok(tokens_ok) = tokens {
            assert_eq!(tokens_ok.len(), 1);
            assert_eq!(
                tokens_ok[0],
                Token::new(TokenType::Reserved(ReservedToken::Set), 0, 1, 0, "s")
            )
        }
    }

    #[test]
    fn test_lex_set_short_cap() {
        let input = "S";
        let mut tokenizer = Tokenizer::new(input.to_string());
        let tokens = tokenizer.tokenize();

        assert!(tokens.is_ok());
        if let Ok(tokens_ok) = tokens {
            assert_eq!(tokens_ok.len(), 1);
            assert_eq!(
                tokens_ok[0],
                Token::new(TokenType::Reserved(ReservedToken::Set), 0, 1, 0, "S")
            )
        }
    }

    #[test]
    fn test_lex_set_long() {
        let input = "set";
        let mut tokenizer = Tokenizer::new(input.to_string());
        let tokens = tokenizer.tokenize();

        assert!(tokens.is_ok());
        if let Ok(tokens_ok) = tokens {
            assert_eq!(tokens_ok.len(), 1);
            assert_eq!(
                tokens_ok[0],
                Token::new(TokenType::Reserved(ReservedToken::Set), 0, 3, 0, "set")
            )
        }
    }

    #[test]
    fn test_lex_set_long_cap() {
        let input = "SET";
        let mut tokenizer = Tokenizer::new(input.to_string());
        let tokens = tokenizer.tokenize();

        assert!(tokens.is_ok());
        if let Ok(tokens_ok) = tokens {
            assert_eq!(tokens_ok.len(), 1);
            assert_eq!(
                tokens_ok[0],
                Token::new(TokenType::Reserved(ReservedToken::Set), 0, 3, 0, "SET")
            )
        }
    }
    //endregion

    //region Lex String Tests
    #[test]
    fn test_lex_string() {
        let input = "string";
        let mut tokenizer = Tokenizer::new(input.to_string());
        let tokens = tokenizer.tokenize();

        assert!(tokens.is_ok());
        if let Ok(tokens_ok) = tokens {
            assert_eq!(tokens_ok.len(), 1);
            assert_eq!(
                tokens_ok[0],
                Token::new(
                    TokenType::Reserved(ReservedToken::String),
                    0,
                    6,
                    0,
                    "string"
                )
            )
        }
    }

    #[test]
    fn test_lex_string_cap() {
        let input = "STRING";
        let mut tokenizer = Tokenizer::new(input.to_string());
        let tokens = tokenizer.tokenize();

        assert!(tokens.is_ok());
        if let Ok(tokens_ok) = tokens {
            assert_eq!(tokens_ok.len(), 1);
            assert_eq!(
                tokens_ok[0],
                Token::new(
                    TokenType::Reserved(ReservedToken::String),
                    0,
                    6,
                    0,
                    "STRING"
                )
            )
        }
    }
    //endregion

    //region Lex Double Tests
    #[test]
    fn test_lex_double() {
        let input = "double";
        let mut tokenizer = Tokenizer::new(input.to_string());
        let tokens = tokenizer.tokenize();

        assert!(tokens.is_ok());
        if let Ok(tokens_ok) = tokens {
            assert_eq!(tokens_ok.len(), 1);
            assert_eq!(
                tokens_ok[0],
                Token::new(
                    TokenType::Reserved(ReservedToken::Double),
                    0,
                    6,
                    0,
                    "double"
                )
            )
        }
    }

    #[test]
    fn test_lex_double_cap() {
        let input = "DOUBLE";
        let mut tokenizer = Tokenizer::new(input.to_string());
        let tokens = tokenizer.tokenize();

        assert!(tokens.is_ok());
        if let Ok(tokens_ok) = tokens {
            assert_eq!(tokens_ok.len(), 1);
            assert_eq!(
                tokens_ok[0],
                Token::new(
                    TokenType::Reserved(ReservedToken::Double),
                    0,
                    6,
                    0,
                    "DOUBLE"
                )
            )
        }
    }
    //endregion

    #[test]
    fn test_lex_identifier() {
        let input = "foo %foo foo123";
        let mut tokenizer = Tokenizer::new(input.to_string());
        let tokens = tokenizer.tokenize();

        assert!(tokens.is_ok());
        if let Ok(tokens_ok) = tokens {
            assert_eq!(tokens_ok.len(), 3);
            assert_eq!(
                tokens_ok[0],
                Token::new(TokenType::Identifier, 0, 3, 0, "foo")
            );
            assert_eq!(
                tokens_ok[1],
                Token::new(TokenType::Identifier, 4, 8, 0, "%foo")
            );
            assert_eq!(
                tokens_ok[2],
                Token::new(TokenType::Identifier, 9, 15, 0, "foo123")
            )
        }
    }

    #[test]
    fn test_lex_new_line() {
        let input = "\n";
        let mut tokenizer = Tokenizer::new(input.to_string());
        let tokens = tokenizer.tokenize();

        assert!(tokens.is_ok());
        if let Ok(tokens_ok) = tokens {
            assert_eq!(tokens_ok.len(), 1);
            assert_eq!(tokens_ok[0], Token::new(TokenType::NewLine, 0, 1, 0, "\n"))
        }
    }

    //region Lex If Tests
    #[test]
    fn test_lex_if_short() {
        let input = "i";
        let mut tokenizer = Tokenizer::new(input.to_string());
        let tokens = tokenizer.tokenize();

        assert!(tokens.is_ok());
        if let Ok(tokens_ok) = tokens {
            assert_eq!(tokens_ok.len(), 1);
            assert_eq!(
                tokens_ok[0],
                Token::new(TokenType::Reserved(ReservedToken::If), 0, 1, 0, "i")
            )
        }
    }

    #[test]
    fn test_lex_if_short_cap() {
        let input = "I";
        let mut tokenizer = Tokenizer::new(input.to_string());
        let tokens = tokenizer.tokenize();

        assert!(tokens.is_ok());
        if let Ok(tokens_ok) = tokens {
            assert_eq!(tokens_ok.len(), 1);
            assert_eq!(
                tokens_ok[0],
                Token::new(TokenType::Reserved(ReservedToken::If), 0, 1, 0, "I")
            )
        }
    }

    #[test]
    fn test_lex_if_long() {
        let input = "if";
        let mut tokenizer = Tokenizer::new(input.to_string());
        let tokens = tokenizer.tokenize();

        assert!(tokens.is_ok());
        if let Ok(tokens_ok) = tokens {
            assert_eq!(tokens_ok.len(), 1);
            assert_eq!(
                tokens_ok[0],
                Token::new(TokenType::Reserved(ReservedToken::If), 0, 2, 0, "if")
            )
        }
    }

    #[test]
    fn test_lex_if_long_cap() {
        let input = "IF";
        let mut tokenizer = Tokenizer::new(input.to_string());
        let tokens = tokenizer.tokenize();

        assert!(tokens.is_ok());
        if let Ok(tokens_ok) = tokens {
            assert_eq!(tokens_ok.len(), 1);
            assert_eq!(
                tokens_ok[0],
                Token::new(TokenType::Reserved(ReservedToken::If), 0, 2, 0, "IF")
            )
        }
    }
    //endregion

    //region Lex Integer Tests
    #[test]
    fn test_lex_int() {
        let input = "integer";
        let mut tokenizer = Tokenizer::new(input.to_string());
        let tokens = tokenizer.tokenize();

        assert!(tokens.is_ok());
        if let Ok(tokens_ok) = tokens {
            assert_eq!(tokens_ok.len(), 1);
            assert_eq!(
                tokens_ok[0],
                Token::new(
                    TokenType::Reserved(ReservedToken::Integer),
                    0,
                    7,
                    0,
                    "integer"
                )
            )
        }
    }

    #[test]
    fn test_lex_int_cap() {
        let input = "INTEGER";
        let mut tokenizer = Tokenizer::new(input.to_string());
        let tokens = tokenizer.tokenize();

        assert!(tokens.is_ok());
        if let Ok(tokens_ok) = tokens {
            assert_eq!(tokens_ok.len(), 1);
            assert_eq!(
                tokens_ok[0],
                Token::new(
                    TokenType::Reserved(ReservedToken::Integer),
                    0,
                    7,
                    0,
                    "INTEGER"
                )
            )
        }
    }

    #[test]
    fn test_lex_int_short() {
        let input = "int";
        let mut tokenizer = Tokenizer::new(input.to_string());
        let tokens = tokenizer.tokenize();

        assert!(tokens.is_ok());
        if let Ok(tokens_ok) = tokens {
            assert_eq!(tokens_ok.len(), 1);
            assert_eq!(
                tokens_ok[0],
                Token::new(TokenType::Reserved(ReservedToken::Integer), 0, 3, 0, "int")
            )
        }
    }

    #[test]
    fn test_lex_int_cap_short() {
        let input = "INT";
        let mut tokenizer = Tokenizer::new(input.to_string());
        let tokens = tokenizer.tokenize();

        assert!(tokens.is_ok());
        if let Ok(tokens_ok) = tokens {
            assert_eq!(tokens_ok.len(), 1);
            assert_eq!(
                tokens_ok[0],
                Token::new(TokenType::Reserved(ReservedToken::Integer), 0, 3, 0, "INT")
            )
        }
    }
    //endregion

    //region Lex Else Tests
    #[test]
    fn test_lex_else_short() {
        let input = "e";
        let mut tokenizer = Tokenizer::new(input.to_string());
        let tokens = tokenizer.tokenize();

        assert!(tokens.is_ok());
        if let Ok(tokens_ok) = tokens {
            assert_eq!(tokens_ok.len(), 1);
            assert_eq!(
                tokens_ok[0],
                Token::new(TokenType::Reserved(ReservedToken::Else), 0, 1, 0, "e")
            )
        }
    }

    #[test]
    fn test_lex_else_short_cap() {
        let input = "E";
        let mut tokenizer = Tokenizer::new(input.to_string());
        let tokens = tokenizer.tokenize();

        assert!(tokens.is_ok());
        if let Ok(tokens_ok) = tokens {
            assert_eq!(tokens_ok.len(), 1);
            assert_eq!(
                tokens_ok[0],
                Token::new(TokenType::Reserved(ReservedToken::Else), 0, 1, 0, "E")
            )
        }
    }

    #[test]
    fn test_lex_else_long() {
        let input = "else";
        let mut tokenizer = Tokenizer::new(input.to_string());
        let tokens = tokenizer.tokenize();

        assert!(tokens.is_ok());
        if let Ok(tokens_ok) = tokens {
            assert_eq!(tokens_ok.len(), 1);
            assert_eq!(
                tokens_ok[0],
                Token::new(TokenType::Reserved(ReservedToken::Else), 0, 4, 0, "else")
            )
        }
    }

    #[test]
    fn test_lex_else_long_cap() {
        let input = "ELSE";
        let mut tokenizer = Tokenizer::new(input.to_string());
        let tokens = tokenizer.tokenize();

        assert!(tokens.is_ok());
        if let Ok(tokens_ok) = tokens {
            assert_eq!(tokens_ok.len(), 1);
            assert_eq!(
                tokens_ok[0],
                Token::new(TokenType::Reserved(ReservedToken::Else), 0, 4, 0, "ELSE")
            )
        }
    }
    //endregion

    #[test]
    fn test_lex_colon() {
        let input = ":";
        let mut tokenizer = Tokenizer::new(input.to_string());
        let tokens = tokenizer.tokenize();

        assert!(tokens.is_ok());
        if let Ok(tokens_ok) = tokens {
            assert_eq!(tokens_ok.len(), 1);
            assert_eq!(tokens_ok[0], Token::new(TokenType::Colon, 0, 1, 0, ":"))
        }
    }

    //region Lex For Tests
    #[test]
    fn test_lex_for_short() {
        let input = "f";
        let mut tokenizer = Tokenizer::new(input.to_string());
        let tokens = tokenizer.tokenize();

        assert!(tokens.is_ok());
        if let Ok(tokens_ok) = tokens {
            assert_eq!(tokens_ok.len(), 1);
            assert_eq!(
                tokens_ok[0],
                Token::new(TokenType::Reserved(ReservedToken::For), 0, 1, 0, "f")
            )
        }
    }

    #[test]
    fn test_lex_for_short_cap() {
        let input = "F";
        let mut tokenizer = Tokenizer::new(input.to_string());
        let tokens = tokenizer.tokenize();

        assert!(tokens.is_ok());
        if let Ok(tokens_ok) = tokens {
            assert_eq!(tokens_ok.len(), 1);
            assert_eq!(
                tokens_ok[0],
                Token::new(TokenType::Reserved(ReservedToken::For), 0, 1, 0, "F")
            )
        }
    }

    #[test]
    fn test_lex_for_long() {
        let input = "for";
        let mut tokenizer = Tokenizer::new(input.to_string());
        let tokens = tokenizer.tokenize();

        assert!(tokens.is_ok());
        if let Ok(tokens_ok) = tokens {
            assert_eq!(tokens_ok.len(), 1);
            assert_eq!(
                tokens_ok[0],
                Token::new(TokenType::Reserved(ReservedToken::For), 0, 3, 0, "for")
            )
        }
    }

    #[test]
    fn test_lex_else_for_cap() {
        let input = "FOR";
        let mut tokenizer = Tokenizer::new(input.to_string());
        let tokens = tokenizer.tokenize();

        assert!(tokens.is_ok());
        if let Ok(tokens_ok) = tokens {
            assert_eq!(tokens_ok.len(), 1);
            assert_eq!(
                tokens_ok[0],
                Token::new(TokenType::Reserved(ReservedToken::For), 0, 3, 0, "FOR")
            )
        }
    }
    //endregion

    //region Lex Curly tests

    #[test]
    fn test_lex_l_curly() {
        let input = "{";
        let mut tokenizer = Tokenizer::new(input.to_string());
        let tokens = tokenizer.tokenize();

        assert!(tokens.is_ok());
        if let Ok(tokens_ok) = tokens {
            assert_eq!(tokens_ok.len(), 1);
            assert_eq!(tokens_ok[0], Token::new(TokenType::LCurly, 0, 1, 0, "{"))
        }
    }

    #[test]
    fn test_lex_r_curly() {
        let input = "}";
        let mut tokenizer = Tokenizer::new(input.to_string());
        let tokens = tokenizer.tokenize();

        assert!(tokens.is_ok());
        if let Ok(tokens_ok) = tokens {
            assert_eq!(tokens_ok.len(), 1);
            assert_eq!(tokens_ok[0], Token::new(TokenType::RCurly, 0, 1, 0, "}"))
        }
    }

    //endregion

    #[test]
    fn test_lex_dollar_dollar() {
        let input = "$$";
        let mut tokenizer = Tokenizer::new(input.to_string());
        let tokens = tokenizer.tokenize();

        assert!(tokens.is_ok());
        if let Ok(tokens_ok) = tokens {
            assert_eq!(tokens_ok.len(), 1);
            assert_eq!(
                tokens_ok[0],
                Token::new(TokenType::DollarDollar, 0, 2, 0, "$$")
            )
        }
    }
}
