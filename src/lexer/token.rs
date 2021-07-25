/*
 * Copyright (c) 2021, Jonah Shafran.
 *
 * SPDX-License-Identifier: GPL-3.0-only
 */

use std::fmt;

#[derive(PartialEq, Debug)]
pub struct Token<'a> {
    pub token_type: TokenType,
    pub start: usize,
    pub end: usize,
    pub value: &'a str,
}

impl Token<'_> {
    pub fn new(token_type: TokenType, start: usize, end: usize, value: &str) -> Token {
        Token {
            token_type,
            start,
            end,
            value,
        }
    }
}

impl fmt::Display for Token<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "{{Token token_type: {} start: {} end: {} value: {}}}",
            self.token_type, self.start, self.end, self.value
        )
    }
}

#[derive(PartialEq, Debug)]
pub enum TokenType {
    NumLit,
    StrLit,
    Plus,
    Minus,
    Times,
    Divide,
    Modulus,
    LParen,
    RParen,
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}
