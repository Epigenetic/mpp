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
    pub line: usize,
    pub value: &'a str,
}

impl Token<'_> {
    pub fn new(token_type: TokenType, start: usize, end: usize, line: usize, value: &str) -> Token {
        Token {
            token_type,
            start,
            end,
            line,
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
    Identifier,

    Plus,
    Minus,

    Times,
    Divide,
    Hash,
    IntDivide,

    Power,
    LParen,
    RParen,

    Bang,
    QuestionMark,

    LessThanOrEqualTo,
    GreaterThanOrEqualTo,
    GreaterThan,
    LessThan,

    Not,
    Equals,
    NotEquals,

    Reserved(ReservedToken),

    Comma,
    Colon,
    NewLine,
    LCurly,
    RCurly,
}

#[derive(PartialEq, Debug)]
pub enum ReservedToken {
    Write,
    New,
    Set,
    If,
    Else,
    For,
    Integer,
    String,
    Double,
}

impl fmt::Display for TokenType {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}
