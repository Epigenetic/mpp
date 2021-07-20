/*
 * Copyright (c) 2021, Jonah Shafran.
 *
 * SPDX-License-Identifier: GPL-3.0-only
 */

use std::fmt;

#[derive(PartialEq, Debug)]
pub enum TokenType<'a> {
    NumLit {
        value: &'a str,
        start: usize,
        end: usize,
    },
    Plus {
        position: usize,
    },
    Minus {
        position: usize,
    },
    Times {
        position: usize,
    },
    Divide {
        position: usize,
    },
}

impl fmt::Display for TokenType<'_> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TokenType::NumLit { value, start, end } => write!(
                f,
                "(NumLit, value: {}, start: {}, end: {})",
                value, start, end
            ),
            TokenType::Plus { position } => write!(f, "(Plus, position:{})", position),
            TokenType::Minus { position } => write!(f, "(Minus, position: {})", position),
            TokenType::Times { position } => write!(f, "(Times, position: {})", position),
            TokenType::Divide { position } => write!(f, "(Divide, position: {})", position),
        }
    }
}
