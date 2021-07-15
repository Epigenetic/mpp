use std::fmt;

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
        }
    }
}
