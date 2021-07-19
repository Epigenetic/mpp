/*
 * Copyright (c) 2021. Jonah Shafran
 *
 * SPDX-License-Identifier: GPL-3.0-only
 */

use crate::lexer::TokenType;
use crate::parser::parse_node::{AddOp, MulOp, ParserNode};
use crate::parser::ParserNodeType;
use rust_decimal::Decimal;
use std::str::FromStr;

pub struct Parser<'a> {
    tokens: Vec<TokenType<'a>>,
}

impl Parser<'_> {
    pub fn new(tokens: Vec<TokenType>) -> Parser {
        Parser { tokens }
    }

    pub fn parse(&self) -> Option<ParserNode> {
        if self.tokens.len() == 0 {
            return None;
        }

        let (expression, expression_rest) = parse_expression(&self.tokens[0..]);
        if expression_rest.len() != 0 {
            println!("Warning: not all tokens processed.");
            for token in expression_rest {
                println!("{}", token);
            }
        }
        return expression;
    }
}
/// Expression -> Term ExpressionTail
fn parse_expression<'a>(tokens: &'a [TokenType]) -> (Option<ParserNode>, &'a [TokenType<'a>]) {
    let (term, term_rest) = parse_term(tokens);

    if let Some(term_node) = term {
        let (expression_tail, expression_tail_rest) = parse_expression_tail(term_rest);

        return if let Some(expression_tail_node) = expression_tail {
            (
                Some(ParserNode::new(
                    vec![term_node, expression_tail_node],
                    ParserNodeType::Expression,
                )),
                expression_tail_rest,
            )
        } else {
            (
                Some(ParserNode::new(vec![term_node], ParserNodeType::Expression)),
                term_rest,
            )
        };
    } else {
        handle_syntax_error(&term_rest, "Term");
        unreachable!();
    }
}

/// ExpressionTail -> ε | AddOp Term ExpressionTail
fn parse_expression_tail<'a>(tokens: &'a [TokenType]) -> (Option<ParserNode>, &'a [TokenType<'a>]) {
    let (add_op, add_op_rest) = parse_add_op(tokens);

    if let Some(add_op_node) = add_op {
        let (term, term_rest) = parse_term(add_op_rest);

        if let Some(term_node) = term {
            let (expression_tail, expression_tail_rest) = parse_expression_tail(term_rest);

            return if let Some(expression_tail_node) = expression_tail {
                (
                    Some(ParserNode::new(
                        vec![add_op_node, term_node, expression_tail_node],
                        ParserNodeType::ExpressionTail,
                    )),
                    expression_tail_rest,
                )
            } else {
                (
                    Some(ParserNode::new(
                        vec![add_op_node, term_node],
                        ParserNodeType::ExpressionTail,
                    )),
                    expression_tail_rest,
                )
            };
        } else {
            handle_syntax_error(term_rest, "Term");
            unreachable!();
        }
    } else {
        return (None, tokens);
    }
}

/// AddOp -> + | -
fn parse_add_op<'a>(tokens: &'a [TokenType]) -> (Option<ParserNode>, &'a [TokenType<'a>]) {
    if tokens.len() == 0 {
        return (None, tokens);
    }

    return match &tokens[0] {
        TokenType::Plus { position: _ } => (
            Some(ParserNode::new(
                Vec::new(),
                ParserNodeType::AddOp(AddOp::Plus),
            )),
            &tokens[1..],
        ),
        TokenType::Minus { position: _ } => (
            Some(ParserNode::new(
                Vec::new(),
                ParserNodeType::AddOp(AddOp::Minus),
            )),
            &tokens[1..],
        ),
        _ => (None, tokens),
    };
}

/// Term -> Factor TermTail
fn parse_term<'a>(tokens: &'a [TokenType]) -> (Option<ParserNode>, &'a [TokenType<'a>]) {
    let (factor, factor_rest) = parse_factor(tokens);

    if let Some(factor_node) = factor {
        let (term_tail, term_tail_rest) = parse_term_tail(factor_rest);

        return if let Some(term_tail_node) = term_tail {
            (
                Some(ParserNode::new(
                    vec![factor_node, term_tail_node],
                    ParserNodeType::Term,
                )),
                term_tail_rest,
            )
        } else {
            (
                Some(ParserNode::new(vec![factor_node], ParserNodeType::Term)),
                factor_rest,
            )
        };
    } else {
        handle_syntax_error(&factor_rest, "Factor");
        unreachable!();
    }
}

/// TermTail -> ε | MulOp Factor TermTail
fn parse_term_tail<'a>(tokens: &'a [TokenType]) -> (Option<ParserNode>, &'a [TokenType<'a>]) {
    let (mul_op, mul_op_rest) = parse_mul_op(tokens);

    return if let Some(mul_node) = mul_op {
        let (factor, factor_rest) = parse_factor(mul_op_rest);

        if let Some(factor_node) = factor {
            let (term_tail, term_tail_rest) = parse_term_tail(factor_rest);

            if let Some(term_tail_node) = term_tail {
                let parse_node = ParserNode::new(
                    vec![mul_node, factor_node, term_tail_node],
                    ParserNodeType::TermTail,
                );
                (Some(parse_node), term_tail_rest)
            } else {
                (
                    Some(ParserNode::new(
                        vec![mul_node, factor_node],
                        ParserNodeType::TermTail,
                    )),
                    term_tail_rest,
                )
            }
        } else {
            handle_syntax_error(factor_rest, "Factor");
            unreachable!();
        }
    } else {
        (None, &tokens)
    };
}

/// MulOp -> * | /
fn parse_mul_op<'a>(tokens: &'a [TokenType]) -> (Option<ParserNode>, &'a [TokenType<'a>]) {
    if tokens.len() == 0 {
        return (None, tokens);
    }

    return match &tokens[0] {
        TokenType::Times { position: _ } => (
            Some(ParserNode::new(
                Vec::new(),
                ParserNodeType::MulOp(MulOp::Times),
            )),
            &tokens[1..],
        ),
        TokenType::Divide { position: _ } => (
            Some(ParserNode::new(
                Vec::new(),
                ParserNodeType::MulOp(MulOp::Divide),
            )),
            &tokens[1..],
        ),
        _ => (None, tokens),
    };
}

/// Factor -> NumericLiteral | - NumericLiteral
fn parse_factor<'a>(tokens: &'a [TokenType]) -> (Option<ParserNode>, &'a [TokenType<'a>]) {
    if tokens.len() == 0 {
        handle_syntax_error(tokens, "Factor");
        unreachable!();
    }

    match &tokens[0] {
        // NumericLiteral
        TokenType::NumLit {
            value,
            start: _,
            end: _,
        } => {
            let numeric_literal = ParserNode::new(
                Vec::new(),
                ParserNodeType::NumericLiteral(Decimal::from_str(value).unwrap()),
            );
            return (
                Some(ParserNode::new(
                    vec![numeric_literal],
                    ParserNodeType::Factor,
                )),
                &tokens[1..],
            );
        }

        // - NumericLiteral
        TokenType::Minus { position: _ } => {
            if let TokenType::NumLit {
                value,
                start: _,
                end: _,
            } = tokens[1]
            {
                let numeric_literal = ParserNode::new(
                    Vec::new(),
                    ParserNodeType::NumericLiteral(
                        Decimal::from_str(&*format!("-{}", value)).unwrap(),
                    ),
                );
                return (
                    Some(ParserNode::new(
                        vec![numeric_literal],
                        ParserNodeType::Factor,
                    )),
                    &tokens[2..],
                );
            } else {
                handle_syntax_error(tokens, "Factor");
                unreachable!()
            }
        }
        _ => {
            todo!();
            unreachable!();
        }
    }
}

fn handle_syntax_error(tokens: &[TokenType], expected: &str) {
    if tokens.len() == 0 {
        panic!("Expected {} found EOF instead", expected);
    } else {
        panic!("Expected {} found {} instead", expected, tokens[0]);
    }
}
