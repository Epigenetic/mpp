/*
 * Copyright (c) 2021. Jonah Shafran
 *
 * SPDX-License-Identifier: GPL-3.0-only
 */

use crate::lexer::{Token, TokenType};
use crate::parser::parse_node::{AddOp, MulOp, ParserNode, UnaryOp};
use crate::parser::ParserNodeType;
use crate::runtime::MVal;

pub struct Parser<'a> {
    tokens: Vec<Token<'a>>,
}

impl Parser<'_> {
    pub fn new(tokens: Vec<Token>) -> Parser {
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
fn parse_expression<'a>(tokens: &'a [Token]) -> (Option<ParserNode>, &'a [Token<'a>]) {
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
fn parse_expression_tail<'a>(tokens: &'a [Token]) -> (Option<ParserNode>, &'a [Token<'a>]) {
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
fn parse_add_op<'a>(tokens: &'a [Token]) -> (Option<ParserNode>, &'a [Token<'a>]) {
    if tokens.len() == 0 {
        return (None, tokens);
    }

    return match &tokens[0].token_type {
        TokenType::Plus => (
            Some(ParserNode::new(
                Vec::new(),
                ParserNodeType::AddOp(AddOp::Plus),
            )),
            &tokens[1..],
        ),
        TokenType::Minus => (
            Some(ParserNode::new(
                Vec::new(),
                ParserNodeType::AddOp(AddOp::Minus),
            )),
            &tokens[1..],
        ),
        _ => (None, tokens),
    };
}

/// Term -> Unary TermTail
fn parse_term<'a>(tokens: &'a [Token]) -> (Option<ParserNode>, &'a [Token<'a>]) {
    let (unary, unary_rest) = parse_unary(tokens);

    if let Some(unary_node) = unary {
        let (term_tail, term_tail_rest) = parse_term_tail(unary_rest);

        return if let Some(term_tail_node) = term_tail {
            (
                Some(ParserNode::new(
                    vec![unary_node, term_tail_node],
                    ParserNodeType::Term,
                )),
                term_tail_rest,
            )
        } else {
            (
                Some(ParserNode::new(vec![unary_node], ParserNodeType::Term)),
                unary_rest,
            )
        };
    } else {
        handle_syntax_error(&unary_rest, "Factor");
        unreachable!();
    }
}

/// TermTail -> ε | MulOp Unary TermTail
fn parse_term_tail<'a>(tokens: &'a [Token]) -> (Option<ParserNode>, &'a [Token<'a>]) {
    let (mul_op, mul_op_rest) = parse_mul_op(tokens);

    return if let Some(mul_node) = mul_op {
        let (unary, unary_rest) = parse_unary(mul_op_rest);

        if let Some(unary_node) = unary {
            let (term_tail, term_tail_rest) = parse_term_tail(unary_rest);

            if let Some(term_tail_node) = term_tail {
                let parse_node = ParserNode::new(
                    vec![mul_node, unary_node, term_tail_node],
                    ParserNodeType::TermTail,
                );
                (Some(parse_node), term_tail_rest)
            } else {
                (
                    Some(ParserNode::new(
                        vec![mul_node, unary_node],
                        ParserNodeType::TermTail,
                    )),
                    term_tail_rest,
                )
            }
        } else {
            handle_syntax_error(unary_rest, "Factor");
            unreachable!();
        }
    } else {
        (None, &tokens)
    };
}

/// MulOp -> * | / | # | \ | ε
fn parse_mul_op<'a>(tokens: &'a [Token]) -> (Option<ParserNode>, &'a [Token<'a>]) {
    if tokens.len() == 0 {
        return (None, tokens);
    }

    return match &tokens[0].token_type {
        TokenType::Times => (
            Some(ParserNode::new(
                Vec::new(),
                ParserNodeType::MulOp(MulOp::Times),
            )),
            &tokens[1..],
        ),
        TokenType::Divide => (
            Some(ParserNode::new(
                Vec::new(),
                ParserNodeType::MulOp(MulOp::Divide),
            )),
            &tokens[1..],
        ),
        TokenType::Modulus => (
            Some(ParserNode::new(
                Vec::new(),
                ParserNodeType::MulOp(MulOp::Modulus),
            )),
            &tokens[1..],
        ),
        TokenType::IntDivide => (
            Some(ParserNode::new(
                Vec::new(),
                ParserNodeType::MulOp(MulOp::IntegerDivide),
            )),
            &tokens[1..],
        ),
        _ => (None, tokens),
    };
}

/// Unary -> UnaryOp Unary | Factor
fn parse_unary<'a>(tokens: &'a [Token]) -> (Option<ParserNode>, &'a [Token<'a>]) {
    let (unary_op, unary_op_rest) = parse_unary_op(tokens);

    return if let Some(unary_op_node) = unary_op {
        let (unary, unary_rest) = parse_unary(unary_op_rest);

        if let Some(unary_node) = unary {
            (
                Some(ParserNode::new(
                    vec![unary_op_node, unary_node],
                    ParserNodeType::Unary,
                )),
                unary_rest,
            )
        } else {
            handle_syntax_error(tokens, "Unary");
            unreachable!();
        }
    } else {
        let (factor, factor_rest) = parse_factor(tokens);
        (factor, factor_rest)
    };
}

/// UnaryOp -> + | - | ε
fn parse_unary_op<'a>(tokens: &'a [Token]) -> (Option<ParserNode>, &'a [Token<'a>]) {
    return match &tokens[0].token_type {
        TokenType::Plus => (
            Some(ParserNode::new(
                Vec::new(),
                ParserNodeType::UnaryOp(UnaryOp::Plus),
            )),
            &tokens[1..],
        ),
        TokenType::Minus => (
            Some(ParserNode::new(
                Vec::new(),
                ParserNodeType::UnaryOp(UnaryOp::Minus),
            )),
            &tokens[1..],
        ),

        _ => (None, &tokens),
    };
}

/// Factor -> NumericLiteral | ( expression ) | StringLiteral
fn parse_factor<'a>(tokens: &'a [Token]) -> (Option<ParserNode>, &'a [Token<'a>]) {
    if tokens.len() == 0 {
        handle_syntax_error(tokens, "Factor");
        unreachable!();
    }

    match &tokens[0].token_type {
        // NumericLiteral
        TokenType::NumLit => {
            let value = tokens[0].value;
            let numeric_literal = ParserNode::new(
                Vec::new(),
                ParserNodeType::NumericLiteral(MVal::from_string_no_sanitize(value.to_string())),
            );
            return (
                Some(ParserNode::new(
                    vec![numeric_literal],
                    ParserNodeType::Factor,
                )),
                &tokens[1..],
            );
        }

        // StringLiteral
        TokenType::StrLit => {
            let value = tokens[0].value;
            let string_literal = ParserNode::new(
                Vec::new(),
                ParserNodeType::StringLiteral(MVal::from_string(value.to_string())),
            );

            return (
                Some(ParserNode::new(
                    vec![string_literal],
                    ParserNodeType::Factor,
                )),
                &tokens[1..],
            );
        }

        // ( expression )
        TokenType::LParen => {
            let (expression, expression_rest) = parse_expression(&tokens[1..]);
            if let Some(expression_node) = expression {
                if expression_rest[0].token_type != TokenType::RParen {
                    handle_syntax_error(expression_rest, "Missing closing parenthesis");
                    unreachable!();
                }
                return (
                    Some(ParserNode::new(
                        vec![expression_node],
                        ParserNodeType::Expression,
                    )),
                    &expression_rest[1..],
                );
            } else {
                handle_syntax_error(tokens, "No expression inside parentheses");
                unreachable!();
            }
        }
        _ => {
            todo!();
            unreachable!();
        }
    }
}

fn handle_syntax_error(tokens: &[Token], expected: &str) {
    if tokens.len() == 0 {
        panic!("Expected {} found EOF instead", expected);
    } else {
        panic!("Expected {} found {} instead", expected, tokens[0]);
    }
}
