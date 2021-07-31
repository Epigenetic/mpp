/*
 * Copyright (c) 2021. Jonah Shafran
 *
 * SPDX-License-Identifier: GPL-3.0-only
 */

use crate::lexer::{ReservedToken, Token, TokenType};
use crate::parser::parse_node::{AddOp, MulOp, ParserNode, UnaryOp, WriteFormat};
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

        let (expression, expression_rest) = parse_write_statement(&self.tokens);
        if expression_rest.len() != 0 {
            println!("Warning: not all tokens processed.");
            for token in expression_rest {
                println!("{}", token);
            }
        }
        return expression;
    }
}

/// WriteStatement -> Write WriteExpressionList
fn parse_write_statement<'a>(tokens: &'a [Token]) -> (Option<ParserNode>, &'a [Token<'a>]) {
    match &tokens[0].token_type {
        TokenType::Reserved(ReservedToken::Write) => {
            let (write_expression_list, write_expression_list_rest) =
                parse_write_expression_list(&tokens[1..]);

            return if let Some(write_expression_list_node) = write_expression_list {
                (
                    Some(ParserNode::new(
                        vec![write_expression_list_node],
                        ParserNodeType::WriteStatement,
                    )),
                    write_expression_list_rest,
                )
            } else {
                handle_syntax_error(tokens, "WriteExpressionList");
                unreachable!();
            };
        }
        _ => {
            handle_syntax_error(tokens, "Write");
            unreachable!();
        }
    }
}

/// WriteExpressionList -> Expression WriteExpressionListTail
fn parse_write_expression_list<'a>(tokens: &'a [Token]) -> (Option<ParserNode>, &'a [Token<'a>]) {
    let (expression, expression_rest) = parse_write_expression(tokens);

    if let Some(expression_node) = expression {
        let (write_expression_list_tail, write_expression_list_tail_rest) =
            parse_write_expr_list_tail(expression_rest);

        if let Some(write_expression_list_tail_node) = write_expression_list_tail {
            (
                Some(ParserNode::new(
                    vec![expression_node, write_expression_list_tail_node],
                    ParserNodeType::WriteExpressionList,
                )),
                write_expression_list_tail_rest,
            )
        } else {
            (
                Some(ParserNode::new(
                    vec![expression_node],
                    ParserNodeType::WriteExpressionList,
                )),
                expression_rest,
            )
        }
    } else {
        handle_syntax_error(tokens, "Expression");
        unreachable!();
    }
}

/// Comma WriteExpression WriteExpressionListTail | ε
fn parse_write_expr_list_tail<'a>(tokens: &'a [Token]) -> (Option<ParserNode>, &'a [Token<'a>]) {
    if tokens.len() == 0 {
        return (None, tokens);
    }

    return match &tokens[0].token_type {
        TokenType::Comma => {
            let (expression, expression_rest) = parse_write_expression(&tokens[1..]);

            if let Some(expression_node) = expression {
                let (expr_list_tail, expr_list_tail_rest) =
                    parse_write_expr_list_tail(expression_rest);

                if let Some(expr_list_tail_node) = expr_list_tail {
                    (
                        Some(ParserNode::new(
                            vec![expression_node, expr_list_tail_node],
                            ParserNodeType::WriteExpressionListTail,
                        )),
                        expr_list_tail_rest,
                    )
                } else {
                    (
                        Some(ParserNode::new(
                            vec![expression_node],
                            ParserNodeType::WriteExpressionListTail,
                        )),
                        expression_rest,
                    )
                }
            } else {
                handle_syntax_error(&tokens[1..], "Expression");
                unreachable!();
            }
        }
        _ => (None, tokens),
    };
}

/// FormatExpression | Expression
fn parse_write_expression<'a>(tokens: &'a [Token]) -> (Option<ParserNode>, &'a [Token<'a>]) {
    if tokens.len() == 0 {
        return (None, tokens);
    }

    return match &tokens[0].token_type {
        TokenType::Hash | TokenType::Bang | TokenType::QuestionMark => {
            let (format_expression, format_expression_rest) = parse_format_expression(tokens);

            if let Some(format_expression_node) = format_expression {
                (
                    Some(ParserNode::new(
                        vec![format_expression_node],
                        ParserNodeType::WriteExpression,
                    )),
                    format_expression_rest,
                )
            } else {
                handle_syntax_error(tokens, "FormatExpression");
                unreachable!();
            }
        }
        _ => {
            let (expression, expression_rest) = parse_expression(tokens);

            return if let Some(expression_node) = expression {
                (
                    Some(ParserNode::new(
                        vec![expression_node],
                        ParserNodeType::WriteExpression,
                    )),
                    expression_rest,
                )
            } else {
                handle_syntax_error(tokens, "Expression");
                unreachable!()
            };
        }
    };
}

/// HashBangFormat FormatExpressionTail | ε
fn parse_format_expression<'a>(tokens: &'a [Token]) -> (Option<ParserNode>, &'a [Token<'a>]) {
    let (hash_bang_format, hash_bang_format_rest) = parse_hash_bang_format(tokens);

    if let Some(hash_bang_format_node) = hash_bang_format {
        let (format_expression_tail, format_expression_tail_rest) =
            parse_format_expression_tail(hash_bang_format_rest);

        if let Some(format_expression_tail_node) = format_expression_tail {
            (
                Some(ParserNode::new(
                    vec![hash_bang_format_node, format_expression_tail_node],
                    ParserNodeType::FormatExpression,
                )),
                format_expression_tail_rest,
            )
        } else {
            (
                Some(ParserNode::new(
                    vec![hash_bang_format_node],
                    ParserNodeType::FormatExpression,
                )),
                hash_bang_format_rest,
            )
        }
    } else {
        (None, tokens)
    }
}

/// ( ! | # ) HashBangFormat | ε
fn parse_hash_bang_format<'a>(tokens: &'a [Token]) -> (Option<ParserNode>, &'a [Token<'a>]) {
    if tokens.len() == 0 {
        return (None, tokens);
    }

    return match &tokens[0].token_type {
        TokenType::Bang => {
            let bang_node = ParserNode::new(
                Vec::new(),
                ParserNodeType::WriteFormat(WriteFormat::NewLine),
            );
            let (hash_bang_format, hash_bang_format_rest) = parse_hash_bang_format(&tokens[1..]);

            if let Some(hash_bang_format_node) = hash_bang_format {
                (
                    Some(ParserNode::new(
                        vec![bang_node, hash_bang_format_node],
                        ParserNodeType::HashBangFormat,
                    )),
                    hash_bang_format_rest,
                )
            } else {
                (
                    Some(ParserNode::new(
                        vec![bang_node],
                        ParserNodeType::HashBangFormat,
                    )),
                    &tokens[1..],
                )
            }
        }
        TokenType::Hash => {
            let hash_node = ParserNode::new(
                Vec::new(),
                ParserNodeType::WriteFormat(WriteFormat::ClearScreen),
            );
            let (hash_bang_format, hash_bang_format_rest) = parse_hash_bang_format(&tokens[1..]);

            if let Some(hash_bang_format_node) = hash_bang_format {
                (
                    Some(ParserNode::new(
                        vec![hash_node, hash_bang_format_node],
                        ParserNodeType::HashBangFormat,
                    )),
                    hash_bang_format_rest,
                )
            } else {
                (
                    Some(ParserNode::new(
                        vec![hash_node],
                        ParserNodeType::HashBangFormat,
                    )),
                    &tokens[1..],
                )
            }
        }
        _ => (None, tokens),
    };
}

///  ? Expression | ε
fn parse_format_expression_tail<'a>(tokens: &'a [Token]) -> (Option<ParserNode>, &'a [Token<'a>]) {
    if tokens.len() == 0 {
        return (None, tokens);
    }

    return match &tokens[0].token_type {
        TokenType::QuestionMark => {
            let (expression, expression_rest) = parse_expression(&tokens[1..]);

            if let Some(expression_node) = expression {
                let write_to_col_node = ParserNode::new(
                    vec![expression_node],
                    ParserNodeType::WriteFormat(WriteFormat::ToCol),
                );

                (
                    Some(ParserNode::new(
                        vec![write_to_col_node],
                        ParserNodeType::FormatExpressionTail,
                    )),
                    expression_rest,
                )
            } else {
                handle_syntax_error(tokens, "Expression");
                unreachable!();
            }
        }
        _ => (None, tokens),
    };
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
        handle_syntax_error(&unary_rest, "Unary");
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
        TokenType::Hash => (
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
        let (exponential_term, exponential_term_rest) = parse_exponential_term(tokens);
        (exponential_term, exponential_term_rest)
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

/// ExponentialTerm -> Factor ExponentialTermTail
fn parse_exponential_term<'a>(tokens: &'a [Token]) -> (Option<ParserNode>, &'a [Token<'a>]) {
    let (term, term_rest) = parse_factor(tokens);

    if let Some(term_node) = term {
        let (exponential_term_tail, exponential_term_tail_rest) =
            parse_exponential_term_tail(term_rest);

        if let Some(exponential_term_tail_node) = exponential_term_tail {
            (
                Some(ParserNode::new(
                    vec![term_node, exponential_term_tail_node],
                    ParserNodeType::ExpTerm,
                )),
                exponential_term_tail_rest,
            )
        } else {
            (
                Some(ParserNode::new(vec![term_node], ParserNodeType::ExpTerm)),
                term_rest,
            )
        }
    } else {
        handle_syntax_error(tokens, "Term");
        unreachable!();
    }
}

/// ExponentialTermTail -> Exponential Factor ExponentialTermTail | ε
fn parse_exponential_term_tail<'a>(tokens: &'a [Token]) -> (Option<ParserNode>, &'a [Token<'a>]) {
    let (exponent, exponent_rest) = parse_exponential(tokens);

    return if let Some(exponent_node) = exponent {
        let (factor, factor_rest) = parse_factor(exponent_rest);

        if let Some(factor_node) = factor {
            let (exponential_term_tail, exponential_term_tail_rest) =
                parse_exponential_term_tail(factor_rest);

            if let Some(exponential_term_tail_node) = exponential_term_tail {
                (
                    Some(ParserNode::new(
                        vec![exponent_node, factor_node, exponential_term_tail_node],
                        ParserNodeType::ExpTermTail,
                    )),
                    exponential_term_tail_rest,
                )
            } else {
                (
                    Some(ParserNode::new(
                        vec![exponent_node, factor_node],
                        ParserNodeType::ExpTermTail,
                    )),
                    factor_rest,
                )
            }
        } else {
            handle_syntax_error(tokens, "Factor");
            unreachable!();
        }
    } else {
        (None, tokens)
    };
}

/// ** | ε
fn parse_exponential<'a>(tokens: &'a [Token]) -> (Option<ParserNode>, &'a [Token<'a>]) {
    if tokens.len() == 0 {
        return (None, tokens);
    }

    return match &tokens[0].token_type {
        TokenType::Power => (
            Some(ParserNode::new(Vec::new(), ParserNodeType::ExpOp)),
            &tokens[1..],
        ),
        _ => (None, tokens),
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
