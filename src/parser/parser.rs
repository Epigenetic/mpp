/*
 * Copyright (c) 2021. Jonah Shafran
 *
 * SPDX-License-Identifier: GPL-3.0-only
 */

use crate::lexer::{ReservedToken, Token, TokenType};
use crate::parser::parse_node::{AddOp, MulOp, ParserNode, RelOp, UnaryOp, WriteFormat};
use crate::parser::ParserNodeType;
use crate::runtime::MVal;

pub struct Parser<'a> {
    tokens: Vec<Token<'a>>,
}

impl Parser<'_> {
    pub fn new(tokens: Vec<Token>) -> Parser {
        Parser { tokens }
    }

    pub fn parse<'a>(&self) -> Result<Option<ParserNode>, ParseError> {
        if self.tokens.len() == 0 {
            return Ok(None);
        }

        let (block, line_rest) = parse_block(&self.tokens)?;
        if line_rest.len() != 0 {
            println!("Warning: not all tokens processed.");
            for token in line_rest {
                println!("{}", token);
            }
        }
        return Ok(block);
    }
}

pub fn print_parse_error(input: String, error: ParseError) {
    let lines: Vec<&str> = input.lines().collect();

    if error.remaining_tokens.len() == 0
        || error.remaining_tokens[0].token_type == TokenType::NewLine
    {
        let last_line = lines[lines.len() - 1];
        eprintln!("{}:{}", lines.len() - 1, last_line.len() - 1);
        eprintln!("{}", last_line);
        for _ in 0..last_line.len() - 1 {
            eprint!(" ");
        }
        eprintln!("^ Unexpected end of line. {}", error.message)
    } else {
        let bad_token = &error.remaining_tokens[0];
        eprintln!("{}:{}", bad_token.line, bad_token.start);
        let bad_line = lines[bad_token.line];
        eprintln!("{}", bad_line);
        for _ in 0..bad_token.start {
            eprint!(" ");
        }
        for _ in bad_token.start..bad_token.end {
            eprint!("^");
        }
        eprintln!(" {}", error.message)
    }
}

/// Block -> Lines | ε
fn parse_block<'a>(
    tokens: &'a [Token],
) -> Result<(Option<ParserNode<'a>>, &'a [Token<'a>]), ParseError<'a>> {
    let (lines, lines_rest) = parse_lines(tokens)?;

    if let Some(lines_node) = lines {
        Ok((
            Some(ParserNode::new(
                vec![lines_node],
                ParserNodeType::Block(None),
            )),
            lines_rest,
        ))
    } else {
        Ok((None, lines_rest))
    }
}

/// Lines -> Line Lines | ε
fn parse_lines<'a>(
    tokens: &'a [Token],
) -> Result<(Option<ParserNode<'a>>, &'a [Token<'a>]), ParseError<'a>> {
    if tokens.len() == 0 {
        return Ok((None, tokens));
    }

    let (line, line_rest) = parse_line(tokens)?;

    return if let Some(line_node) = line {
        let (lines, lines_rest) = parse_lines(line_rest)?;

        if let Some(lines_node) = lines {
            Ok((
                Some(ParserNode::new(
                    vec![line_node, lines_node],
                    ParserNodeType::Lines,
                )),
                lines_rest,
            ))
        } else {
            Ok((
                Some(ParserNode::new(vec![line_node], ParserNodeType::Lines)),
                line_rest,
            ))
        }
    } else {
        Ok((None, line_rest))
    };
}

/// Line -> Statements \n
fn parse_line<'a>(
    tokens: &'a [Token],
) -> Result<(Option<ParserNode<'a>>, &'a [Token<'a>]), ParseError<'a>> {
    let (statement, statement_rest) = parse_statement(tokens)?;

    return if let Some(statement_node) = statement {
        let (statements, statements_rest) = parse_statements(statement_rest)?;

        if let Some(statements_node) = statements {
            if statements_rest.len() == 0 || statements_rest[0].token_type != TokenType::NewLine {
                Err(ParseError {
                    remaining_tokens: statements_rest,
                    message: "Expecting end of line.",
                })
            } else {
                Ok((
                    Some(ParserNode::new(
                        vec![statement_node, statements_node],
                        ParserNodeType::Line,
                    )),
                    &statements_rest[1..],
                ))
            }
        } else {
            if statement_rest.len() == 0 || statement_rest[0].token_type != TokenType::NewLine {
                Err(ParseError {
                    remaining_tokens: statement_rest,
                    message: "Expecting end of line.",
                })
            } else {
                Ok((
                    Some(ParserNode::new(vec![statement_node], ParserNodeType::Line)),
                    &statement_rest[1..],
                ))
            }
        }
    } else {
        if statement_rest.len() == 0 || statement_rest[0].token_type != TokenType::NewLine {
            Err(ParseError {
                remaining_tokens: statement_rest,
                message: "Expecting end of line.",
            })
        } else {
            Ok((None, &tokens[1..]))
        }
    };
}

/// Statements -> Statement Statements | ε
fn parse_statements<'a>(
    tokens: &'a [Token],
) -> Result<(Option<ParserNode<'a>>, &'a [Token<'a>]), ParseError<'a>> {
    let (statement, statement_rest) = parse_statement(tokens)?;

    return if let Some(statement_node) = statement {
        let (statements, statements_rest) = parse_statements(statement_rest)?;

        if let Some(statements_node) = statements {
            Ok((
                Some(ParserNode::new(
                    vec![statement_node, statements_node],
                    ParserNodeType::Statements,
                )),
                statements_rest,
            ))
        } else {
            Ok((
                Some(ParserNode::new(
                    vec![statement_node],
                    ParserNodeType::Statements,
                )),
                statement_rest,
            ))
        }
    } else {
        Ok((None, tokens))
    };
}

/// Statement -> WriteStatement | SetStatement | NewStatement | ε
fn parse_statement<'a>(
    tokens: &'a [Token],
) -> Result<(Option<ParserNode<'a>>, &'a [Token<'a>]), ParseError<'a>> {
    if tokens.len() == 0 {
        return Ok((None, tokens));
    }

    return match &tokens[0].token_type {
        TokenType::Reserved(ReservedToken::New) => {
            let (new_statement, new_statement_rest) = parse_new_statement(tokens)?;

            if let Some(new_statement_node) = new_statement {
                Ok((
                    Some(ParserNode::new(
                        vec![new_statement_node],
                        ParserNodeType::Statement,
                    )),
                    new_statement_rest,
                ))
            } else {
                unreachable!("NewStatement does not go to epsilon")
            }
        }
        TokenType::Reserved(ReservedToken::Set) => {
            let (set_statement, set_statement_rest) = parse_set_statement(tokens)?;

            if let Some(set_statement_node) = set_statement {
                Ok((
                    Some(ParserNode::new(
                        vec![set_statement_node],
                        ParserNodeType::Statement,
                    )),
                    set_statement_rest,
                ))
            } else {
                unreachable!("SetStatement does not go to epsilon")
            }
        }
        TokenType::Reserved(ReservedToken::Write) => {
            let (write_statement, write_statement_rest) = parse_write_statement(tokens)?;

            if let Some(write_statement_node) = write_statement {
                Ok((
                    Some(ParserNode::new(
                        vec![write_statement_node],
                        ParserNodeType::Statement,
                    )),
                    write_statement_rest,
                ))
            } else {
                unreachable!("WriteStatement does not go to epsilon")
            }
        }
        TokenType::NewLine => Ok((None, tokens)),
        _ => Err(ParseError {
            remaining_tokens: tokens,
            message: "Illegal start of statement.",
        }),
    };
}

/// NewStatement -> New IdentifierList
fn parse_new_statement<'a>(
    tokens: &'a [Token],
) -> Result<(Option<ParserNode<'a>>, &'a [Token<'a>]), ParseError<'a>> {
    return match &tokens[0].token_type {
        TokenType::Reserved(ReservedToken::New) => {
            let (identifier_list, identifier_list_rest) = parse_identifier_list(&tokens[1..])?;

            if let Some(identifier_list_node) = identifier_list {
                Ok((
                    Some(ParserNode::new(
                        vec![identifier_list_node],
                        ParserNodeType::NewStatement,
                    )),
                    identifier_list_rest,
                ))
            } else {
                unreachable!("IdentifierList does not go to epsilon")
            }
        }
        _ => Err(ParseError {
            remaining_tokens: tokens,
            message: "Expected new.",
        }),
    };
}

/// IdentifierList -> Identifier IdentifierListTail
fn parse_identifier_list<'a>(
    tokens: &'a [Token],
) -> Result<(Option<ParserNode<'a>>, &'a [Token<'a>]), ParseError<'a>> {
    if tokens.len() == 0 {
        return Err(ParseError {
            remaining_tokens: tokens,
            message: "Expecting identifier.",
        });
    }

    return match &tokens[0].token_type {
        TokenType::Identifier => {
            let identifier_node =
                ParserNode::new(Vec::new(), ParserNodeType::Identifier(tokens[0].value));
            let (identifier_list_tail, identifier_list_tail_rest) =
                parse_identifier_list_tail(&tokens[1..])?;

            if let Some(identifier_list_tail_node) = identifier_list_tail {
                Ok((
                    Some(ParserNode::new(
                        vec![identifier_node, identifier_list_tail_node],
                        ParserNodeType::IdentifierList,
                    )),
                    identifier_list_tail_rest,
                ))
            } else {
                Ok((
                    Some(ParserNode::new(
                        vec![identifier_node],
                        ParserNodeType::IdentifierList,
                    )),
                    &tokens[1..],
                ))
            }
        }
        _ => Err(ParseError {
            remaining_tokens: tokens,
            message: "Expecting identifier.",
        }),
    };
}

/// IdentifierListTail -> , Identifier IdentifierListTail | ε
fn parse_identifier_list_tail<'a>(
    tokens: &'a [Token],
) -> Result<(Option<ParserNode<'a>>, &'a [Token<'a>]), ParseError<'a>> {
    // Not long enough to have another item
    if tokens.len() < 2 {
        return Ok((None, tokens));
    }

    return match &tokens[0].token_type {
        TokenType::Comma => match &tokens[1].token_type {
            TokenType::Identifier => {
                let identifier_node =
                    ParserNode::new(Vec::new(), ParserNodeType::Identifier(tokens[1].value));
                let (identifier_list_tail, identifier_list_tail_rest) =
                    parse_identifier_list_tail(&tokens[2..])?;

                if let Some(identifier_list_tail_node) = identifier_list_tail {
                    Ok((
                        Some(ParserNode::new(
                            vec![identifier_node, identifier_list_tail_node],
                            ParserNodeType::IdentifierListTail,
                        )),
                        identifier_list_tail_rest,
                    ))
                } else {
                    Ok((
                        Some(ParserNode::new(
                            vec![identifier_node],
                            ParserNodeType::IdentifierListTail,
                        )),
                        &tokens[2..],
                    ))
                }
            }
            _ => Err(ParseError {
                remaining_tokens: &tokens[1..],
                message: "Missing variable name.",
            }),
        },
        _ => Ok((None, tokens)),
    };
}

/// SetStatement -> Set AssignmentList
fn parse_set_statement<'a>(
    tokens: &'a [Token],
) -> Result<(Option<ParserNode<'a>>, &'a [Token<'a>]), ParseError<'a>> {
    return match &tokens[0].token_type {
        TokenType::Reserved(ReservedToken::Set) => {
            let (assignment_list, assignment_list_rest) = parse_assignment_list(&tokens[1..])?;

            if let Some(assignment_list_node) = assignment_list {
                Ok((
                    Some(ParserNode::new(
                        vec![assignment_list_node],
                        ParserNodeType::SetStatement,
                    )),
                    assignment_list_rest,
                ))
            } else {
                unreachable!("AssignmentList does not go to epsilon")
            }
        }
        _ => Err(ParseError {
            remaining_tokens: tokens,
            message: "Expected set.",
        }),
    };
}

/// AssignmentList -> AssignmentStatement AssignmentListTail
fn parse_assignment_list<'a>(
    tokens: &'a [Token],
) -> Result<(Option<ParserNode<'a>>, &'a [Token<'a>]), ParseError<'a>> {
    let (assignment_statement, assignment_statement_rest) = parse_assignment_statement(tokens)?;

    return if let Some(assignment_statement_node) = assignment_statement {
        let (assignment_list_tail, assignment_list_tail_rest) =
            parse_assignment_list_tail(assignment_statement_rest)?;

        if let Some(set_list_tail_node) = assignment_list_tail {
            Ok((
                Some(ParserNode::new(
                    vec![assignment_statement_node, set_list_tail_node],
                    ParserNodeType::AssignmentList,
                )),
                assignment_list_tail_rest,
            ))
        } else {
            Ok((
                Some(ParserNode::new(
                    vec![assignment_statement_node],
                    ParserNodeType::AssignmentList,
                )),
                assignment_statement_rest,
            ))
        }
    } else {
        unreachable!("RelationalExpression does not go to epsilon")
    };
}

/// AssignmentListTail -> , AssignmentStatement AssignmentListTail | ε
fn parse_assignment_list_tail<'a>(
    tokens: &'a [Token],
) -> Result<(Option<ParserNode<'a>>, &'a [Token<'a>]), ParseError<'a>> {
    if tokens.len() == 0 {
        return Ok((None, tokens));
    }

    return match &tokens[0].token_type {
        TokenType::Comma => {
            let (assignment_statement, assignment_statement_rest) =
                parse_assignment_statement(&tokens[1..])?;

            return if let Some(assignment_statement_node) = assignment_statement {
                let (assignment_list_tail, assignment_list_tail_rest) =
                    parse_assignment_list_tail(assignment_statement_rest)?;

                if let Some(assignment_list_tail_node) = assignment_list_tail {
                    Ok((
                        Some(ParserNode::new(
                            vec![assignment_statement_node, assignment_list_tail_node],
                            ParserNodeType::AssignmentListTail,
                        )),
                        assignment_list_tail_rest,
                    ))
                } else {
                    Ok((
                        Some(ParserNode::new(
                            vec![assignment_statement_node],
                            ParserNodeType::AssignmentListTail,
                        )),
                        assignment_statement_rest,
                    ))
                }
            } else {
                Ok((None, tokens))
            };
        }
        _ => Ok((None, tokens)),
    };
}

/// AssignmentStatement -> Identifier = RelationalExpression | ε
fn parse_assignment_statement<'a>(
    tokens: &'a [Token],
) -> Result<(Option<ParserNode<'a>>, &'a [Token<'a>]), ParseError<'a>> {
    if tokens.len() < 3 {
        return Ok((None, tokens));
    }

    return match &tokens[0].token_type {
        TokenType::Identifier => {
            let identifier_node =
                ParserNode::new(Vec::new(), ParserNodeType::Identifier(tokens[0].value));

            match &tokens[1].token_type {
                TokenType::Equals => {
                    let (relational_expression, relational_expression_rest) =
                        parse_relational_expression(&tokens[2..])?;

                    if let Some(relational_expression_node) = relational_expression {
                        Ok((
                            Some(ParserNode::new(
                                vec![identifier_node, relational_expression_node],
                                ParserNodeType::AssignmentStatement,
                            )),
                            relational_expression_rest,
                        ))
                    } else {
                        unreachable!("RelationalExpression does not go to epsilon")
                    }
                }
                _ => Err(ParseError {
                    remaining_tokens: &tokens[1..],
                    message: "Expected equals.",
                }),
            }
        }
        _ => Ok((None, tokens)),
    };
}

/// WriteStatement -> Write WriteExpressionList
fn parse_write_statement<'a>(
    tokens: &'a [Token],
) -> Result<(Option<ParserNode<'a>>, &'a [Token<'a>]), ParseError<'a>> {
    return match &tokens[0].token_type {
        TokenType::Reserved(ReservedToken::Write) => {
            let (write_expression_list, write_expression_list_rest) =
                parse_write_expression_list(&tokens[1..])?;

            if let Some(write_expression_list_node) = write_expression_list {
                Ok((
                    Some(ParserNode::new(
                        vec![write_expression_list_node],
                        ParserNodeType::WriteStatement,
                    )),
                    write_expression_list_rest,
                ))
            } else {
                unreachable!("WriteExpressionList cannot go to epsilon");
            }
        }
        _ => Err(ParseError {
            remaining_tokens: tokens,
            message: "Expected write.",
        }),
    };
}

/// WriteExpressionList -> WriteExpression WriteExpressionListTail
fn parse_write_expression_list<'a>(
    tokens: &'a [Token],
) -> Result<(Option<ParserNode<'a>>, &'a [Token<'a>]), ParseError<'a>> {
    let (expression, expression_rest) = parse_write_expression(tokens)?;

    if let Some(expression_node) = expression {
        let (write_expression_list_tail, write_expression_list_tail_rest) =
            parse_write_expr_list_tail(expression_rest)?;

        if let Some(write_expression_list_tail_node) = write_expression_list_tail {
            Ok((
                Some(ParserNode::new(
                    vec![expression_node, write_expression_list_tail_node],
                    ParserNodeType::WriteExpressionList,
                )),
                write_expression_list_tail_rest,
            ))
        } else {
            Ok((
                Some(ParserNode::new(
                    vec![expression_node],
                    ParserNodeType::WriteExpressionList,
                )),
                expression_rest,
            ))
        }
    } else {
        return Err(ParseError {
            remaining_tokens: tokens,
            message: "Operand for write expected.",
        });
    }
}

/// Comma WriteExpression WriteExpressionListTail | ε
fn parse_write_expr_list_tail<'a>(
    tokens: &'a [Token],
) -> Result<(Option<ParserNode<'a>>, &'a [Token<'a>]), ParseError<'a>> {
    if tokens.len() == 0 {
        return Ok((None, tokens));
    }

    return match &tokens[0].token_type {
        TokenType::Comma => {
            let (write_expression, write_expression_rest) = parse_write_expression(&tokens[1..])?;

            if let Some(write_expression_node) = write_expression {
                let (expr_list_tail, expr_list_tail_rest) =
                    parse_write_expr_list_tail(write_expression_rest)?;

                if let Some(expr_list_tail_node) = expr_list_tail {
                    Ok((
                        Some(ParserNode::new(
                            vec![write_expression_node, expr_list_tail_node],
                            ParserNodeType::WriteExpressionListTail,
                        )),
                        expr_list_tail_rest,
                    ))
                } else {
                    Ok((
                        Some(ParserNode::new(
                            vec![write_expression_node],
                            ParserNodeType::WriteExpressionListTail,
                        )),
                        write_expression_rest,
                    ))
                }
            } else {
                Err(ParseError {
                    remaining_tokens: tokens,
                    message: "Operand for write expected.",
                })
            }
        }
        _ => Ok((None, tokens)),
    };
}

/// FormatExpression | RelationalExpression | ε
fn parse_write_expression<'a>(
    tokens: &'a [Token],
) -> Result<(Option<ParserNode<'a>>, &'a [Token<'a>]), ParseError<'a>> {
    if tokens.len() == 0 || tokens[0].token_type == TokenType::NewLine {
        return Ok((None, tokens));
    }

    return match &tokens[0].token_type {
        TokenType::Hash | TokenType::Bang | TokenType::QuestionMark => {
            let (format_expression, format_expression_rest) = parse_format_expression(tokens)?;

            if let Some(format_expression_node) = format_expression {
                Ok((
                    Some(ParserNode::new(
                        vec![format_expression_node],
                        ParserNodeType::WriteExpression,
                    )),
                    format_expression_rest,
                ))
            } else {
                unreachable!("Hash and Bang should be parsed successfully via our checks, ? Expression failure should be handled further downstream")
            }
        }
        _ => {
            let (relational_expression, relational_expression_rest) =
                parse_relational_expression(tokens)?;

            return if let Some(expression_node) = relational_expression {
                Ok((
                    Some(ParserNode::new(
                        vec![expression_node],
                        ParserNodeType::WriteExpression,
                    )),
                    relational_expression_rest,
                ))
            } else {
                unreachable!("Expression cannot go to epsilon")
            };
        }
    };
}

/// HashBangFormat FormatExpressionTail | ε
fn parse_format_expression<'a>(
    tokens: &'a [Token],
) -> Result<(Option<ParserNode<'a>>, &'a [Token<'a>]), ParseError<'a>> {
    let (hash_bang_format, hash_bang_format_rest) = parse_hash_bang_format(tokens);

    if let Some(hash_bang_format_node) = hash_bang_format {
        let (format_expression_tail, format_expression_tail_rest) =
            parse_format_expression_tail(hash_bang_format_rest)?;

        if let Some(format_expression_tail_node) = format_expression_tail {
            Ok((
                Some(ParserNode::new(
                    vec![hash_bang_format_node, format_expression_tail_node],
                    ParserNodeType::FormatExpression,
                )),
                format_expression_tail_rest,
            ))
        } else {
            Ok((
                Some(ParserNode::new(
                    vec![hash_bang_format_node],
                    ParserNodeType::FormatExpression,
                )),
                hash_bang_format_rest,
            ))
        }
    } else {
        Ok((None, tokens))
    }
}

/// ( ! | # ) HashBangFormat | ε
fn parse_hash_bang_format<'a>(tokens: &'a [Token]) -> (Option<ParserNode<'a>>, &'a [Token<'a>]) {
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

///  ? RelationalExpression | ε
fn parse_format_expression_tail<'a>(
    tokens: &'a [Token],
) -> Result<(Option<ParserNode<'a>>, &'a [Token<'a>]), ParseError<'a>> {
    if tokens.len() == 0 {
        return Ok((None, tokens));
    }

    return match &tokens[0].token_type {
        TokenType::QuestionMark => {
            let (relational_expression, relational_expression_rest) =
                parse_relational_expression(&tokens[1..])?;

            if let Some(expression_node) = relational_expression {
                let write_to_col_node = ParserNode::new(
                    vec![expression_node],
                    ParserNodeType::WriteFormat(WriteFormat::ToCol),
                );

                Ok((
                    Some(ParserNode::new(
                        vec![write_to_col_node],
                        ParserNodeType::FormatExpressionTail,
                    )),
                    relational_expression_rest,
                ))
            } else {
                unreachable!("Expression can not go to epsilon");
            }
        }
        _ => Ok((None, tokens)),
    };
}

/// RelationalExpression -> Expression RelationalExpressionTail
fn parse_relational_expression<'a>(
    tokens: &'a [Token],
) -> Result<(Option<ParserNode<'a>>, &'a [Token<'a>]), ParseError<'a>> {
    let (expression, expression_rest) = parse_expression(tokens)?;

    return if let Some(expression_node) = expression {
        let (relational_expression_tail, relational_expression_rest) =
            parse_relational_expression_tail(expression_rest)?;

        if let Some(relational_expression_tail_node) = relational_expression_tail {
            Ok((
                Some(ParserNode::new(
                    vec![expression_node, relational_expression_tail_node],
                    ParserNodeType::RelationalExpression,
                )),
                relational_expression_rest,
            ))
        } else {
            Ok((
                Some(ParserNode::new(
                    vec![expression_node],
                    ParserNodeType::RelationalExpression,
                )),
                expression_rest,
            ))
        }
    } else {
        unreachable!("Expression cannot go to epsilon")
    };
}

/// RelationalExpressionTail -> RelOp RelationalExpressionTail | ε
fn parse_relational_expression_tail<'a>(
    tokens: &'a [Token],
) -> Result<(Option<ParserNode<'a>>, &'a [Token<'a>]), ParseError<'a>> {
    let (rel_op, rel_op_rest) = parse_rel_op(tokens)?;

    return if let Some(rel_op_node) = rel_op {
        let (expression, expression_rest) = parse_expression(rel_op_rest)?;

        if let Some(expression_node) = expression {
            Ok((
                Some(ParserNode::new(
                    vec![rel_op_node, expression_node],
                    ParserNodeType::RelationalExpressionTail,
                )),
                expression_rest,
            ))
        } else {
            unreachable!("Expression cannot go to epsilon")
        }
    } else {
        return Ok((None, tokens));
    };
}

/// RelOp -> > | < | >= | <= | ε
fn parse_rel_op<'a>(
    tokens: &'a [Token],
) -> Result<(Option<ParserNode<'a>>, &'a [Token<'a>]), ParseError<'a>> {
    if tokens.len() == 0 {
        return Ok((None, tokens));
    }

    return match tokens[0].token_type {
        TokenType::GreaterThan => Ok((
            Some(ParserNode::new(
                Vec::new(),
                ParserNodeType::RelOp(RelOp::GreaterThan),
            )),
            &tokens[1..],
        )),
        TokenType::LessThan => Ok((
            Some(ParserNode::new(
                Vec::new(),
                ParserNodeType::RelOp(RelOp::LessThan),
            )),
            &tokens[1..],
        )),
        TokenType::GreaterThanOrEqualTo => Ok((
            Some(ParserNode::new(
                Vec::new(),
                ParserNodeType::RelOp(RelOp::GreaterThanOrEqualTo),
            )),
            &tokens[1..],
        )),
        TokenType::LessThanOrEqualTo => Ok((
            Some(ParserNode::new(
                Vec::new(),
                ParserNodeType::RelOp(RelOp::LessThanOrEqualTo),
            )),
            &tokens[1..],
        )),
        _ => Ok((None, tokens)),
    };
}

/// Expression -> Term ExpressionTail
fn parse_expression<'a>(
    tokens: &'a [Token],
) -> Result<(Option<ParserNode<'a>>, &'a [Token<'a>]), ParseError<'a>> {
    let (term, term_rest) = parse_term(tokens)?;

    if let Some(term_node) = term {
        let (expression_tail, expression_tail_rest) = parse_expression_tail(term_rest)?;

        return if let Some(expression_tail_node) = expression_tail {
            Ok((
                Some(ParserNode::new(
                    vec![term_node, expression_tail_node],
                    ParserNodeType::Expression,
                )),
                expression_tail_rest,
            ))
        } else {
            Ok((
                Some(ParserNode::new(vec![term_node], ParserNodeType::Expression)),
                term_rest,
            ))
        };
    } else {
        unreachable!("Term cannot go to epsilon");
    }
}

/// ExpressionTail -> ε | AddOp Term ExpressionTail
fn parse_expression_tail<'a>(
    tokens: &'a [Token],
) -> Result<(Option<ParserNode<'a>>, &'a [Token<'a>]), ParseError<'a>> {
    let (add_op, add_op_rest) = parse_add_op(tokens);

    if let Some(add_op_node) = add_op {
        let (term, term_rest) = parse_term(add_op_rest)?;

        if let Some(term_node) = term {
            let (expression_tail, expression_tail_rest) = parse_expression_tail(term_rest)?;

            return if let Some(expression_tail_node) = expression_tail {
                Ok((
                    Some(ParserNode::new(
                        vec![add_op_node, term_node, expression_tail_node],
                        ParserNodeType::ExpressionTail,
                    )),
                    expression_tail_rest,
                ))
            } else {
                Ok((
                    Some(ParserNode::new(
                        vec![add_op_node, term_node],
                        ParserNodeType::ExpressionTail,
                    )),
                    expression_tail_rest,
                ))
            };
        } else {
            unreachable!("Term cannot go to epsilon")
        }
    } else {
        return Ok((None, tokens));
    }
}

/// AddOp -> + | -
fn parse_add_op<'a>(tokens: &'a [Token]) -> (Option<ParserNode<'a>>, &'a [Token<'a>]) {
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
fn parse_term<'a>(
    tokens: &'a [Token],
) -> Result<(Option<ParserNode<'a>>, &'a [Token<'a>]), ParseError<'a>> {
    let (unary, unary_rest) = parse_unary(tokens)?;

    if let Some(unary_node) = unary {
        let (term_tail, term_tail_rest) = parse_term_tail(unary_rest)?;

        return if let Some(term_tail_node) = term_tail {
            Ok((
                Some(ParserNode::new(
                    vec![unary_node, term_tail_node],
                    ParserNodeType::Term,
                )),
                term_tail_rest,
            ))
        } else {
            Ok((
                Some(ParserNode::new(vec![unary_node], ParserNodeType::Term)),
                unary_rest,
            ))
        };
    } else {
        unreachable!("Unary cannot go to epsilon");
    }
}

/// TermTail -> ε | MulOp Unary TermTail
fn parse_term_tail<'a>(
    tokens: &'a [Token],
) -> Result<(Option<ParserNode<'a>>, &'a [Token<'a>]), ParseError<'a>> {
    let (mul_op, mul_op_rest) = parse_mul_op(tokens);

    return if let Some(mul_node) = mul_op {
        let (unary, unary_rest) = parse_unary(mul_op_rest)?;

        if let Some(unary_node) = unary {
            let (term_tail, term_tail_rest) = parse_term_tail(unary_rest)?;

            if let Some(term_tail_node) = term_tail {
                let parse_node = ParserNode::new(
                    vec![mul_node, unary_node, term_tail_node],
                    ParserNodeType::TermTail,
                );
                Ok((Some(parse_node), term_tail_rest))
            } else {
                Ok((
                    Some(ParserNode::new(
                        vec![mul_node, unary_node],
                        ParserNodeType::TermTail,
                    )),
                    term_tail_rest,
                ))
            }
        } else {
            unreachable!("Unary cannot go to epsilon")
        }
    } else {
        Ok((None, &tokens))
    };
}

/// MulOp -> * | / | # | \ | ε
fn parse_mul_op<'a>(tokens: &'a [Token]) -> (Option<ParserNode<'a>>, &'a [Token<'a>]) {
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

/// Unary -> UnaryOp Unary | ExponentialTerm
fn parse_unary<'a>(
    tokens: &'a [Token],
) -> Result<(Option<ParserNode<'a>>, &'a [Token<'a>]), ParseError<'a>> {
    let (unary_op, unary_op_rest) = parse_unary_op(tokens);

    return if let Some(unary_op_node) = unary_op {
        let (unary, unary_rest) = parse_unary(unary_op_rest)?;

        if let Some(unary_node) = unary {
            Ok((
                Some(ParserNode::new(
                    vec![unary_op_node, unary_node],
                    ParserNodeType::Unary,
                )),
                unary_rest,
            ))
        } else {
            unreachable!("Unary cannot go to epsilon")
        }
    } else {
        let (exponential_term, exponential_term_rest) = parse_exponential_term(tokens)?;
        Ok((exponential_term, exponential_term_rest))
    };
}

/// UnaryOp -> + | - | ' | ε
fn parse_unary_op<'a>(tokens: &'a [Token]) -> (Option<ParserNode<'a>>, &'a [Token<'a>]) {
    if tokens.len() == 0 {
        return (None, tokens);
    }

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
        TokenType::Not => (
            Some(ParserNode::new(
                Vec::new(),
                ParserNodeType::UnaryOp(UnaryOp::Not),
            )),
            &tokens[1..],
        ),

        _ => (None, &tokens),
    };
}

/// ExponentialTerm -> Factor ExponentialTermTail
fn parse_exponential_term<'a>(
    tokens: &'a [Token],
) -> Result<(Option<ParserNode<'a>>, &'a [Token<'a>]), ParseError<'a>> {
    let (factor, factor_rest) = parse_factor(tokens)?;

    if let Some(term_node) = factor {
        let (exponential_term_tail, exponential_term_tail_rest) =
            parse_exponential_term_tail(factor_rest)?;

        if let Some(exponential_term_tail_node) = exponential_term_tail {
            Ok((
                Some(ParserNode::new(
                    vec![term_node, exponential_term_tail_node],
                    ParserNodeType::ExpTerm,
                )),
                exponential_term_tail_rest,
            ))
        } else {
            Ok((
                Some(ParserNode::new(vec![term_node], ParserNodeType::ExpTerm)),
                factor_rest,
            ))
        }
    } else {
        unreachable!("Factor cannot go to epsilon")
    }
}

/// ExponentialTermTail -> Exponential Factor ExponentialTermTail | ε
fn parse_exponential_term_tail<'a>(
    tokens: &'a [Token],
) -> Result<(Option<ParserNode<'a>>, &'a [Token<'a>]), ParseError<'a>> {
    let (exponent, exponent_rest) = parse_exponential(tokens);

    return if let Some(exponent_node) = exponent {
        let (factor, factor_rest) = parse_factor(exponent_rest)?;

        if let Some(factor_node) = factor {
            let (exponential_term_tail, exponential_term_tail_rest) =
                parse_exponential_term_tail(factor_rest)?;

            if let Some(exponential_term_tail_node) = exponential_term_tail {
                Ok((
                    Some(ParserNode::new(
                        vec![exponent_node, factor_node, exponential_term_tail_node],
                        ParserNodeType::ExpTermTail,
                    )),
                    exponential_term_tail_rest,
                ))
            } else {
                Ok((
                    Some(ParserNode::new(
                        vec![exponent_node, factor_node],
                        ParserNodeType::ExpTermTail,
                    )),
                    factor_rest,
                ))
            }
        } else {
            unreachable!("Factor cannot go to epsilon");
        }
    } else {
        Ok((None, tokens))
    };
}

/// ** | ε
fn parse_exponential<'a>(tokens: &'a [Token]) -> (Option<ParserNode<'a>>, &'a [Token<'a>]) {
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

/// Factor -> NumericLiteral | ( expression ) | StringLiteral | Identifier
fn parse_factor<'a>(
    tokens: &'a [Token],
) -> Result<(Option<ParserNode<'a>>, &'a [Token<'a>]), ParseError<'a>> {
    if tokens.len() == 0 {
        return Err(ParseError {
            remaining_tokens: tokens,
            message: "Missing operand.",
        });
    } else if tokens[0].token_type == TokenType::NewLine {
        return Err(ParseError {
            remaining_tokens: &tokens[1..],
            message: "Missing operand.",
        });
    }

    match &tokens[0].token_type {
        // NumericLiteral
        TokenType::NumLit => {
            let value = tokens[0].value;
            let numeric_literal = ParserNode::new(
                Vec::new(),
                ParserNodeType::NumericLiteral(MVal::from_string_no_sanitize(value.to_string())),
            );
            return Ok((
                Some(ParserNode::new(
                    vec![numeric_literal],
                    ParserNodeType::Factor,
                )),
                &tokens[1..],
            ));
        }

        // StringLiteral
        TokenType::StrLit => {
            let value = tokens[0].value;
            let string_literal = ParserNode::new(
                Vec::new(),
                ParserNodeType::StringLiteral(MVal::from_string(value.to_string())),
            );

            return Ok((
                Some(ParserNode::new(
                    vec![string_literal],
                    ParserNodeType::Factor,
                )),
                &tokens[1..],
            ));
        }

        // Identifier
        TokenType::Identifier => {
            let identifier_name = tokens[0].value;
            let identifier_node =
                ParserNode::new(Vec::new(), ParserNodeType::Identifier(identifier_name));

            return Ok((
                Some(ParserNode::new(
                    vec![identifier_node],
                    ParserNodeType::Factor,
                )),
                &tokens[1..],
            ));
        }

        // ( expression )
        TokenType::LParen => {
            let (expression, expression_rest) = parse_relational_expression(&tokens[1..])?;
            return if let Some(expression_node) = expression {
                if expression_rest.len() == 0 {
                    return Err(ParseError {
                        remaining_tokens: expression_rest,
                        message: "Missing closing parenthesis.",
                    });
                } else if expression_rest[0].token_type != TokenType::RParen {
                    return Err(ParseError {
                        remaining_tokens: &expression_rest[1..],
                        message: "Missing closing parenthesis.",
                    });
                }

                Ok((
                    Some(ParserNode::new(
                        vec![expression_node],
                        ParserNodeType::Expression,
                    )),
                    &expression_rest[1..],
                ))
            } else {
                unreachable!("Expression cannot go to epsilon")
            };
        }
        _ => Err(ParseError {
            remaining_tokens: tokens,
            message: "Not a value.",
        }),
    }
}

pub struct ParseError<'a> {
    remaining_tokens: &'a [Token<'a>],
    message: &'a str,
}
