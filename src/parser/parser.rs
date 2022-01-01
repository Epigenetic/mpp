/*
 * Copyright (c) 2021. Jonah Shafran
 *
 * SPDX-License-Identifier: GPL-3.0-only
 */

use crate::lexer::{ReservedToken, Token, TokenType};
use crate::parser::parse_node::{
    AddOp, EqOp, MulOp, ParserNode, RelOp, Type, UnaryOp, WriteFormat,
};
use crate::parser::ParserNodeType;
use crate::runtime::{MVal, MValType};

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
        eprintln!(" {}", error.message);
    }
}

/// Check that the next token is of the expected type.
/// Returns an appropriate ParseError if it does not.
fn expect<'a>(tokens: &'a [Token<'a>], expected_type: TokenType) -> Result<(), ParseError<'a>> {
    if tokens.len() == 0 || tokens[0].token_type == expected_type {
        return Ok(());
    }
    return Err(ParseError {
        remaining_tokens: tokens,
        message: format!("Expected {:?}.", expected_type),
    });
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
            expect(statements_rest, TokenType::NewLine)?;

            Ok((
                Some(ParserNode::new(
                    vec![statement_node, statements_node],
                    ParserNodeType::Line,
                )),
                &statements_rest[1..],
            ))
        } else {
            expect(statements_rest, TokenType::NewLine)?;

            Ok((
                Some(ParserNode::new(vec![statement_node], ParserNodeType::Line)),
                &statement_rest[1..],
            ))
        }
    } else {
        expect(statement_rest, TokenType::NewLine)?;

        Ok((
            Some(ParserNode::new(Vec::new(), ParserNodeType::Line)),
            &tokens[1..],
        ))
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

/// Statement -> WriteStatement | SetStatement | NewStatement | IfStatement | ε
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
                unreachable!("NewStatement cannot go to epsilon")
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
                unreachable!("SetStatement cannot go to epsilon")
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
                unreachable!("WriteStatement cannot go to epsilon")
            }
        }
        TokenType::Reserved(ReservedToken::If) => {
            let (if_statement, if_statement_rest) = parse_if_statement(tokens)?;

            if let Some(if_statement_node) = if_statement {
                Ok((
                    Some(ParserNode::new(
                        vec![if_statement_node],
                        ParserNodeType::Statement,
                    )),
                    if_statement_rest,
                ))
            } else {
                unreachable!("IfStatement cannot go to epsilon.")
            }
        }
        TokenType::Reserved(ReservedToken::For) => {
            let (for_statement, for_statement_rest) = parse_for_statement(tokens)?;

            if let Some(for_statement_node) = for_statement {
                Ok((
                    Some(ParserNode::new(
                        vec![for_statement_node],
                        ParserNodeType::Statement,
                    )),
                    for_statement_rest,
                ))
            } else {
                unreachable!("ForStatement cannot go to epsilon.")
            }
        }
        TokenType::NewLine | TokenType::Reserved(ReservedToken::Else) => Ok((None, tokens)),
        _ => Err(ParseError {
            remaining_tokens: tokens,
            message: "Illegal start of statement.".to_string(),
        }),
    };
}

/// ForStatement -> For Identifier = ForBound Statements | For Statements
fn parse_for_statement<'a>(
    tokens: &'a [Token],
) -> Result<(Option<ParserNode<'a>>, &'a [Token<'a>]), ParseError<'a>> {
    return match &tokens[0].token_type {
        TokenType::Reserved(ReservedToken::For) => {
            if tokens[1].token_type != TokenType::Identifier {
                let (statements, statements_rest) = parse_statements(&tokens[1..])?;

                if let Some(statements_node) = statements {
                    Ok((
                        Some(ParserNode::new(
                            vec![statements_node],
                            ParserNodeType::ForStatement,
                        )),
                        statements_rest,
                    ))
                } else {
                    Err(ParseError {
                        remaining_tokens: &tokens[1..],
                        message: "Cannot have an empty for loop.".to_string(),
                    })
                }
            } else if tokens[2].token_type != TokenType::Equals {
                Err(ParseError {
                    remaining_tokens: &tokens[2..],
                    message: "Expected equals".to_string(),
                })
            } else {
                let identifier_node =
                    ParserNode::new(Vec::new(), ParserNodeType::Identifier(&tokens[1].value));

                let (for_bound, for_bound_rest) = parse_for_bound(&tokens[3..], identifier_node)?;

                if let Some(for_bound_node) = for_bound {
                    let (statements, statements_rest) = parse_statements(for_bound_rest)?;

                    if let Some(statements_node) = statements {
                        Ok((
                            Some(ParserNode::new(
                                vec![for_bound_node, statements_node],
                                ParserNodeType::ForStatement,
                            )),
                            statements_rest,
                        ))
                    } else {
                        Err(ParseError {
                            remaining_tokens: for_bound_rest,
                            message: "Cannot have an empty for loop.".to_string(),
                        })
                    }
                } else {
                    unreachable!("ForBoundList cannot go to epsilon")
                }
            }
        }
        _ => Err(ParseError {
            remaining_tokens: tokens,
            message: "Expecting for.".to_string(),
        }),
    };
}

/// ForBound -> EqualityExpression | EqualityExpression : EqualityExpression
///             | EqualityExpression : EqualityExpression : EqualityExpression | ε
fn parse_for_bound<'a>(
    tokens: &'a [Token],
    identifier_node: ParserNode<'a>,
) -> Result<(Option<ParserNode<'a>>, &'a [Token<'a>]), ParseError<'a>> {
    let (start_expression, start_expression_rest) = parse_equality_expression(tokens)?;

    return if let Some(start_expression_node) = start_expression {
        if start_expression_rest.len() == 0
            || start_expression_rest[0].token_type != TokenType::Colon
        {
            Ok((
                Some(ParserNode::new(
                    vec![identifier_node, start_expression_node],
                    ParserNodeType::ForBound,
                )),
                start_expression_rest,
            ))
        } else {
            let (increment_expression, increment_expression_rest) =
                parse_equality_expression(&start_expression_rest[1..])?;

            if let Some(increment_expression_node) = increment_expression {
                if increment_expression_rest.len() == 0
                    || increment_expression_rest[0].token_type != TokenType::Colon
                {
                    Ok((
                        Some(ParserNode::new(
                            vec![
                                identifier_node,
                                start_expression_node,
                                increment_expression_node,
                            ],
                            ParserNodeType::ForBound,
                        )),
                        &increment_expression_rest[0..],
                    ))
                } else {
                    let (bound_expression, bound_expression_rest) =
                        parse_equality_expression(&increment_expression_rest[1..])?;

                    if let Some(bound_expression_node) = bound_expression {
                        Ok((
                            Some(ParserNode::new(
                                vec![
                                    identifier_node,
                                    start_expression_node,
                                    increment_expression_node,
                                    bound_expression_node,
                                ],
                                ParserNodeType::ForBound,
                            )),
                            bound_expression_rest,
                        ))
                    } else {
                        Err(ParseError {
                            remaining_tokens: increment_expression_rest,
                            message: "Missing bound expression.".to_string(),
                        })
                    }
                }
            } else {
                Err(ParseError {
                    remaining_tokens: start_expression_rest,
                    message: "Missing increment expression.".to_string(),
                })
            }
        }
    } else {
        Ok((None, tokens))
    };
}

/// IfStatement -> If EqualityExpression Statements ElseStatement
/// TODO: Permit a block below the if expression, this may have to be handled by the do statement
fn parse_if_statement<'a>(
    tokens: &'a [Token],
) -> Result<(Option<ParserNode<'a>>, &'a [Token<'a>]), ParseError<'a>> {
    return match &tokens[0].token_type {
        TokenType::Reserved(ReservedToken::If) => {
            let (equality_expression, equality_expression_rest) =
                parse_equality_expression(&tokens[1..])?;

            if let Some(equality_expression_node) = equality_expression {
                let (statements, statements_rest) = parse_statements(equality_expression_rest)?;

                if let Some(statements_node) = statements {
                    if statements_rest[0].token_type != TokenType::NewLine {
                        return Err(ParseError {
                            remaining_tokens: statements_rest,
                            message: "Expected end of line.".to_string(),
                        });
                    }

                    let (else_statement, else_statement_rest) =
                        parse_else_statement(&statements_rest[1..])?;

                    if let Some(else_statement_node) = else_statement {
                        Ok((
                            Some(ParserNode::new(
                                vec![
                                    equality_expression_node,
                                    statements_node,
                                    else_statement_node,
                                ],
                                ParserNodeType::IfStatement,
                            )),
                            else_statement_rest,
                        ))
                    } else {
                        Ok((
                            Some(ParserNode::new(
                                vec![equality_expression_node, statements_node],
                                ParserNodeType::IfStatement,
                            )),
                            statements_rest,
                        ))
                    }
                } else {
                    Err(ParseError {
                        remaining_tokens: equality_expression_rest,
                        message: "At least one statement after if expression required.".to_string(),
                    })
                }
            } else {
                unreachable!("EqualityExpression cannot go to epsilon")
            }
        }
        _ => Err(ParseError {
            remaining_tokens: tokens,
            message: "Expected if.".to_string(),
        }),
    };
}

/// ElseStatement -> Else Statements | ε
/// TODO: Permit a block below the if expression, this may have to be handled by the do statement
fn parse_else_statement<'a>(
    tokens: &'a [Token],
) -> Result<(Option<ParserNode<'a>>, &'a [Token<'a>]), ParseError<'a>> {
    if tokens.len() == 0 || tokens[0].token_type == TokenType::NewLine {
        return Ok((None, tokens));
    }
    return match &tokens[0].token_type {
        TokenType::Reserved(ReservedToken::Else) => {
            let (statements, statements_rest) = parse_statements(&tokens[1..])?;

            if let Some(statements_node) = statements {
                Ok((
                    Some(ParserNode::new(
                        vec![statements_node],
                        ParserNodeType::ElseStatement,
                    )),
                    statements_rest,
                ))
            } else {
                Err(ParseError {
                    remaining_tokens: &tokens[1..],
                    message: "At least one statement required after else".to_string(),
                })
            }
        }
        _ => Ok((None, tokens)),
    };
}

/// NewStatement -> New IdentifierList
fn parse_new_statement<'a>(
    tokens: &'a [Token],
) -> Result<(Option<ParserNode<'a>>, &'a [Token<'a>]), ParseError<'a>> {
    expect(tokens, TokenType::Reserved(ReservedToken::New))?;

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
        unreachable!("IdentifierList cannot go to epsilon")
    }
}

/// IdentifierList -> Identifier : Type IdentifierListTail
fn parse_identifier_list<'a>(
    tokens: &'a [Token],
) -> Result<(Option<ParserNode<'a>>, &'a [Token<'a>]), ParseError<'a>> {
    expect(tokens, TokenType::Identifier)?;

    let identifier_node = ParserNode::new(Vec::new(), ParserNodeType::Identifier(tokens[0].value));

    expect(&tokens[1..], TokenType::Colon)?;

    if tokens.len() < 2 {
        return Err(ParseError {
            remaining_tokens: &tokens[2..],
            message: "Expecting type".to_string(),
        });
    }

    let identifier_type_node = match &tokens[2].token_type {
        TokenType::Reserved(ReservedToken::String) => {
            ParserNode::new(Vec::new(), ParserNodeType::Type(Type::String))
        }
        TokenType::Reserved(ReservedToken::Int) => {
            ParserNode::new(Vec::new(), ParserNodeType::Type(Type::Int))
        }
        TokenType::Reserved(ReservedToken::Double) => {
            ParserNode::new(Vec::new(), ParserNodeType::Type(Type::Double))
        }
        _ => {
            return Err(ParseError {
                remaining_tokens: &tokens[2..],
                message: "Expecting type".to_string(),
            })
        }
    };

    let (identifier_list_tail, identifier_list_tail_rest) =
        parse_identifier_list_tail(&tokens[3..])?;

    if let Some(identifier_list_tail_node) = identifier_list_tail {
        Ok((
            Some(ParserNode::new(
                vec![
                    identifier_node,
                    identifier_type_node,
                    identifier_list_tail_node,
                ],
                ParserNodeType::IdentifierList,
            )),
            identifier_list_tail_rest,
        ))
    } else {
        Ok((
            Some(ParserNode::new(
                vec![identifier_node, identifier_type_node],
                ParserNodeType::IdentifierList,
            )),
            &tokens[3..],
        ))
    }
}

/// IdentifierListTail -> , Identifier : Type IdentifierListTail | ε
fn parse_identifier_list_tail<'a>(
    tokens: &'a [Token],
) -> Result<(Option<ParserNode<'a>>, &'a [Token<'a>]), ParseError<'a>> {
    // Not long enough to have another item
    if tokens.len() < 2 {
        return Ok((None, tokens));
    }

    return match &tokens[0].token_type {
        TokenType::Comma => {
            expect(&tokens[1..], TokenType::Identifier)?;

            let identifier_node =
                ParserNode::new(Vec::new(), ParserNodeType::Identifier(tokens[1].value));

            expect(&tokens[2..], TokenType::Colon)?;

            if tokens.len() < 3 {
                return Err(ParseError {
                    remaining_tokens: &tokens[2..],
                    message: "Expecting type".to_string(),
                });
            }

            let identifier_type_node = match &tokens[3].token_type {
                TokenType::Reserved(ReservedToken::String) => {
                    ParserNode::new(Vec::new(), ParserNodeType::Type(Type::String))
                }
                TokenType::Reserved(ReservedToken::Int) => {
                    ParserNode::new(Vec::new(), ParserNodeType::Type(Type::Int))
                }
                TokenType::Reserved(ReservedToken::Double) => {
                    ParserNode::new(Vec::new(), ParserNodeType::Type(Type::Double))
                }
                _ => {
                    return Err(ParseError {
                        remaining_tokens: &tokens[2..],
                        message: "Expecting type".to_string(),
                    })
                }
            };

            let (identifier_list_tail, identifier_list_tail_rest) =
                parse_identifier_list_tail(&tokens[4..])?;

            if let Some(identifier_list_tail_node) = identifier_list_tail {
                Ok((
                    Some(ParserNode::new(
                        vec![
                            identifier_node,
                            identifier_type_node,
                            identifier_list_tail_node,
                        ],
                        ParserNodeType::IdentifierListTail,
                    )),
                    identifier_list_tail_rest,
                ))
            } else {
                Ok((
                    Some(ParserNode::new(
                        vec![identifier_node, identifier_type_node],
                        ParserNodeType::IdentifierListTail,
                    )),
                    &tokens[4..],
                ))
            }
        }
        _ => Ok((None, tokens)),
    };
}

/// SetStatement -> Set AssignmentList
fn parse_set_statement<'a>(
    tokens: &'a [Token],
) -> Result<(Option<ParserNode<'a>>, &'a [Token<'a>]), ParseError<'a>> {
    expect(tokens, TokenType::Reserved(ReservedToken::Set))?;

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
        unreachable!("AssignmentList cannot go to epsilon")
    }
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
        unreachable!("AssignmentStatement cannot go to epsilon")
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

/// AssignmentStatement -> Identifier = EqualityExpression | ε
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

            expect(&tokens[1..], TokenType::Equals)?;
            let (equality_expression, equality_expression_rest) =
                parse_equality_expression(&tokens[2..])?;

            if let Some(equality_expression_node) = equality_expression {
                Ok((
                    Some(ParserNode::new(
                        vec![identifier_node, equality_expression_node],
                        ParserNodeType::AssignmentStatement,
                    )),
                    equality_expression_rest,
                ))
            } else {
                unreachable!("EqualityExpression cannot go to epsilon")
            }
        }
        _ => Ok((None, tokens)),
    };
}

/// WriteStatement -> Write WriteExpressionList
fn parse_write_statement<'a>(
    tokens: &'a [Token],
) -> Result<(Option<ParserNode<'a>>, &'a [Token<'a>]), ParseError<'a>> {
    expect(tokens, TokenType::Reserved(ReservedToken::Write))?;

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
            message: "Operand for write expected.".to_string(),
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
                    message: "Operand for write expected.".to_string(),
                })
            }
        }
        _ => Ok((None, tokens)),
    };
}

/// FormatExpression | EqualityExpression | ε
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
            let (equality_expression, equality_expression_tail) =
                parse_equality_expression(tokens)?;

            return if let Some(equality_expression_node) = equality_expression {
                Ok((
                    Some(ParserNode::new(
                        vec![equality_expression_node],
                        ParserNodeType::WriteExpression,
                    )),
                    equality_expression_tail,
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

///  ? EqualityExpression | ε
fn parse_format_expression_tail<'a>(
    tokens: &'a [Token],
) -> Result<(Option<ParserNode<'a>>, &'a [Token<'a>]), ParseError<'a>> {
    if tokens.len() == 0 {
        return Ok((None, tokens));
    }

    return match &tokens[0].token_type {
        TokenType::QuestionMark => {
            let (equality_expression, equality_expression_rest) =
                parse_equality_expression(&tokens[1..])?;

            if let Some(equality_expression_node) = equality_expression {
                let write_to_col_node = ParserNode::new(
                    vec![equality_expression_node],
                    ParserNodeType::WriteFormat(WriteFormat::ToCol),
                );

                Ok((
                    Some(ParserNode::new(
                        vec![write_to_col_node],
                        ParserNodeType::FormatExpressionTail,
                    )),
                    equality_expression_rest,
                ))
            } else {
                unreachable!("Expression can not go to epsilon");
            }
        }
        _ => Ok((None, tokens)),
    };
}

/// EqualityExpression -> RelationalExpression EqualityExpressionTail
fn parse_equality_expression<'a>(
    tokens: &'a [Token],
) -> Result<(Option<ParserNode<'a>>, &'a [Token<'a>]), ParseError<'a>> {
    let (relational_expression, relational_expression_rest) = parse_relational_expression(tokens)?;

    if let Some(relational_expression_node) = relational_expression {
        let (equality_expression_tail, equality_expression_tail_rest) =
            parse_equality_expression_tail(relational_expression_rest)?;

        if let Some(equality_expression_tail_node) = equality_expression_tail {
            Ok((
                Some(ParserNode::new(
                    vec![relational_expression_node, equality_expression_tail_node],
                    ParserNodeType::EqualityExpression,
                )),
                equality_expression_tail_rest,
            ))
        } else {
            Ok((
                Some(ParserNode::new(
                    vec![relational_expression_node],
                    ParserNodeType::EqualityExpression,
                )),
                relational_expression_rest,
            ))
        }
    } else {
        unreachable!("RelationalExpression cannot go to epsilon");
    }
}

/// EqualityExpressionTail -> EqOp RelationalExpression EqualityExpressionTail | ε
fn parse_equality_expression_tail<'a>(
    tokens: &'a [Token],
) -> Result<(Option<ParserNode<'a>>, &'a [Token<'a>]), ParseError<'a>> {
    let (eq_op, eq_op_rest) = parse_eq_op(tokens)?;

    return if let Some(eq_op_node) = eq_op {
        let (relational_expression, relational_expression_rest) =
            parse_relational_expression(eq_op_rest)?;

        if let Some(relational_expression_node) = relational_expression {
            let (equality_expression_tail, equality_expression_tail_rest) =
                parse_equality_expression_tail(relational_expression_rest)?;

            if let Some(equality_expression_tail_node) = equality_expression_tail {
                Ok((
                    Some(ParserNode::new(
                        vec![
                            eq_op_node,
                            relational_expression_node,
                            equality_expression_tail_node,
                        ],
                        ParserNodeType::EqualityExpressionTail,
                    )),
                    equality_expression_tail_rest,
                ))
            } else {
                Ok((
                    Some(ParserNode::new(
                        vec![eq_op_node, relational_expression_node],
                        ParserNodeType::EqualityExpressionTail,
                    )),
                    relational_expression_rest,
                ))
            }
        } else {
            unreachable!("RelationalExpression cannot go to epsilon")
        }
    } else {
        Ok((None, tokens))
    };
}

/// EqOp => = | '= | ε
fn parse_eq_op<'a>(
    tokens: &'a [Token],
) -> Result<(Option<ParserNode<'a>>, &'a [Token<'a>]), ParseError<'a>> {
    if tokens.len() == 0 || tokens[0].token_type == TokenType::NewLine {
        return Ok((None, tokens));
    }

    return match &tokens[0].token_type {
        TokenType::Equals => Ok((
            Some(ParserNode::new(
                Vec::new(),
                ParserNodeType::EqOp(EqOp::Equals),
            )),
            &tokens[1..],
        )),
        TokenType::NotEquals => Ok((
            Some(ParserNode::new(
                Vec::new(),
                ParserNodeType::EqOp(EqOp::NotEquals),
            )),
            &tokens[1..],
        )),
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

/// RelationalExpressionTail -> RelOp Expression RelationalExpressionTail | ε
fn parse_relational_expression_tail<'a>(
    tokens: &'a [Token],
) -> Result<(Option<ParserNode<'a>>, &'a [Token<'a>]), ParseError<'a>> {
    let (rel_op, rel_op_rest) = parse_rel_op(tokens)?;

    return if let Some(rel_op_node) = rel_op {
        let (expression, expression_rest) = parse_expression(rel_op_rest)?;

        if let Some(expression_node) = expression {
            let (relational_expression_tail, relational_expression_tail_rest) =
                parse_relational_expression_tail(expression_rest)?;

            if let Some(relational_expression_tail_node) = relational_expression_tail {
                Ok((
                    Some(ParserNode::new(
                        vec![
                            rel_op_node,
                            expression_node,
                            relational_expression_tail_node,
                        ],
                        ParserNodeType::RelationalExpressionTail,
                    )),
                    relational_expression_tail_rest,
                ))
            } else {
                Ok((
                    Some(ParserNode::new(
                        vec![rel_op_node, expression_node],
                        ParserNodeType::RelationalExpressionTail,
                    )),
                    expression_rest,
                ))
            }
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
            message: "Missing operand.".to_string(),
        });
    } else if tokens[0].token_type == TokenType::NewLine {
        return Err(ParseError {
            remaining_tokens: &tokens[1..],
            message: "Missing operand.".to_string(),
        });
    }

    match &tokens[0].token_type {
        // NumericLiteral
        TokenType::NumLit => {
            let value = tokens[0].value;
            let double_val = value.parse::<f64>().unwrap();
            let numeric_literal = if double_val.fract() == 0.0 {
                let mut mval = MVal::new(MValType::Int);
                mval.set_int_val(double_val as i32);

                ParserNode::new(Vec::new(), ParserNodeType::NumericLiteral(mval))
            } else {
                let mut mval = MVal::new(MValType::Double);
                mval.set_double_val(double_val);

                ParserNode::new(Vec::new(), ParserNodeType::NumericLiteral(mval))
            };

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
            let (equality_expression, expression_rest) = parse_equality_expression(&tokens[1..])?;
            return if let Some(equality_expression_node) = equality_expression {
                if expression_rest.len() == 0 {
                    return Err(ParseError {
                        remaining_tokens: expression_rest,
                        message: "Missing closing parenthesis.".to_string(),
                    });
                } else if expression_rest[0].token_type != TokenType::RParen {
                    return Err(ParseError {
                        remaining_tokens: &expression_rest[1..],
                        message: "Missing closing parenthesis.".to_string(),
                    });
                }

                Ok((
                    Some(ParserNode::new(
                        vec![equality_expression_node],
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
            message: "Not a value.".to_string(),
        }),
    }
}

pub struct ParseError<'a> {
    remaining_tokens: &'a [Token<'a>],
    message: String,
}
