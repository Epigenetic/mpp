/*
 * Copyright (c) 2021, Jonah Shafran.
 *
 * SPDX-License-Identifier: GPL-3.0-only
 */

use crate::parser::parse_node::ParserNodeType::Identifier;
use crate::runtime::{MVal, Ops};
use std::collections::HashMap;
use std::fmt;

pub struct ParserNode<'a> {
    children: Vec<ParserNode<'a>>,
    node_type: ParserNodeType<'a>,
}

impl ParserNode<'_> {
    pub fn new<'a>(children: Vec<ParserNode<'a>>, node_type: ParserNodeType<'a>) -> ParserNode<'a> {
        ParserNode {
            children,
            node_type,
        }
    }

    pub fn to_bytes(&self, program: &mut Vec<u8>, variable_map: &mut HashMap<String, usize>) {
        match &self.node_type {
            ParserNodeType::Block(_) => {
                // Lines
                self.children[0].to_bytes(program, variable_map);
            }

            ParserNodeType::Lines => {
                //Line
                self.children[0].to_bytes(program, variable_map);

                // Has a Lines
                if self.children.len() == 2 {
                    self.children[1].to_bytes(program, variable_map);
                }
            }
            ParserNodeType::Line => {
                // Statement
                self.children[0].to_bytes(program, variable_map);

                // Has a LineTail
                if self.children.len() == 2 {
                    // LineTail
                    self.children[1].to_bytes(program, variable_map)
                }
            }

            ParserNodeType::Statements => {
                // Statement
                self.children[0].to_bytes(program, variable_map);

                // Has a Statements
                if self.children.len() == 2 {
                    // Statement
                    self.children[1].to_bytes(program, variable_map)
                }
            }
            ParserNodeType::Statement => {
                // WriteStatement | SetStatement | NewStatement
                self.children[0].to_bytes(program, variable_map)
            }
            ParserNodeType::NewStatement => {
                // IdentifierList
                self.children[0].to_bytes(program, variable_map)
            }
            ParserNodeType::IdentifierList => {
                // Identifier
                let identifier_node = &self.children[0];
                if let ParserNodeType::Identifier(identifier) = identifier_node.node_type {
                    if variable_map.contains_key(identifier) {
                        panic!("Variable already defined: {}.", identifier)
                    }
                    variable_map.insert(identifier.to_string(), variable_map.len());
                }

                program.push(Ops::New as u8);

                // Has an IdentifierListTail
                if self.children.len() == 2 {
                    // IdentifierListTail
                    self.children[1].to_bytes(program, variable_map);
                }
            }
            ParserNodeType::IdentifierListTail => {
                // Identifier
                let identifier_node = &self.children[0];
                if let ParserNodeType::Identifier(identifier) = identifier_node.node_type {
                    if variable_map.contains_key(identifier) {
                        panic!("Variable already defined: {}.", identifier)
                    }
                    variable_map.insert(identifier.to_string(), variable_map.len());
                }

                program.push(Ops::New as u8);

                // Has an IdentifierListTail
                if self.children.len() == 2 {
                    // IdentifierListTail
                    self.children[1].to_bytes(program, variable_map);
                }
            }
            ParserNodeType::SetStatement => {
                // AssignmentList
                self.children[0].to_bytes(program, variable_map);
            }
            ParserNodeType::AssignmentList => {
                // Assignment
                self.children[0].to_bytes(program, variable_map);

                // Has an AssignmentListTail
                if self.children.len() == 2 {
                    self.children[1].to_bytes(program, variable_map)
                }
            }
            ParserNodeType::AssignmentListTail => {
                // Assignment
                self.children[0].to_bytes(program, variable_map);

                // Has an AssignmentListTail
                if self.children.len() == 2 {
                    self.children[1].to_bytes(program, variable_map)
                }
            }
            ParserNodeType::AssignmentStatement => {
                // RelationalExpression
                self.children[1].to_bytes(program, variable_map);

                program.push(Ops::Set as u8);

                // Identifier
                self.children[0].to_bytes(program, variable_map)
            }
            ParserNodeType::WriteStatement => {
                // WriteExpressionList
                self.children[0].to_bytes(program, variable_map);
            }
            ParserNodeType::WriteExpression => {
                // Expression | ! | #
                self.children[0].to_bytes(program, variable_map);
                if self.children[0].node_type == ParserNodeType::RelationalExpression {
                    program.push(Ops::Write as u8);
                }
            }
            ParserNodeType::WriteFormat(format) => match format {
                WriteFormat::NewLine => program.push(Ops::WriteLine as u8),
                WriteFormat::ClearScreen => program.push(Ops::WriteClearScreen as u8),
                WriteFormat::ToCol => {
                    self.children[0].to_bytes(program, variable_map);
                    program.push(Ops::WriteToCol as u8)
                }
            },
            ParserNodeType::FormatExpression => {
                // HashBangFormat
                self.children[0].to_bytes(program, variable_map);

                //Has a FormatExpressionTail
                if self.children.len() == 2 {
                    //FormatExpressionTail
                    self.children[1].to_bytes(program, variable_map)
                }
            }
            ParserNodeType::FormatExpressionTail => {
                // RelationalExpression
                self.children[1].to_bytes(program, variable_map);

                // RelOp
                self.children[0].to_bytes(program, variable_map);
            }
            ParserNodeType::HashBangFormat => {
                // WriteFormat
                self.children[0].to_bytes(program, variable_map);

                // Has a HashBangFormat
                if self.children.len() == 2 {
                    // HashBangFormat
                    self.children[1].to_bytes(program, variable_map);
                }
            }
            ParserNodeType::WriteExpressionList => {
                // RelationalExpression
                self.children[0].to_bytes(program, variable_map);

                // Has a WriteExpressionListTail
                if self.children.len() == 2 {
                    // ExpressionListTail
                    self.children[1].to_bytes(program, variable_map);
                }
            }
            ParserNodeType::WriteExpressionListTail => {
                // RelationalExpression
                self.children[0].to_bytes(program, variable_map);

                // Has a WriteExpressionListTail
                if self.children.len() == 2 {
                    // WriteExpressionListTail
                    self.children[1].to_bytes(program, variable_map)
                }
            }
            ParserNodeType::RelationalExpression => {
                // Expression
                self.children[0].to_bytes(program, variable_map);

                // Has a RelationalExpressionTail
                if self.children.len() == 2 {
                    // RelationalExpressionTail
                    self.children[1].to_bytes(program, variable_map)
                }
            }
            ParserNodeType::RelationalExpressionTail => {
                // Expression
                self.children[1].to_bytes(program, variable_map);

                // RelOp
                self.children[0].to_bytes(program, variable_map);

                // Has a RelationalExpressionTail
                if self.children.len() == 3 {
                    //RelationalExpressionTail
                    self.children[2].to_bytes(program, variable_map)
                }
            }
            ParserNodeType::RelOp(op) => match op {
                RelOp::LessThan => program.push(Ops::LessThan as u8),
                RelOp::GreaterThan => program.push(Ops::GreaterThan as u8),
                RelOp::LessThanOrEqualTo => program.push(Ops::LessThanOrEqualTo as u8),
                RelOp::GreaterThanOrEqualTo => program.push(Ops::GreaterThanOrEqualTo as u8),
            },
            ParserNodeType::Expression => {
                // Term
                self.children[0].to_bytes(program, variable_map);
                // Has an ExpressionTail
                if self.children.len() == 2 {
                    self.children[1].to_bytes(program, variable_map);
                }
            }
            ParserNodeType::ExpressionTail => {
                // Term
                self.children[1].to_bytes(program, variable_map);
                // AddOp
                self.children[0].to_bytes(program, variable_map);
                // Has an ExpressionTail
                if self.children.len() == 3 {
                    // ExpressionTail
                    self.children[2].to_bytes(program, variable_map);
                }
            }
            ParserNodeType::Term => {
                //Factor
                self.children[0].to_bytes(program, variable_map);
                // Has a TermTail
                if self.children.len() == 2 {
                    // TermTail
                    self.children[1].to_bytes(program, variable_map);
                }
            }
            ParserNodeType::TermTail => {
                // Factor
                self.children[1].to_bytes(program, variable_map);
                // MulOp
                self.children[0].to_bytes(program, variable_map);
                // Has a TermTail
                if self.children.len() == 3 {
                    // TermTail
                    self.children[2].to_bytes(program, variable_map);
                }
            }
            ParserNodeType::Unary => {
                // Unary
                self.children[1].to_bytes(program, variable_map);

                // UnaryOp
                self.children[0].to_bytes(program, variable_map)
            }
            ParserNodeType::ExpTerm => {
                // Factor
                self.children[0].to_bytes(program, variable_map);

                // Has an ExponentialTermTail
                if self.children.len() == 2 {
                    // ExponentialTermTail
                    self.children[1].to_bytes(program, variable_map);
                }
            }
            ParserNodeType::ExpTermTail => {
                // Factor
                self.children[1].to_bytes(program, variable_map);

                // Exponential
                self.children[0].to_bytes(program, variable_map);

                // Has an ExponentialTermTail
                if self.children.len() == 3 {
                    // ExponentialTermTail
                    self.children[2].to_bytes(program, variable_map);
                }
            }
            ParserNodeType::ExpOp => program.push(Ops::Exp as u8),
            ParserNodeType::Factor => {
                // NumericLiteral | StringLiteral | ( Expression ) | Identifier
                if let Identifier(_) = self.children[0].node_type {
                    program.push(Ops::Get as u8);
                }
                self.children[0].to_bytes(program, variable_map);
            }
            ParserNodeType::MulOp(op) => match op {
                MulOp::Times => program.push(Ops::Mult as u8),
                MulOp::Divide => program.push(Ops::Div as u8),
                MulOp::Modulus => program.push(Ops::Mod as u8),
                MulOp::IntegerDivide => program.push(Ops::IntDiv as u8),
            },
            ParserNodeType::AddOp(op) => match op {
                AddOp::Plus => program.push(Ops::Add as u8),
                AddOp::Minus => program.push(Ops::Sub as u8),
            },
            ParserNodeType::UnaryOp(op) => match op {
                UnaryOp::Plus => program.push(Ops::ToNum as u8),
                UnaryOp::Minus => program.push(Ops::ToNegNum as u8),
                UnaryOp::Not => program.push(Ops::Not as u8),
            },
            ParserNodeType::NumericLiteral(value) => {
                program.push(Ops::Push as u8);
                for byte in value.to_bytes() {
                    program.push(byte)
                }
            }
            ParserNodeType::StringLiteral(value) => {
                program.push(Ops::Push as u8);
                for byte in value.to_bytes() {
                    program.push(byte)
                }
            }
            ParserNodeType::Identifier(identifier) => {
                let var_position = variable_map.get(&identifier.to_string());
                if var_position == None {
                    panic!("Undefined variable {}.", identifier);
                }
                let position_bytes = var_position.unwrap().to_le_bytes();
                for byte in position_bytes {
                    program.push(byte);
                }
            }
        }
    }
}

impl fmt::Display for ParserNode<'_> {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.node_type {
            ParserNodeType::Block(_) => write!(f, "Block"),

            ParserNodeType::Lines => write!(f, "Lines"),
            ParserNodeType::Line => write!(f, "Line"),

            ParserNodeType::Statements => write!(f, "Statements"),
            ParserNodeType::Statement => write!(f, "Statement"),

            ParserNodeType::NewStatement => write!(f, "NewStatement"),
            ParserNodeType::IdentifierList => write!(f, "IdentifierList"),
            ParserNodeType::IdentifierListTail => write!(f, "IdentifierListTail"),

            ParserNodeType::SetStatement => write!(f, "SetStatement"),
            ParserNodeType::AssignmentList => write!(f, "AssignmentList"),
            ParserNodeType::AssignmentListTail => write!(f, "AssignmentListTail"),
            ParserNodeType::AssignmentStatement => write!(f, "AssignmentStatement"),

            ParserNodeType::WriteStatement => write!(f, "WriteStatement"),
            ParserNodeType::WriteExpression => write!(f, "WriteExpression"),
            ParserNodeType::WriteFormat(format) => write!(f, "WriteFormat: {:?}", format),
            ParserNodeType::WriteExpressionList => write!(f, "WriteExpressionList"),
            ParserNodeType::WriteExpressionListTail => write!(f, "WriteExpressionListTail"),
            ParserNodeType::FormatExpression => write!(f, "FormatExpression"),
            ParserNodeType::FormatExpressionTail => write!(f, "FormatExpressionTail"),
            ParserNodeType::HashBangFormat => write!(f, "HashBangFormat"),

            ParserNodeType::RelationalExpression => write!(f, "RelationalExpression"),
            ParserNodeType::RelationalExpressionTail => write!(f, "RelationalExpressionTail"),
            ParserNodeType::RelOp(op) => write!(f, "RelOp: {:?}", op),
            ParserNodeType::Expression => write!(f, "Expression"),
            ParserNodeType::ExpressionTail => write!(f, "ExpressionTail"),
            ParserNodeType::Term => write!(f, "Term"),
            ParserNodeType::TermTail => write!(f, "TermTail"),
            ParserNodeType::Unary => write!(f, "Unary"),
            ParserNodeType::Factor => write!(f, "Factor"),
            ParserNodeType::AddOp(op) => write!(f, "AddOp: {:?}", op),
            ParserNodeType::MulOp(op) => write!(f, "MulOp: {:?}", op),
            ParserNodeType::UnaryOp(op) => write!(f, "UnaryOp: {:?}", op),
            ParserNodeType::ExpOp => write!(f, "ExpOp"),
            ParserNodeType::ExpTerm => write!(f, "ExpTerm"),
            ParserNodeType::ExpTermTail => write!(f, "ExpTermTail"),
            ParserNodeType::StringLiteral(value) => write!(f, "StringLiteral: {}", value),
            ParserNodeType::NumericLiteral(value) => write!(f, "NumericLiteral: {}", value),
            ParserNodeType::Identifier(var_name) => write!(f, "Identifier: {}", var_name),
        }
    }
}

#[derive(PartialEq)]
pub enum ParserNodeType<'a> {
    Block(Option<HashMap<&'a str, usize>>),

    Lines,
    Line,

    Statements,
    Statement,

    SetStatement,
    AssignmentList,
    AssignmentListTail,
    AssignmentStatement,

    NewStatement,
    IdentifierList,
    IdentifierListTail,

    WriteStatement,
    WriteExpressionList,
    WriteExpressionListTail,
    WriteExpression,
    WriteFormat(WriteFormat),
    FormatExpression,
    FormatExpressionTail,
    HashBangFormat,

    RelationalExpression,
    RelationalExpressionTail,

    Expression,
    ExpressionTail,
    Term,
    TermTail,
    Unary,
    Factor,
    AddOp(AddOp),
    MulOp(MulOp),
    UnaryOp(UnaryOp),
    RelOp(RelOp),
    ExpOp,
    ExpTerm,
    ExpTermTail,

    NumericLiteral(MVal),
    StringLiteral(MVal),
    Identifier(&'a str),
}

#[derive(Debug, PartialEq)]
pub enum AddOp {
    Plus,
    Minus,
}

#[derive(Debug, PartialEq)]
pub enum MulOp {
    Times,
    Divide,
    Modulus,
    IntegerDivide,
}

#[derive(Debug, PartialEq)]
pub enum UnaryOp {
    Plus,
    Minus,
    Not,
}

#[derive(Debug, PartialEq)]
pub enum WriteFormat {
    NewLine,
    ClearScreen,
    ToCol,
}

#[derive(Debug, PartialEq)]
pub enum RelOp {
    GreaterThan,
    LessThan,
    GreaterThanOrEqualTo,
    LessThanOrEqualTo,
}

pub fn print_parse_tree(root: &ParserNode) {
    print_parse_tree_recursive(root, 0);
}

fn print_parse_tree_recursive(node: &ParserNode, indent: usize) {
    for _ in 0..indent {
        print!("\t | ")
    }
    println!("{}", node);

    for child in &node.children {
        print_parse_tree_recursive(child, indent + 1);
    }
}
