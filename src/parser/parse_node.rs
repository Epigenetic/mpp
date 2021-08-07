/*
 * Copyright (c) 2021, Jonah Shafran.
 *
 * SPDX-License-Identifier: GPL-3.0-only
 */

use crate::runtime::{MVal, Ops};
use std::fmt;

pub struct ParserNode {
    children: Vec<ParserNode>,
    node_type: ParserNodeType,
}

impl ParserNode {
    pub fn new(children: Vec<ParserNode>, node_type: ParserNodeType) -> ParserNode {
        ParserNode {
            children,
            node_type,
        }
    }

    pub fn to_bytes(&self, program: &mut Vec<u8>) {
        match &self.node_type {
            ParserNodeType::WriteStatement => {
                // WriteExpressionList
                self.children[0].to_bytes(program);
            }
            ParserNodeType::WriteExpression => {
                // Expression | ! | #
                self.children[0].to_bytes(program);
                if self.children[0].node_type == ParserNodeType::RelationalExpression {
                    program.push(Ops::Write as u8);
                }
            }
            ParserNodeType::WriteFormat(format) => match format {
                WriteFormat::NewLine => program.push(Ops::WriteLine as u8),
                WriteFormat::ClearScreen => program.push(Ops::WriteClearScreen as u8),
                WriteFormat::ToCol => {
                    self.children[0].to_bytes(program);
                    program.push(Ops::WriteToCol as u8)
                }
            },
            ParserNodeType::FormatExpression => {
                // HashBangFormat
                self.children[0].to_bytes(program);

                //Has a FormatExpressionTail
                if self.children.len() == 2 {
                    //FormatExpressionTail
                    self.children[1].to_bytes(program)
                }
            }
            ParserNodeType::FormatExpressionTail => {
                // RelationalExpression
                self.children[1].to_bytes(program);

                // RelOp
                self.children[0].to_bytes(program);
            }
            ParserNodeType::HashBangFormat => {
                // WriteFormat
                self.children[0].to_bytes(program);

                // Has a HashBangFormat
                if self.children.len() == 2 {
                    // HashBangFormat
                    self.children[1].to_bytes(program);
                }
            }
            ParserNodeType::WriteExpressionList => {
                // RelationalExpression
                self.children[0].to_bytes(program);

                // Has a WriteExpressionListTail
                if self.children.len() == 2 {
                    // ExpressionListTail
                    self.children[1].to_bytes(program);
                }
            }
            ParserNodeType::WriteExpressionListTail => {
                // RelationalExpression
                self.children[0].to_bytes(program);

                // Has a WriteExpressionListTail
                if self.children.len() == 2 {
                    // WriteExpressionListTail
                    self.children[1].to_bytes(program)
                }
            }
            ParserNodeType::RelationalExpression => {
                // Expression
                self.children[0].to_bytes(program);

                // Has a RelationalExpressionTail
                if self.children.len() == 2 {
                    // RelationalExpressionTail
                    self.children[1].to_bytes(program)
                }
            }
            ParserNodeType::RelationalExpressionTail => {
                // Expression
                self.children[1].to_bytes(program);

                // RelOp
                self.children[0].to_bytes(program);
            }
            ParserNodeType::RelOp(op) => match op {
                RelOp::LessThan => program.push(Ops::LessThan as u8),
                RelOp::GreaterThan => program.push(Ops::GreaterThan as u8),
                RelOp::LessThanOrEqualTo => program.push(Ops::LessThanOrEqualTo as u8),
                RelOp::GreaterThanOrEqualTo => program.push(Ops::GreaterThanOrEqualTo as u8),
            },
            ParserNodeType::Expression => {
                // Term
                self.children[0].to_bytes(program);
                // Has an ExpressionTail
                if self.children.len() == 2 {
                    self.children[1].to_bytes(program);
                }
            }
            ParserNodeType::ExpressionTail => {
                // Term
                self.children[1].to_bytes(program);
                // AddOp
                self.children[0].to_bytes(program);
                // Has an ExpressionTail
                if self.children.len() == 3 {
                    // ExpressionTail
                    self.children[2].to_bytes(program);
                }
            }
            ParserNodeType::Term => {
                //Factor
                self.children[0].to_bytes(program);
                // Has a TermTail
                if self.children.len() == 2 {
                    // TermTail
                    self.children[1].to_bytes(program);
                }
            }
            ParserNodeType::TermTail => {
                // Factor
                self.children[1].to_bytes(program);
                // MulOp
                self.children[0].to_bytes(program);
                // Has a TermTail
                if self.children.len() == 3 {
                    // TermTail
                    self.children[2].to_bytes(program);
                }
            }
            ParserNodeType::Unary => {
                // Unary
                self.children[1].to_bytes(program);

                // UnaryOp
                self.children[0].to_bytes(program)
            }
            ParserNodeType::ExpTerm => {
                // Factor
                self.children[0].to_bytes(program);

                // Has an ExponentialTermTail
                if self.children.len() == 2 {
                    // ExponentialTermTail
                    self.children[1].to_bytes(program);
                }
            }
            ParserNodeType::ExpTermTail => {
                // Factor
                self.children[1].to_bytes(program);

                // Exponential
                self.children[0].to_bytes(program);

                // Has an ExponentialTermTail
                if self.children.len() == 3 {
                    // ExponentialTermTail
                    self.children[2].to_bytes(program);
                }
            }
            ParserNodeType::ExpOp => program.push(Ops::Exp as u8),
            ParserNodeType::Factor => {
                // NumericLiteral | StringLiteral | ( Expression )
                //TODO: Handle more complex factors
                self.children[0].to_bytes(program);
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
                UnaryOp::Not => todo!(),
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
        }
    }
}

impl fmt::Display for ParserNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.node_type {
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
        }
    }
}

#[derive(PartialEq)]
pub enum ParserNodeType {
    NumericLiteral(MVal),
    StringLiteral(MVal),
    Expression,
    ExpressionTail,
    Term,
    TermTail,
    Unary,
    Factor,
    AddOp(AddOp),
    MulOp(MulOp),
    UnaryOp(UnaryOp),
    ExpOp,
    ExpTerm,
    ExpTermTail,
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
    RelOp(RelOp),
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
