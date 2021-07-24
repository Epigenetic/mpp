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
            ParserNodeType::NumericLiteral(value) => {
                program.push(Ops::Push as u8);
                for byte in value.to_bytes() {
                    program.push(byte)
                }
            }
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
            ParserNodeType::Factor => {
                // NumericLiteral
                //TODO: Handle more complex factors
                self.children[0].to_bytes(program);
            }
            ParserNodeType::MulOp(op) => match op {
                MulOp::Times => program.push(Ops::Mult as u8),
                MulOp::Divide => program.push(Ops::Div as u8),
                MulOp::Modulus => program.push(Ops::Mod as u8),
            },
            ParserNodeType::AddOp(op) => match op {
                AddOp::Plus => program.push(Ops::Add as u8),
                AddOp::Minus => program.push(Ops::Sub as u8),
            },
        }
    }
}

impl fmt::Display for ParserNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match &self.node_type {
            ParserNodeType::NumericLiteral(value) => write!(f, "NumericLiteral: {}", value),
            ParserNodeType::Expression => write!(f, "Expression"),
            ParserNodeType::ExpressionTail => write!(f, "ExpressionTail"),
            ParserNodeType::Term => write!(f, "Term"),
            ParserNodeType::TermTail => write!(f, "TermTail"),
            ParserNodeType::Factor => write!(f, "Factor"),
            ParserNodeType::AddOp(op) => write!(f, "AddOp: {:?}", op),
            ParserNodeType::MulOp(op) => write!(f, "MulOp: {:?}", op),
        }
    }
}

pub enum ParserNodeType {
    NumericLiteral(MVal),
    Expression,
    ExpressionTail,
    Term,
    TermTail,
    Factor,
    AddOp(AddOp),
    MulOp(MulOp),
}

#[derive(Debug)]
pub enum AddOp {
    Plus,
    Minus,
}

#[derive(Debug)]
pub enum MulOp {
    Times,
    Divide,
    Modulus,
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
