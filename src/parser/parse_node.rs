/*
 * Copyright (c) 2021, Jonah Shafran.
 *
 * SPDX-License-Identifier: GPL-3.0-only
 */

use rust_decimal::Decimal;
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
    NumericLiteral(Decimal),
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
