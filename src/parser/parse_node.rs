/*
 * Copyright (c) 2021, Jonah Shafran.
 *
 * SPDX-License-Identifier: GPL-3.0-only
 */

use crate::lexer::Token;
use crate::parser::parse_node::ParserNodeType::Identifier;
use crate::parser::scope::{ParameterDefinition, Scopes};
use crate::runtime::{MVal, MValType, Ops};
use std::collections::HashMap;
use std::fmt;

pub struct ParserNode<'a> {
    pub children: Vec<ParserNode<'a>>,
    pub node_type: ParserNodeType<'a>,
    pub value_type: Option<MValType>,
    pub token: Option<&'a Token<'a>>,
}

impl ParserNode<'_> {
    pub fn new<'a>(
        children: Vec<ParserNode<'a>>,
        node_type: ParserNodeType<'a>,
        token: Option<&'a Token<'a>>,
    ) -> ParserNode<'a> {
        ParserNode {
            children,
            node_type,
            value_type: None,
            token,
        }
    }

    pub fn to_bytes(&self, program: &mut Vec<u8>, identifier_scopes: &mut Scopes) {
        match &self.node_type {
            ParserNodeType::Block(_) => {
                // Lines
                self.children[0].to_bytes(program, identifier_scopes);
            }

            ParserNodeType::Lines => {
                //Line
                self.children[0].to_bytes(program, identifier_scopes);

                // Has a Lines
                if self.children.len() == 2 {
                    self.children[1].to_bytes(program, identifier_scopes);
                }
            }
            ParserNodeType::Line => {
                if self.children.len() > 0 {
                    // Statement
                    self.children[0].to_bytes(program, identifier_scopes);

                    // Has a LineTail
                    if self.children.len() == 2 {
                        // LineTail
                        self.children[1].to_bytes(program, identifier_scopes)
                    }
                }
            }

            ParserNodeType::Statements => {
                // Statement
                self.children[0].to_bytes(program, identifier_scopes);

                // Has a Statements
                if self.children.len() == 2 {
                    // Statement
                    self.children[1].to_bytes(program, identifier_scopes)
                }
            }
            ParserNodeType::Statement => {
                // WriteStatement | SetStatement | NewStatement
                self.children[0].to_bytes(program, identifier_scopes)
            }

            ParserNodeType::ForStatement => {
                // No bounds
                if self.children[0].node_type == ParserNodeType::Statements {
                    let start_pos = program.len();

                    self.children[0].to_bytes(program, identifier_scopes);

                    let end_pos = program.len();

                    program.push(Ops::JumpUp as u8);
                    program.push(0xff);
                    program.push(0xff);

                    let offset = end_pos - start_pos;

                    self.back_patch_jump(program, program.len(), offset);
                } else {
                    // TODO - Come up with a more elegant solution than hoisting the processing of the
                    //        For bound into the for statement
                    let for_bound_node = &self.children[0];

                    // EqualityExpression
                    for_bound_node.children[1].to_bytes(program, identifier_scopes);
                    program.push(Ops::Set as u8);

                    //Identifier
                    for_bound_node.children[0].to_bytes(program, identifier_scopes);

                    let loop_start_pos = program.len();
                    let mut for_condition_pos: Option<usize> = None;

                    // Has a third equality expression
                    if for_bound_node.children.len() == 4 {
                        program.push(Ops::Get as u8);

                        // Identifier
                        for_bound_node.children[0].to_bytes(program, identifier_scopes);

                        // EqualityExpression
                        for_bound_node.children[3].to_bytes(program, identifier_scopes);
                        program.push(Ops::LessThanOrEqualTo as u8);

                        // If should not loop jump over loop body
                        program.push(Ops::JumpIfFalse as u8);
                        program.push(0xff);
                        program.push(0xff);
                        for_condition_pos = Some(program.len());
                    }

                    // Statements
                    self.children[1].to_bytes(program, identifier_scopes);

                    // Has a second EqualityExpression
                    if for_bound_node.children.len() > 2 {
                        // EqualityExpression
                        for_bound_node.children[2].to_bytes(program, identifier_scopes);
                        program.push(Ops::Get as u8);

                        // Identifier
                        for_bound_node.children[0].to_bytes(program, identifier_scopes);

                        program.push(Ops::Add as u8);
                        program.push(Ops::Set as u8);

                        // Identifier
                        for_bound_node.children[0].to_bytes(program, identifier_scopes);
                    }
                    let end_pos = program.len();
                    let offset = end_pos - loop_start_pos;

                    program.push(Ops::JumpUp as u8);
                    program.push(0xff);
                    program.push(0xff);

                    self.back_patch_jump(program, program.len(), offset);

                    if let Some(for_condition_start) = for_condition_pos {
                        self.back_patch_jump(
                            program,
                            for_condition_start,
                            program.len() - for_condition_start + 3,
                        )
                    }
                }
            }

            ParserNodeType::ForBound => {
                panic!("Should not be calling to_bytes for ForBound")
            }

            ParserNodeType::IfStatement => {
                // EqualityExpression
                self.children[0].to_bytes(program, identifier_scopes);

                program.push(Ops::JumpIfFalse as u8);
                program.push(0xff);
                program.push(0xff);
                let if_start_pos = program.len();

                // Statements
                self.children[1].to_bytes(program, identifier_scopes);

                // Has an ElseStatement
                if self.children.len() == 3 {
                    program.push(Ops::Jump as u8);
                    program.push(0xff);
                    program.push(0xff);
                    let else_start_pos = program.len();
                    let if_end_pos = program.len();
                    let if_offset = if_end_pos - if_start_pos;

                    self.back_patch_jump(
                        program,
                        if_start_pos,
                        if_offset + std::mem::size_of::<u16>() + 1,
                    );

                    // ElseStatement
                    self.children[2].to_bytes(program, identifier_scopes);

                    let else_end_pos = program.len();
                    let else_offset = else_end_pos - else_start_pos;

                    self.back_patch_jump(
                        program,
                        else_start_pos,
                        else_offset + std::mem::size_of::<u16>() + 1,
                    );
                } else {
                    let end_pos = program.len();
                    let offset = end_pos - if_start_pos;

                    self.back_patch_jump(
                        program,
                        if_start_pos,
                        offset + std::mem::size_of::<u16>() + 1,
                    );
                }
            }

            ParserNodeType::ElseStatement => {
                // Statements
                self.children[0].to_bytes(program, identifier_scopes);
            }

            ParserNodeType::NewStatement => {
                // IdentifierList
                self.children[0].to_bytes(program, identifier_scopes)
            }
            ParserNodeType::IdentifierList => {
                // Identifier
                let identifier_node = &self.children[0];
                if let ParserNodeType::Identifier(identifier) = identifier_node.node_type {
                    program.push(Ops::New as u8);
                    if let ParserNodeType::Type(type_val) = &self.children[1].node_type {
                        identifier_scopes
                            .add_variable(identifier.to_string(), type_val.to_mval_type());
                        self.children[1].to_bytes(program, identifier_scopes)
                    }
                }

                // Has an IdentifierListTail
                if self.children.len() == 3 {
                    // IdentifierListTail
                    self.children[2].to_bytes(program, identifier_scopes);
                }
            }
            ParserNodeType::IdentifierListTail => {
                // Identifier
                let identifier_node = &self.children[0];
                if let ParserNodeType::Identifier(identifier) = identifier_node.node_type {
                    if variable_map.contains_key(identifier) {
                        panic!("Variable already defined: {}.", identifier)
                    }
                    program.push(Ops::New as u8);

                    if let ParserNodeType::Type(type_val) = &self.children[1].node_type {
                        variable_map.insert(
                            identifier.to_string(),
                            VariableDefinition {
                                stack_position: variable_map.len(),
                                val_type: type_val.to_mval_type(),
                            },
                        );
                        self.children[1].to_bytes(program, variable_map)
                    }
                }

                // Has an IdentifierListTail
                if self.children.len() == 3 {
                    // IdentifierListTail
                    self.children[2].to_bytes(program, variable_map);
                }
            }
            ParserNodeType::SetStatement => {
                // AssignmentList
                self.children[0].to_bytes(program, identifier_scopes);
            }
            ParserNodeType::AssignmentList => {
                // Assignment
                self.children[0].to_bytes(program, identifier_scopes);

                // Has an AssignmentListTail
                if self.children.len() == 2 {
                    self.children[1].to_bytes(program, identifier_scopes)
                }
            }
            ParserNodeType::AssignmentListTail => {
                // Assignment
                self.children[0].to_bytes(program, identifier_scopes);

                // Has an AssignmentListTail
                if self.children.len() == 2 {
                    self.children[1].to_bytes(program, identifier_scopes)
                }
            }
            ParserNodeType::AssignmentStatement => {
                // RelationalExpression
                self.children[1].to_bytes(program, identifier_scopes);

                program.push(Ops::Set as u8);

                // Identifier
                self.children[0].to_bytes(program, identifier_scopes)
            }
            ParserNodeType::WriteStatement => {
                // WriteExpressionList
                self.children[0].to_bytes(program, identifier_scopes);
            }
            ParserNodeType::WriteExpression => {
                // Expression | ! | #
                self.children[0].to_bytes(program, identifier_scopes);
                if self.children[0].node_type == ParserNodeType::EqualityExpression {
                    program.push(Ops::Write as u8);
                }
            }
            ParserNodeType::WriteFormat(format) => match format {
                WriteFormat::NewLine => program.push(Ops::WriteLine as u8),
                WriteFormat::ClearScreen => program.push(Ops::WriteClearScreen as u8),
                WriteFormat::ToCol => {
                    self.children[0].to_bytes(program, identifier_scopes);
                    program.push(Ops::WriteToCol as u8)
                }
            },
            ParserNodeType::FormatExpression => {
                // HashBangFormat
                self.children[0].to_bytes(program, identifier_scopes);

                //Has a FormatExpressionTail
                if self.children.len() == 2 {
                    //FormatExpressionTail
                    self.children[1].to_bytes(program, identifier_scopes)
                }
            }
            ParserNodeType::FormatExpressionTail => {
                // RelationalExpression
                self.children[0].to_bytes(program, identifier_scopes);
            }
            ParserNodeType::HashBangFormat => {
                // WriteFormat
                self.children[0].to_bytes(program, identifier_scopes);

                // Has a HashBangFormat
                if self.children.len() == 2 {
                    // HashBangFormat
                    self.children[1].to_bytes(program, identifier_scopes);
                }
            }
            ParserNodeType::WriteExpressionList => {
                // WriteExpression
                self.children[0].to_bytes(program, identifier_scopes);

                // Has a WriteExpressionListTail
                if self.children.len() == 2 {
                    // ExpressionListTail
                    self.children[1].to_bytes(program, identifier_scopes);
                }
            }
            ParserNodeType::WriteExpressionListTail => {
                // WriteExpression
                self.children[0].to_bytes(program, identifier_scopes);

                // Has a WriteExpressionListTail
                if self.children.len() == 2 {
                    // WriteExpressionListTail
                    self.children[1].to_bytes(program, identifier_scopes)
                }
            }
            ParserNodeType::EqualityExpression => {
                // RelationalExpression
                self.children[0].to_bytes(program, identifier_scopes);

                // Has an EqualityExpressionTail
                if self.children.len() == 2 {
                    // EqualityExpressionTail
                    self.children[1].to_bytes(program, identifier_scopes);
                }
            }
            ParserNodeType::EqualityExpressionTail => {
                // RelationalExpression
                self.children[1].to_bytes(program, identifier_scopes);

                // EqOp
                self.children[0].to_bytes(program, identifier_scopes);

                // Has an EqualityExpressionTail
                if self.children.len() == 3 {
                    // EqualityExpression
                    self.children[2].to_bytes(program, identifier_scopes);
                }
            }
            ParserNodeType::EqOp(op) => match op {
                EqOp::Equals => program.push(Ops::Equals as u8),
                EqOp::NotEquals => program.push(Ops::NotEquals as u8),
            },
            ParserNodeType::RelationalExpression => {
                // Expression
                self.children[0].to_bytes(program, identifier_scopes);

                // Has a RelationalExpressionTail
                if self.children.len() == 2 {
                    // RelationalExpressionTail
                    self.children[1].to_bytes(program, identifier_scopes)
                }
            }
            ParserNodeType::RelationalExpressionTail => {
                // Expression
                self.children[1].to_bytes(program, identifier_scopes);

                // RelOp
                self.children[0].to_bytes(program, identifier_scopes);

                // Has a RelationalExpressionTail
                if self.children.len() == 3 {
                    //RelationalExpressionTail
                    self.children[2].to_bytes(program, identifier_scopes)
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
                self.children[0].to_bytes(program, identifier_scopes);
                // Has an ExpressionTail
                if self.children.len() == 2 {
                    self.children[1].to_bytes(program, identifier_scopes);
                }
            }
            ParserNodeType::ExpressionTail => {
                // Term
                self.children[1].to_bytes(program, identifier_scopes);
                // AddOp
                self.children[0].to_bytes(program, identifier_scopes);
                // Has an ExpressionTail
                if self.children.len() == 3 {
                    // ExpressionTail
                    self.children[2].to_bytes(program, identifier_scopes);
                }
            }
            ParserNodeType::Term => {
                //Factor
                self.children[0].to_bytes(program, identifier_scopes);
                // Has a TermTail
                if self.children.len() == 2 {
                    // TermTail
                    self.children[1].to_bytes(program, identifier_scopes);
                }
            }
            ParserNodeType::TermTail => {
                // Factor
                self.children[1].to_bytes(program, identifier_scopes);
                // MulOp
                self.children[0].to_bytes(program, identifier_scopes);
                // Has a TermTail
                if self.children.len() == 3 {
                    // TermTail
                    self.children[2].to_bytes(program, identifier_scopes);
                }
            }
            ParserNodeType::Unary => {
                // Unary
                self.children[1].to_bytes(program, identifier_scopes);

                // UnaryOp
                self.children[0].to_bytes(program, identifier_scopes)
            }
            ParserNodeType::ExpTerm => {
                // Factor
                self.children[0].to_bytes(program, identifier_scopes);

                // Has an ExponentialTermTail
                if self.children.len() == 2 {
                    // ExponentialTermTail
                    self.children[1].to_bytes(program, identifier_scopes);
                }
            }
            ParserNodeType::ExpTermTail => {
                // Factor
                self.children[1].to_bytes(program, identifier_scopes);

                // Exponential
                self.children[0].to_bytes(program, identifier_scopes);

                // Has an ExponentialTermTail
                if self.children.len() == 3 {
                    // ExponentialTermTail
                    self.children[2].to_bytes(program, identifier_scopes);
                }
            }
            ParserNodeType::ExpOp => program.push(Ops::Exp as u8),
            ParserNodeType::Factor => {
                // NumericLiteral | StringLiteral | ( Expression ) | Identifier
                if let Identifier(_) = self.children[0].node_type {
                    program.push(Ops::Get as u8);
                }
                self.children[0].to_bytes(program, identifier_scopes);
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
                let var_definition = identifier_scopes.get_variable(identifier.to_string());

                if let None = var_definition {
                    panic!("Undefined variable {}.", identifier);
                }
                let position_bytes = var_definition.unwrap().position.to_le_bytes();
                for byte in position_bytes {
                    program.push(byte);
                }
            }
            ParserNodeType::Type(type_obj) => program.push(match type_obj {
                Type::String => MValType::String as u8,
                Type::Int => MValType::Int as u8,
                Type::Double => MValType::Double as u8,
            }),
        }
    }

    pub fn get_bounds(&self) -> (usize, usize, usize) {
        if self.children.len() == 0 {
            return (
                self.token.unwrap().start,
                self.token.unwrap().end,
                self.token.unwrap().line,
            );
        }

        // TODO: This approach of identifying bounds is a bit too aggressive. We should try
        //       to find a method by which we don't capture unnecessary content
        let first_child = &self.children[0];
        let last_child = &self.children[self.children.len() - 1];
        let (first_child_start, line) = first_child.get_start();

        return (first_child_start, last_child.get_end(), line);
    }

    fn get_start(&self) -> (usize, usize) {
        if self.children.len() == 0 {
            return (self.token.unwrap().start, self.token.unwrap().line);
        }

        return self.children[0].get_start();
    }

    fn get_end(&self) -> usize {
        if self.children.len() == 0 {
            return self.token.unwrap().end;
        }

        return self.children[self.children.len() - 1].get_end();
    }

    /// Back patch a jump to the final value. Validates jump is not larger than max jump size.
    /// # Parameters
    /// * `program` - Program vector to update
    /// * `start_pos` - Where the patch should start
    /// * `jump_offset` - Length of the jump
    fn back_patch_jump(&self, program: &mut Vec<u8>, start_pos: usize, jump_offset: usize) {
        if jump_offset > std::u16::MAX as usize {
            panic!("Offset greater than max jump length")
        }
        let offset_bytes: [u8; 2] = (jump_offset as u16).to_le_bytes();
        program[start_pos - 2] = offset_bytes[0];
        program[start_pos - 1] = offset_bytes[1];
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

            ParserNodeType::ForStatement => write!(f, "ForStatement"),
            ParserNodeType::ForBound => write!(f, "ForBound"),

            ParserNodeType::IfStatement => write!(f, "IfStatement"),
            ParserNodeType::ElseStatement => write!(f, "ElseStatement"),

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

            ParserNodeType::EqualityExpression => write!(f, "EqualityExpression"),
            ParserNodeType::EqualityExpressionTail => write!(f, "EqualityExpressionTail"),
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
            ParserNodeType::EqOp(op) => write!(f, "EqOp: {:?}", op),
            ParserNodeType::ExpOp => write!(f, "ExpOp"),
            ParserNodeType::ExpTerm => write!(f, "ExpTerm"),
            ParserNodeType::ExpTermTail => write!(f, "ExpTermTail"),
            ParserNodeType::StringLiteral(value) => write!(f, "StringLiteral: {}", value),
            ParserNodeType::NumericLiteral(value) => write!(f, "NumericLiteral: {}", value),
            ParserNodeType::Identifier(var_name) => write!(f, "Identifier: {}", var_name),
            ParserNodeType::Type(type_name) => write!(f, "Type: {:?}", type_name),
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

    IfStatement,

    ElseStatement,

    ForStatement,
    ForBound,

    EqualityExpression,
    EqualityExpressionTail,

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
    EqOp(EqOp),
    ExpOp,

    ExpTerm,
    ExpTermTail,

    NumericLiteral(MVal),
    StringLiteral(MVal),
    Identifier(&'a str),
    Type(Type),
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
pub enum RelOp {
    GreaterThan,
    LessThan,
    GreaterThanOrEqualTo,
    LessThanOrEqualTo,
}

#[derive(Debug, PartialEq)]
pub enum EqOp {
    Equals,
    NotEquals,
}

#[derive(Debug, PartialEq)]
pub enum WriteFormat {
    NewLine,
    ClearScreen,
    ToCol,
}

#[derive(Debug, PartialEq)]
pub enum Type {
    String,
    Double,
    Int,
}

impl Type {
    pub fn to_mval_type(&self) -> MValType {
        match self {
            Type::String => MValType::String,
            Type::Int => MValType::Int,
            Type::Double => MValType::Double,
        }
    }
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
