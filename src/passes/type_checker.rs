/*
 * Copyright (c) 2022, Jonah Shafran.
 *
 * SPDX-License-Identifier: GPL-3.0-only
 */

use crate::parser::ParserNode;
use crate::parser::ParserNodeType;
use crate::runtime::MValType;
use crate::VariableDefinition;
use std::collections::HashMap;

pub fn check_types(
    node: &mut ParserNode,
    variable_map: &HashMap<String, VariableDefinition>,
) -> Result<(), TypeError> {
    match &node.node_type {
        ParserNodeType::NumericLiteral(num) => {
            node.value_type = Some(num.value_type);
            return Ok(());
        }
        ParserNodeType::StringLiteral(_) => {
            node.value_type = Some(MValType::String);
            return Ok(());
        }
        ParserNodeType::Identifier(identifier) => {
            node.value_type = Some(
                variable_map
                    .get(&identifier.to_string())
                    .expect("Missing declaration for variable in variable_map")
                    .val_type,
            );
            return Ok(());
        }
        ParserNodeType::Type(type_val) => {
            node.value_type = Some(type_val.to_mval_type());
            return Ok(());
        }

        //TODO: This is kind of messy, can we come up with a more maintainable solution?
        ParserNodeType::AddOp(_)
        | ParserNodeType::MulOp(_)
        | ParserNodeType::UnaryOp(_)
        | ParserNodeType::ExpOp
        | ParserNodeType::EqOp(_)
        | ParserNodeType::RelOp(_)
        | ParserNodeType::IdentifierList
        | ParserNodeType::IdentifierListTail
        | ParserNodeType::AssignmentList
        | ParserNodeType::AssignmentListTail
        | ParserNodeType::WriteExpressionList
        | ParserNodeType::WriteExpression
        | ParserNodeType::WriteFormat(_) => {
            return Ok(());
        }

        _ => {
            for child in &mut node.children {
                check_types(child, variable_map)?;
            }

            let mut val_type: Option<MValType> = None;

            for child in &node.children {
                let child_type = child.value_type;

                if let Some(child_type) = child_type {
                    if val_type == None {
                        val_type = Some(child_type);
                    }

                    if child_type != val_type.unwrap() {
                        return Err(TypeError {
                            // TODO: Make this more user friendly & informative
                            message: format!(
                                "Mismatched types {} and {:?} in {}",
                                if let Some(val_type) = val_type {
                                    format!("{:?}", val_type)
                                } else {
                                    "Unknown".to_string()
                                },
                                child_type,
                                node
                            ),
                        });
                    }
                }
            }

            node.value_type = val_type;

            return Ok(());
        }
    }
}

#[derive(Debug)]
pub struct TypeError {
    pub message: String,
}
