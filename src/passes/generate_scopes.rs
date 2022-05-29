/*
 * Copyright (c) 2022, Jonah Shafran.
 *
 * SPDX-License-Identifier: GPL-3.0-only
 */

use crate::parser::{ParameterDefinition, ParserNode, ParserNodeType};
use crate::Scopes;

pub fn generate_scopes(root: &ParserNode) -> Scopes {
    let mut scopes = Scopes::new();
    generate_scopes_recursive(root, &mut scopes, &None);
    scopes
}

fn generate_scopes_recursive(
    node: &ParserNode,
    mut scopes: &mut Scopes,
    current_function_name: &Option<&str>,
) {
    match node.node_type {
        ParserNodeType::FunctionDefinition => {
            if let ParserNodeType::Identifier(function_name) = node.children[0].node_type {
                let has_params = node.children.len() == 4;
                if let ParserNodeType::Type(function_type) =
                    &node.children[if has_params { 2 } else { 1 }].node_type
                {
                    scopes.add_function(function_name.to_string(), function_type.to_mval_type(), 0)
                } else {
                    unreachable!()
                }

                if has_params {
                    generate_scopes_recursive(&node.children[1], scopes, &Some(function_name))
                }

                scopes.begin_scope();
                generate_scopes_recursive(
                    &node.children[if has_params { 3 } else { 2 }],
                    scopes,
                    &None,
                );
                scopes.end_scope()
            } else {
                unreachable!()
            }
        }
        ParserNodeType::ParamList | ParserNodeType::ParamListTail => {
            let mut function_definition = scopes
                .get_function_mut(current_function_name.unwrap().to_string())
                .unwrap();
            if let ParserNodeType::Identifier(parameter_name) = node.children[0].node_type {
                if let ParserNodeType::Type(param_type) = &node.children[1].node_type {
                    function_definition.params_types.push(ParameterDefinition {
                        name: parameter_name.to_string(),
                        val_type: param_type.to_mval_type(),
                    });

                    if node.children.len() == 3 {
                        generate_scopes_recursive(&node.children[2], scopes, current_function_name)
                    }
                } else {
                    unreachable!()
                }
            } else {
                unreachable!()
            }
        }
        // ParserNodeType::NewStatement => {}
        _ => {
            for child in &node.children {
                generate_scopes_recursive(child, &mut scopes, current_function_name)
            }
        }
    }
}
