/*
 * Copyright (c) 2022, Jonah Shafran.
 *
 * SPDX-License-Identifier: GPL-3.0-only
 */
//
// use crate::parser::ParserNode;
// use crate::parser::ParserNodeType;
// use crate::runtime::MValType;
// use crate::{Scopes, VariableDefinition};
// use std::collections::HashMap;
//
// pub fn check_types(node: &mut ParserNode, variable_map: &Scopes) -> Result<(), TypeError> {
//     return match &node.node_type {
//         ParserNodeType::NumericLiteral(num) => {
//             node.value_type = Some(num.value_type);
//             Ok(())
//         }
//         ParserNodeType::StringLiteral(_) => {
//             node.value_type = Some(MValType::String);
//             Ok(())
//         }
//         ParserNodeType::Identifier(identifier) => {
//             node.value_type = Some(
//                 variable_map
//                     .get(&identifier.to_string())
//                     .expect("Missing declaration for variable in variable_map")
//                     .val_type,
//             );
//             Ok(())
//         }
//         ParserNodeType::Type(type_val) => {
//             node.value_type = Some(type_val.to_mval_type());
//             Ok(())
//         }
//
//         //TODO: This is kind of messy, can we come up with a more maintainable solution?
//         ParserNodeType::AddOp(_)
//         | ParserNodeType::MulOp(_)
//         | ParserNodeType::UnaryOp(_)
//         | ParserNodeType::ExpOp
//         | ParserNodeType::EqOp(_)
//         | ParserNodeType::RelOp(_)
//         | ParserNodeType::WriteFormat(_)
//         | ParserNodeType::IdentifierList
//         | ParserNodeType::IdentifierListTail => Ok(()),
//
//         ParserNodeType::WriteExpressionList | ParserNodeType::WriteExpressionListTail => {
//             // Each item of a write expression list can be of a different type, but
//             // the items themselves need to be type checked
//             for child in &mut node.children {
//                 check_types(child, variable_map)?;
//             }
//
//             Ok(())
//         }
//
//         _ => {
//             for child in &mut node.children {
//                 check_types(child, variable_map)?;
//             }
//
//             let mut val_type: Option<MValType> = None;
//             let mut first_node: Option<&ParserNode> = None;
//
//             for child in &node.children {
//                 let child_type = child.value_type;
//
//                 if let Some(child_type) = child_type {
//                     if val_type == None {
//                         val_type = Some(child_type);
//                         first_node = Some(child);
//                     }
//
//                     if child_type != val_type.unwrap() {
//                         if !(child_type.is_numeric_type() && val_type.unwrap().is_numeric_type()) {
//                             return Err(TypeError {
//                                 first_node_type: first_node.unwrap().value_type.unwrap(),
//                                 second_node_type: child_type,
//                                 first_node_bounds: first_node.unwrap().get_bounds(),
//                                 second_node_bounds: child.get_bounds(),
//                             });
//                         }
//                     }
//                 }
//             }
//
//             node.value_type = val_type;
//
//             Ok(())
//         }
//     };
// }
//
// pub fn print_type_error(error: TypeError, input: &str) {
//     let lines: Vec<&str> = input.lines().collect();
//     let (first_node_start, first_node_end, first_node_line) = error.first_node_bounds;
//     let (second_node_start, second_node_end, second_node_line) = error.second_node_bounds;
//
//     eprintln!("{}:{}", first_node_start, first_node_end);
//     eprintln!("{}", lines[first_node_line]);
//     for _ in 0..first_node_start {
//         eprint!(" ");
//     }
//     for _ in first_node_start..first_node_end {
//         eprint!("^");
//     }
//     eprintln!(" Mismatched types {:?}", error.first_node_type);
//
//     eprintln!("{}:{}", second_node_start, second_node_end);
//     eprintln!("{}", lines[second_node_line]);
//     for _ in 0..second_node_start {
//         eprint!(" ");
//     }
//     for _ in second_node_start..second_node_end {
//         eprint!("^");
//     }
//     eprintln!(" and {:?}", error.second_node_type);
// }
//
// #[derive(Debug)]
// pub struct TypeError {
//     first_node_type: MValType,
//     second_node_type: MValType,
//     first_node_bounds: (usize, usize, usize),
//     second_node_bounds: (usize, usize, usize),
// }
