/*
 * Copyright (c) 2022, Jonah Shafran.
 *
 * SPDX-License-Identifier: GPL-3.0-only
 */

use crate::runtime::MValType;
use std::collections::HashMap;

#[derive(Debug)]
pub struct VariableDefinition {
    pub val_type: MValType,
    pub position: usize,
}

#[derive(Debug)]
pub struct FunctionDefinition {
    pub return_type: MValType,
    pub params_types: Vec<ParameterDefinition>,
    pub position: usize,
    pub callers: Vec<usize>,
}

#[derive(Debug)]
pub struct ParameterDefinition {
    pub name: String,
    pub val_type: MValType,
}

#[derive(Debug)]
pub struct Scope {
    variables: HashMap<String, VariableDefinition>,
    functions: HashMap<String, FunctionDefinition>,
}

impl Scope {
    pub fn new() -> Scope {
        Scope {
            variables: HashMap::new(),
            functions: HashMap::new(),
        }
    }

    pub fn add_variable(&mut self, variable_name: String, val_type: MValType) {
        if self.functions.contains_key(&variable_name) {
            panic!(
                "Variable {} already defined as a function in the same scope!",
                variable_name
            )
        }

        if self.variables.contains_key(&variable_name) {
            panic!("Variable {} already defined!", &variable_name)
        }
        self.variables.insert(
            variable_name,
            VariableDefinition {
                val_type,
                position: self.variables.len(),
            },
        );
    }

    pub fn add_function(
        &mut self,
        function_name: String,
        return_type: MValType,
        params_types: Vec<ParameterDefinition>,
        position: usize,
    ) {
        if self.variables.contains_key(&function_name) {
            panic!(
                "Function {} already defined as a variable in the same scope!",
                function_name
            )
        }

        if self.functions.contains_key(&function_name) {
            panic!("Function {} already defined!", function_name)
        }
        self.functions.insert(
            function_name,
            FunctionDefinition {
                return_type,
                params_types,
                position,
                callers: Vec::new(),
            },
        );
    }

    pub fn get_variable(&self, variable_name: &str) -> Option<&VariableDefinition> {
        self.variables.get(variable_name)
    }

    pub fn get_function(&self, function_name: &str) -> Option<&FunctionDefinition> {
        self.functions.get(function_name)
    }

    pub fn get_function_mut(&mut self, function_name: &str) -> Option<&mut FunctionDefinition> {
        self.functions.get_mut(function_name)
    }
}

#[derive(Debug)]
pub struct Scopes {
    scopes: Vec<Scope>,
}

impl Scopes {
    pub fn new() -> Scopes {
        Scopes {
            scopes: vec![Scope::new()],
        }
    }

    pub fn begin_scope(&mut self) {
        self.scopes.push(Scope::new());
    }

    pub fn end_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn patch_function_calls(&self, program: &mut Vec<u8>) {
        for scope in &self.scopes {
            for (_, function) in &scope.functions {
                for caller in &function.callers {
                    self.patch_function_call(caller, function.position, program)
                }
            }
        }
    }

    fn patch_function_call(
        &self,
        caller_position: &usize,
        function_position: usize,
        program: &mut Vec<u8>,
    ) {
        let mut position = *caller_position;
        for byte in function_position.to_le_bytes() {
            program[position] = byte;
            position += 1;
        }
    }

    pub fn get_variable(&self, variable_name: String) -> Option<&VariableDefinition> {
        for scope in self.scopes.iter().rev() {
            let result = scope.get_variable(&variable_name);
            if result.is_some() {
                return result;
            }
        }

        return None;
    }

    pub fn get_function(&self, function_name: String) -> Option<&FunctionDefinition> {
        // Functions should only be declared at top level scope
        let scope = self.scopes.first().unwrap();
        scope.get_function(&function_name)
    }

    pub fn get_function_mut(&mut self, function_name: String) -> Option<&mut FunctionDefinition> {
        // Functions should only be declared at top level scope
        let scope = self.scopes.first_mut().unwrap();
        scope.get_function_mut(&function_name)
    }

    pub fn add_variable(&mut self, variable_name: String, val_type: MValType) {
        let current_scope = self.scopes.last_mut().unwrap();
        current_scope.add_variable(variable_name, val_type)
    }

    pub fn add_function(&mut self, function_name: String, return_type: MValType, position: usize) {
        let current_scope = self.scopes.last_mut().unwrap();
        current_scope.add_function(function_name, return_type, Vec::new(), position)
    }
}
