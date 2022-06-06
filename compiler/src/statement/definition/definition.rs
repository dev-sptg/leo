// Copyright (C) 2019-2022 Aleo Systems Inc.
// This file is part of the Leo library.

// The Leo library is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.

// The Leo library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with the Leo library. If not, see <https://www.gnu.org/licenses/>.

//! Enforces a definition statement in a compiled Leo program.

use crate::program::Program;
use leo_asg::{CircuitMember, DefinitionStatement, Expression, Type, Variable};
use leo_asg::CircuitMember::{Const, Function};
use leo_errors::{CompilerError, Result};
use snarkvm_debugdata::{DebugInstruction, DebugVariable, DebugVariableType};
use snarkvm_ir::{Instruction, Integer, QueryData, Value};

impl<'a> Program<'a> {
    fn enforce_multiple_definition(&mut self, variable_names: &[&'a Variable<'a>], values: Value) -> Result<()> {
        for (i, variable) in variable_names.iter().enumerate() {
            let target = self.alloc_var(variable);
            self.emit(Instruction::TupleIndexGet(QueryData {
                destination: target,
                values: vec![values.clone(), Value::Integer(Integer::U32(i as u32))],
            }));
        }

        Ok(())
    }

    #[allow(clippy::too_many_arguments)]
    pub fn enforce_definition_statement(&mut self, statement: &DefinitionStatement<'a>) -> Result<()> {
        let num_variables = statement.variables.len();
        let func_index = self.resolve_function(self.current_function.expect("return in non-function"));
        let line_start =  *&statement.span.clone().unwrap_or_default().line_start as u32;
        let line_end =  *&statement.span.clone().unwrap_or_default().line_stop as u32;

        self.current_dbg_func = func_index;
        let expression = self.enforce_expression(statement.value.get())?;

        if num_variables == 1 {
            let variable = statement.variables.get(0).unwrap();
            // Define a single variable with a single value
            self.alloc_var(variable);

            let line_start = *&statement.span.clone().unwrap_or_default().line_start as u32;
            let line_end = *&statement.span.clone().unwrap_or_default().line_stop as u32;
    
            self.store(variable, expression);
            self.insert_instruction(func_index, line_start);
            let id = self.resolve_var(variable);
            match variable.clone().borrow().type_.clone() {
                Type::Address => {}
                Type::Boolean => {
                    let dbg_var = DebugVariable::new_some_variable(DebugVariableType::Boolean, String::from(variable.borrow().name.to_string()), "Value is currently unavailable".to_string(), line_start, line_end);
                    self.debug_data.add_variable(id, dbg_var);
                    self.debug_data.add_variable_to_function(self.current_dbg_func, id);
                }
                Type::Char => {}
                Type::Field => {
                    let dbg_var = DebugVariable::new_some_variable(DebugVariableType::Group, String::from(variable.borrow().name.to_string()), "Value is currently unavailable".to_string(), line_start, line_end);
                    self.debug_data.add_variable(id, dbg_var);
                    self.debug_data.add_variable_to_function(self.current_dbg_func, id);
                }
                Type::Group => {
                    let dbg_var = DebugVariable::new_some_variable(DebugVariableType::Group, String::from(variable.borrow().name.to_string()), "Value is currently unavailable".to_string(), line_start, line_end);
                    self.debug_data.add_variable(id, dbg_var);
                    self.debug_data.add_variable_to_function(self.current_dbg_func, id);
                }
                Type::Integer(_) => {
                    let dbg_var = DebugVariable::new_some_variable(DebugVariableType::Integer, String::from(variable.borrow().name.to_string()), "Value is currently unavailable".to_string(), line_start, line_end);
                    self.debug_data.add_variable(id, dbg_var);
                    self.debug_data.add_variable_to_function(self.current_dbg_func, id);
                }
                Type::Array(type_, len) => {
                    let mut dbg_var = DebugVariable::new_some_variable(DebugVariableType::Array, String::from(variable.borrow().name.to_string()), "Array".to_string(), line_start, line_end);
                    for index in  0..len {
                        let mut sub_var = DebugVariable::new_some_variable(DebugVariableType::Integer, format!("[{}]", index), format!("{}", type_), 0, 0);
                        self.resolve_circuit(&*type_, &mut sub_var);
                        dbg_var.sub_variables.push(sub_var);
                    }

                    self.debug_data.debug_variable = DebugVariable::new();
                    self.debug_data.add_variable(id, dbg_var);
                    self.debug_data.add_variable_to_function(self.current_dbg_func, id);
                }
                Type::ArrayWithoutSize(_) => {}
                Type::Tuple(items) => {
                    let mut dbg_var =DebugVariable::new_some_variable(DebugVariableType::Tuple, String::from(variable.borrow().name.to_string()), "Tuple".to_string(), line_start, line_end);
                    let mut index = 0;
                    for item in items {
                        let mut sub_var = DebugVariable::new_some_variable(DebugVariableType::Integer, format!("{}", index), "".to_string(), 0, 0);
                        self.resolve_circuit(&item, &mut sub_var);
                        dbg_var.sub_variables.push(sub_var);
                        index += 1;
                    }

                    self.debug_data.add_variable(id, dbg_var);
                    self.debug_data.add_variable_to_function(self.current_dbg_func, id);
                }
                Type::Circuit(circuit) => {
                    let mut dbg_var = DebugVariable::new_circuit(String::from(variable.borrow().name.to_string()), circuit.name.borrow().name.to_string(), self.debug_data.last_circuit_id, line_start, line_end);
                    let members = circuit.members.borrow();
                    for (name, member) in members.iter() {
                        match  member {
                            CircuitMember::Variable(var) => {
                                let mut sub_var = DebugVariable::new_some_variable(DebugVariableType::Integer, name.to_string(), "Value is currently unavailable".to_string(), 0, 0);
                                self.resolve_circuit(&var, &mut sub_var);
                                dbg_var.sub_variables.push(sub_var);
                            }
                            _=> {}
                        }
                    }

                    self.debug_data.add_variable(id, dbg_var);
                    self.debug_data.add_variable_to_function(self.current_dbg_func, id);
                }
                Type::Err => {}
            }
            Ok(())
        } else {
            self.enforce_multiple_definition(&statement.variables[..], expression)
        }
    }

    pub fn resolve_circuit(&mut self, var: &Type<'a>, variable:  &mut DebugVariable) {
        match var {
            Type::Tuple(items) => {
                variable.type_ = DebugVariableType::Tuple;
                variable.value = "Tuple".to_string();
                let mut index = 0;
                for item in items {
                    let mut sub_var = DebugVariable::new_some_variable(DebugVariableType::Integer, format!("{}", index), "".to_string(), 0, 0);
                    self.resolve_circuit(&item, &mut sub_var);
                    variable.sub_variables.push(sub_var);
                    index += 1;
                }
            }
            Type::Array(type_, len) => {
                variable.type_ = DebugVariableType::Array;
                variable.value = "Array".to_string();
                for index in  0..*len {
                    let mut sub_var = DebugVariable::new_some_variable(DebugVariableType::Integer, format!("[{}]", index), format!("{}", type_), 0, 0);
                    self.resolve_circuit(&*type_, &mut sub_var);
                    variable.sub_variables.push(sub_var);
                }
            }
            Type::Circuit(circuit) => {
                variable.type_ = DebugVariableType::Circuit;
                variable.value = circuit.name.borrow().name.to_string();
                let members = circuit.members.borrow();
                for (name, member) in members.iter() {
                    match  member {
                        CircuitMember::Variable(var) => {
                            let mut sub_var = DebugVariable::new_some_variable(DebugVariableType::Integer, format!("{}", name), "Value is currently unavailable".to_string(), 0, 0);
                            self.resolve_circuit(&var, &mut sub_var);
                            variable.sub_variables.push(sub_var);
                        }
                        _=> {
                            
                        }
                    }
                }

            }
            Type::Group => {
                variable.type_ = DebugVariableType::Group;
                variable.value = "group".to_string();
            }
            Type::Err => {}
            _=> {
                variable.value = "Value is currently unavailable".to_string();
            }
        }

    }
}
