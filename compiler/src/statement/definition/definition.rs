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
        let value = expression.clone();
        match value {
            Value::Address(_) => {}
            Value::Boolean(_) => {}
            Value::Field(_) => {}
            Value::Char(_) => {}
            Value::Group(_) => {}
            Value::Integer(_) => {}
            Value::Array(_) => {}
            Value::Tuple(_) => {}
            Value::Str(_) => {}
            Value::Ref(_id) => {

                let instruction_index = self.current_instructions_index() - 1;
                self.debug_data.insert_instruction(func_index, instruction_index , DebugInstruction {
                    self_var_id: 0,
                    line_start,
                    line_end,
                });
            }
        };


        //let id = self.resolve_var(variable_ref.variable);
       // self.current_dbg_func.variables.insert(id, dbg_var);


        if num_variables == 1 {
            let variable = statement.variables.get(0).unwrap();


            // Define a single variable with a single value
            self.alloc_var(variable);

            let line_start = *&statement.span.clone().unwrap_or_default().line_start as u32;
            let line_end = *&statement.span.clone().unwrap_or_default().line_stop as u32;
    
            self.store(variable, expression);
            let instruction_index = self.current_instructions_index() - 1;
            self.debug_data.insert_instruction(func_index, instruction_index , DebugInstruction {
                self_var_id: 0,
                line_start,
                line_end,
            });
            let id = self.resolve_var(variable);
            match variable.clone().borrow().type_ {
                Type::Address => {}
                Type::Boolean => {
                    let dbg_var = DebugVariable {
                        name: String::from(variable.borrow().name.to_string()),
                        type_: DebugVariableType::Boolean,
                        value: "".to_string(),
                        circuit_id: 0,
                        mutable: false,
                        const_: false,
                        line_start: *&statement.span.clone().unwrap_or_default().line_start as u32,
                        line_end: *&statement.span.clone().unwrap_or_default().line_stop as u32,
                        sub_variables: Vec::new()
                    };
                    self.debug_data.add_variable(id, dbg_var);
                    self.debug_data.add_variable_to_function(self.current_dbg_func, id);
                }
                Type::Char => {}
                Type::Field => {}
                Type::Group => {}
                Type::Integer(_) => {
                    let dbg_var = DebugVariable {
                        name: String::from(variable.borrow().name.to_string()),
                        type_: DebugVariableType::Integer,
                        value: "".to_string(),
                        circuit_id: 0,
                        mutable: false,
                        const_: false,
                        line_start: *&statement.span.clone().unwrap_or_default().line_start as u32,
                        line_end: *&statement.span.clone().unwrap_or_default().line_stop as u32,
                        sub_variables: Vec::new()
                    };

                    self.debug_data.add_variable(id, dbg_var);
                    self.debug_data.add_variable_to_function(self.current_dbg_func, id);
                    //self.add_debug_variable(cur_func, id, DebugItem::Variable(dbg_var));
                }
                Type::Array(_, _) => {
                    let dbg_var = DebugVariable {
                        name: String::from(variable.borrow().name.to_string()),
                        type_: DebugVariableType::Array,
                        value: "Array".to_string(),
                        circuit_id: 0,
                        mutable: false,
                        const_: false,
                        line_start: *&statement.span.clone().unwrap_or_default().line_start as u32,
                        line_end: *&statement.span.clone().unwrap_or_default().line_stop as u32,
                        sub_variables: Vec::new()
                    };

                    self.debug_data.add_variable(id, dbg_var);
                    self.debug_data.add_variable_to_function(self.current_dbg_func, id);
                }
                Type::ArrayWithoutSize(_) => {}
                Type::Tuple(_) => {}
                Type::Circuit(circuit) => {

                    let mut dbg_var = DebugVariable {
                        name: String::from(variable.borrow().name.to_string()),
                        type_: DebugVariableType::Circuit,
                        value: circuit.name.borrow().name.to_string(),
                        circuit_id: self.debug_data.last_circuit_id,
                        mutable: false,
                        const_: false,
                        line_start: *&statement.span.clone().unwrap_or_default().line_start as u32,
                        line_end: *&statement.span.clone().unwrap_or_default().line_stop as u32,
                        sub_variables: Vec::new()
                    };

                    /*let item = self.debug_data.circuits.get_mut(&self.debug_data.last_circuit_id);
                    match item {
                        Some(circuit) => {
                            for member in &circuit.members {
                                match self.debug_data.variables.get_mut(member) {
                                    Some(variable) => {
                                        dbg_var.sub_variables.push(variable.clone());
                                    }
                                    None => {  },
                                }
                            }
                        },
                        None => {  },
                    }*/

                    //let member = circuit.members.borrow_mut().iter;
                    let members = circuit.members.borrow();
                    for (name, member) in members.iter() {
                        match  member {
                            CircuitMember::Variable(_) => {
                                let sub_var = DebugVariable {
                                    name: name.to_string(),
                                    type_: DebugVariableType::Integer,
                                    value: "".to_string(),
                                    circuit_id: 0,
                                    mutable: false,
                                    const_: false,
                                    line_start: 0,
                                    line_end: 0,
                                    sub_variables: Vec::new()
                                };
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

            //let id = self.resolve_var(variable);
            //cur_func.variables.insert(id, dbg_var);
            Ok(())
        } else {
            self.enforce_multiple_definition(&statement.variables[..], expression)
        }
    }
}
