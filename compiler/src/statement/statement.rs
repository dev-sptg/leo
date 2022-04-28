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

//! Enforces a statement in a compiled Leo program.

use crate::program::Program;
use leo_asg::Statement;
use leo_errors::Result;
use snarkvm_debugdata::DebugInstruction;
use snarkvm_ir::Value;

pub type StatementResult<T> = Result<T>;

impl<'a> Program<'a> {
    ///
    /// Enforce a program statement.
    /// Returns a Vector of (indicator, value) tuples.
    /// Each evaluated statement may execute of one or more statements that may return early.
    /// To indicate which of these return values to take we conditionally select the value according
    /// to the `indicator` bit that evaluates to true.
    ///
    #[allow(clippy::too_many_arguments)]
    pub fn enforce_statement(&mut self, statement: &'a Statement<'a>) -> StatementResult<()> {
        match statement {
            Statement::Return(statement) => {
                self.enforce_return_statement(statement)?;
            }
            Statement::Definition(statement) => {
                self.enforce_definition_statement(statement)?;
            }
            Statement::Assign(statement) => {
                self.enforce_assign_statement(statement)?;
            }
            Statement::Conditional(statement) => {
                self.enforce_conditional_statement(statement)?;
            }
            Statement::Iteration(statement) => {
                self.enforce_iteration_statement(statement)?;
            }
            Statement::Console(statement) => {
                self.evaluate_console_function_call(statement)?;
                let line_start =  *&statement.span.clone().unwrap_or_default().line_start as u32;
                let line_end =  *&statement.span.clone().unwrap_or_default().line_stop as u32;
                let func_index = self.resolve_function(self.current_function.expect("return in non-function"));

                let instruction_index = self.current_instructions_index() - 1;
                self.debug_data.insert_instruction(func_index, instruction_index , DebugInstruction {
                    self_var_id: 0,
                    line_start,
                    line_end,
                });
            }
            Statement::Expression(statement) => {
                let _value = self.enforce_expression(statement.expression.get())?;
                let line_start =  *&statement.span.clone().unwrap_or_default().line_start as u32;
                let line_end =  *&statement.span.clone().unwrap_or_default().line_stop as u32;
                let func_index = self.resolve_function(self.current_function.expect("return in non-function"));

                let value = _value.clone();
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
        
            }
            Statement::Block(statement) => {
                self.evaluate_block(statement)?;
            }
            Statement::Empty(_) => (),
        };

        Ok(())
    }
}
