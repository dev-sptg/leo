// Copyright (C) 2019-2021 Aleo Systems Inc.
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

//! Enforces an array expression in a compiled Leo program.

use crate::{errors::ExpressionError, program::ConstrainedProgram, value::ConstrainedValue, GroupType};
use leo_asg::{Expression, Span};
use std::sync::Arc;

use snarkvm_models::{
    curves::{Field, PrimeField},
    gadgets::r1cs::ConstraintSystem,
};

impl<F: Field + PrimeField, G: GroupType<F>> ConstrainedProgram<F, G> {
    /// Enforce array expressions
    pub fn enforce_array<CS: ConstraintSystem<F>>(
        &mut self,
        cs: &mut CS,
        array: &[(Arc<Expression>, bool)],
        span: Span,
    ) -> Result<ConstrainedValue<F, G>, ExpressionError> {
        let expected_dimension = None;

        let mut result = vec![];
        for (element, is_spread) in array.iter() {
            let element_value = self.enforce_expression(cs, element)?;
            if *is_spread {
                match element_value {
                    ConstrainedValue::Array(array) => result.extend(array),
                    _ => unimplemented!(), // type should already be checked
                }
            } else {
                result.push(element_value);
            }
        }

        // Check expected_dimension if given.
        if let Some(dimension) = expected_dimension {
            // Return an error if the expected dimension != the actual dimension.
            if dimension != result.len() {
                return Err(ExpressionError::invalid_length(dimension, result.len(), span));
            }
        }

        Ok(ConstrainedValue::Array(result))
    }

    ///
    /// Returns an array value from an array initializer expression.
    ///
    #[allow(clippy::too_many_arguments)]
    pub fn enforce_array_initializer<CS: ConstraintSystem<F>>(
        &mut self,
        cs: &mut CS,
        element_expression: &Arc<Expression>,
        actual_size: usize,
    ) -> Result<ConstrainedValue<F, G>, ExpressionError> {
        let mut value = self.enforce_expression(cs, element_expression)?;

        // Allocate the array.
        let array = vec![value; actual_size];

        // Set the array value.
        value = ConstrainedValue::Array(array);

        Ok(value)
    }
}
