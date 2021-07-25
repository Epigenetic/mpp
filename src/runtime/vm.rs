/*
 * Copyright (c) 2021, Jonah Shafran.
 *
 * SPDX-License-Identifier: GPL-3.0-only
 */

use crate::runtime::mval::MVal;
use crate::runtime::Ops;
use rust_decimal::Decimal;

pub struct VM {
    stack: Vec<MVal>,
    program: Vec<u8>,
    program_counter: usize,
}

impl VM {
    pub fn new(program: Vec<u8>) -> VM {
        VM {
            stack: Vec::new(),
            program,
            program_counter: 0,
        }
    }

    pub fn execute(&mut self) {
        while self.program_counter < self.program.len() {
            println!("{} {:?}", self.program_counter, self.stack);
            let op = Ops::from_u8(self.program[self.program_counter]);

            match op {
                Ops::Push => self.execute_push(),
                Ops::Add => self.execute_add(),
                Ops::Sub => self.execute_subtract(),
                Ops::Mult => self.execute_multiply(),
                Ops::Div => self.execute_divide(),
                Ops::Mod => self.execute_modulus(),
                Ops::IntDiv => self.execute_integer_divide(),
                Ops::ToNum => self.execute_to_number(),
                Ops::ToNegNum => self.execute_to_negative_number(),
            }
        }
        println!("Result {}", self.stack[0])
    }

    fn execute_push(&mut self) {
        self.program_counter += 1;
        let (operand, operand_size) = MVal::from_bytes(&self.program[self.program_counter..]);
        self.stack.push(operand);
        self.program_counter += operand_size + std::mem::size_of::<usize>();
    }

    fn execute_add(&mut self) {
        let rhs = self.stack.pop().expect("No rhs for add");
        let lhs = self.stack.pop().expect("No lhs for add");

        self.stack.push(lhs + rhs);
        self.program_counter += 1;
    }

    fn execute_subtract(&mut self) {
        let rhs = self.stack.pop().expect("No rhs for subtract");
        let lhs = self.stack.pop().expect("No lhs for subtract");

        self.stack.push(lhs - rhs);
        self.program_counter += 1;
    }

    fn execute_multiply(&mut self) {
        let rhs = self.stack.pop().expect("No rhs for multiply");
        let lhs = self.stack.pop().expect("No lhs for multiply");

        self.stack.push(lhs * rhs);
        self.program_counter += 1;
    }

    fn execute_divide(&mut self) {
        let rhs = self.stack.pop().expect("No rhs for divide");
        let lhs = self.stack.pop().expect("No lhs for divide");

        self.stack.push(lhs / rhs);
        self.program_counter += 1;
    }

    fn execute_modulus(&mut self) {
        let rhs = self.stack.pop().expect("No rhs for modulus");
        let lhs = self.stack.pop().expect("No lhs for modulus");

        self.stack.push(lhs.modulo(rhs));
        self.program_counter += 1;
    }

    fn execute_integer_divide(&mut self) {
        let rhs = self.stack.pop().expect("No rhs for integer divide");
        let lhs = self.stack.pop().expect("No lhs for integer divide");

        self.stack.push(lhs.integer_divide(rhs));
        self.program_counter += 1;
    }

    fn execute_to_number(&mut self) {
        let operand = self.stack.pop().expect("No operand for to number");

        self.stack.push(MVal::from_string(
            operand.numeric_interpretation().to_string(),
        ));
        self.program_counter += 1;
    }

    fn execute_to_negative_number(&mut self) {
        let operand = self.stack.pop().expect("No operand for to negative number");

        self.stack.push(MVal::from_string(
            ((operand.numeric_interpretation() * Decimal::from(-1)).to_string()),
        ));
        self.program_counter += 1;
    }
}
