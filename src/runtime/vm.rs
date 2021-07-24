/*
 * Copyright (c) 2021, Jonah Shafran.
 *
 * SPDX-License-Identifier: GPL-3.0-only
 */

use crate::runtime::mval::MVal;
use crate::runtime::Ops;

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
        let rhs = self.stack.pop().unwrap();
        let lhs = self.stack.pop().unwrap();

        self.stack.push(lhs + rhs);
        self.program_counter += 1;
    }

    fn execute_subtract(&mut self) {
        let rhs = self.stack.pop().unwrap();
        let lhs = self.stack.pop().unwrap();

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
        let rhs = self.stack.pop().unwrap();
        let lhs = self.stack.pop().unwrap();

        self.stack.push(lhs.modulo(rhs));
        self.program_counter += 1;
    }
}
