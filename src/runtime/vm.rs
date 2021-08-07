/*
 * Copyright (c) 2021, Jonah Shafran.
 *
 * SPDX-License-Identifier: GPL-3.0-only
 */

use crate::runtime::mval::MVal;
use crate::runtime::Ops;
use crossterm::cursor::{position, MoveTo};
use crossterm::terminal::{Clear, ClearType};
use crossterm::ExecutableCommand;
use rust_decimal::prelude::ToPrimitive;
use rust_decimal::Decimal;
use std::io;
use std::io::{stdout, Write};

pub struct VM {
    stack: Vec<MVal>,
    program: Vec<u8>,
    program_counter: usize,
    print_status: bool,
}

impl VM {
    pub fn new(program: Vec<u8>, print_status: bool) -> VM {
        VM {
            stack: Vec::new(),
            program,
            program_counter: 0,
            print_status,
        }
    }

    pub fn execute(&mut self) {
        while self.program_counter < self.program.len() {
            if self.print_status {
                println!("PC: {} Stack: {:?}", self.program_counter, self.stack);
            }
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
                Ops::Exp => self.execute_exponent(),
                Ops::Write => self.execute_write(),
                Ops::WriteLine => self.execute_write_line(),
                Ops::WriteClearScreen => self.execute_write_clear_screen(),
                Ops::WriteToCol => self.execute_write_to_col(),
                Ops::LessThan => self.execute_less_than(),
                Ops::GreaterThan => self.execute_greater_than(),
                Ops::LessThanOrEqualTo => self.execute_less_than_or_equal_to(),
                Ops::GreaterThanOrEqualTo => self.execute_greater_than_or_equal_to(),
                Ops::Not => self.execute_not(),
            }
        }
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

    fn execute_exponent(&mut self) {
        let rhs = self.stack.pop().expect("No rhs for exponent");
        let lhs = self.stack.pop().expect("No lhs for exponent");

        self.stack.push(lhs.exponent(rhs));
        self.program_counter += 1;
    }

    fn execute_to_number(&mut self) {
        let operand = self.stack.pop().expect("No operand for to number");

        self.stack.push(MVal::from_string_no_sanitize(
            operand.numeric_interpretation().to_string(),
        ));
        self.program_counter += 1;
    }

    fn execute_to_negative_number(&mut self) {
        let operand = self.stack.pop().expect("No operand for to negative number");

        self.stack.push(MVal::from_string_no_sanitize(
            (operand.numeric_interpretation() * Decimal::from(-1)).to_string(),
        ));
        self.program_counter += 1;
    }

    fn execute_write(&mut self) {
        let operand = self.stack.pop().expect("No operand for write");

        print!("{}", operand.string_interpretation());
        io::stdout().flush().expect("Issue flushing stdout");
        self.program_counter += 1;
    }

    fn execute_write_line(&mut self) {
        println!();
        self.program_counter += 1;
    }

    fn execute_write_clear_screen(&mut self) {
        stdout()
            .execute(Clear(ClearType::All))
            .expect("Unable to clear screen")
            .execute(MoveTo(0, 0))
            .expect("Unable to move cursor to 0 0");
        io::stdout().flush().expect("Issue flushing stdout");
        self.program_counter += 1
    }

    fn execute_write_to_col(&mut self) {
        let (x, y) = position().expect("Unable to read cursor position");

        let col = self
            .stack
            .pop()
            .expect("No operand for write to col")
            .numeric_interpretation()
            .to_u16()
            .unwrap();

        if x < col {
            stdout()
                .execute(MoveTo(col, y))
                .expect(&*format!("Unable to move cursor to {} {} ", col, y));
        }
        self.program_counter += 1
    }

    fn execute_less_than(&mut self) {
        let rhs = self.stack.pop().expect("No rhs for less than");
        let lhs = self.stack.pop().expect("No lhs for less than");

        self.stack.push(lhs.less_than(rhs));
        self.program_counter += 1;
    }

    fn execute_greater_than(&mut self) {
        let rhs = self.stack.pop().expect("No rhs for greater than");
        let lhs = self.stack.pop().expect("No lhs for greater than");

        self.stack.push(lhs.greater_than(rhs));
        self.program_counter += 1;
    }

    fn execute_less_than_or_equal_to(&mut self) {
        let rhs = self.stack.pop().expect("No rhs for less than or equal to");
        let lhs = self.stack.pop().expect("No lhs for less than or equal to");

        self.stack.push(lhs.less_than_or_equal_to(rhs));
        self.program_counter += 1;
    }

    fn execute_greater_than_or_equal_to(&mut self) {
        let rhs = self
            .stack
            .pop()
            .expect("No rhs for greater than or equal to");
        let lhs = self
            .stack
            .pop()
            .expect("No lhs for greater than or equal to");

        self.stack.push(lhs.greater_than_or_equal_to(rhs));
        self.program_counter += 1;
    }

    fn execute_not(&mut self) {
        let operand = self.stack.pop().expect("No operand for not");

        self.stack.push(operand.not());
        self.program_counter += 1;
    }
}
