/*
 * Copyright (c) 2021, Jonah Shafran.
 *
 * SPDX-License-Identifier: GPL-3.0-only
 */

use crate::runtime::MVal;
use rust_decimal::Decimal;

#[repr(u8)]
pub enum Ops {
    Push = 1,
    Add = 2,
    Sub = 3,
    Mult = 4,
    Div = 5,
}

impl Ops {
    pub fn from_u8(value: u8) -> Ops {
        match value {
            1 => Ops::Push,
            2 => Ops::Add,
            3 => Ops::Sub,
            4 => Ops::Mult,
            5 => Ops::Div,
            _ => panic!("Unrecognized op code"),
        }
    }
}

pub fn print_program(program: &Vec<u8>) {
    let mut index = 0;

    while index < program.len() {
        match Ops::from_u8(program[index]) {
            Ops::Push => {
                let (value, offset) = MVal::from_bytes(&program[index + 1..]);
                println!("PUSH: {}", value);
                index += offset + std::mem::size_of::<usize>() + 1;
            }
            Ops::Add => {
                println!("ADD");
                index += 1;
            }
            Ops::Sub => {
                println!("SUB");
                index += 1;
            }
            Ops::Mult => {
                println!("MULT");
                index += 1;
            }
            Ops::Div => {
                println!("DIV");
                index += 1;
            }
        }
    }
}
