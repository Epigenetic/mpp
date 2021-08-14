/*
 * Copyright (c) 2021, Jonah Shafran.
 *
 * SPDX-License-Identifier: GPL-3.0-only
 */

use crate::runtime::MVal;

#[repr(u8)]
#[derive(Debug)]
pub enum Ops {
    Push = 1,
    Add = 2,
    Sub = 3,
    Mult = 4,
    Div = 5,
    Mod = 6,
    IntDiv = 7,
    ToNum = 8,
    ToNegNum = 9,
    Exp = 10,
    Write = 11,
    WriteLine = 12,
    WriteClearScreen = 13,
    WriteToCol = 14,
    LessThan = 15,
    GreaterThan = 16,
    LessThanOrEqualTo = 17,
    GreaterThanOrEqualTo = 18,
    Not = 19,
    New = 20,
    Set = 21,
    Get = 22,
}

impl Ops {
    pub fn from_u8(value: u8) -> Ops {
        match value {
            1 => Ops::Push,
            2 => Ops::Add,
            3 => Ops::Sub,
            4 => Ops::Mult,
            5 => Ops::Div,
            6 => Ops::Mod,
            7 => Ops::IntDiv,
            8 => Ops::ToNum,
            9 => Ops::ToNegNum,
            10 => Ops::Exp,
            11 => Ops::Write,
            12 => Ops::WriteLine,
            13 => Ops::WriteClearScreen,
            14 => Ops::WriteToCol,
            15 => Ops::LessThan,
            16 => Ops::GreaterThan,
            17 => Ops::LessThanOrEqualTo,
            18 => Ops::GreaterThanOrEqualTo,
            19 => Ops::Not,
            20 => Ops::New,
            21 => Ops::Set,
            22 => Ops::Get,
            op => panic!("Unrecognized op code {}", op),
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
            Ops::Mod => {
                println!("MOD");
                index += 1
            }
            Ops::IntDiv => {
                println!("INT_DIV");
                index += 1
            }
            Ops::ToNum => {
                println!("TO_NUM");
                index += 1
            }
            Ops::ToNegNum => {
                println!("TO_NEG_NUM");
                index += 1
            }
            Ops::Exp => {
                println!("EXP");
                index += 1
            }
            Ops::Write => {
                println!("WRITE");
                index += 1
            }
            Ops::WriteLine => {
                println!("WRITE_LINE");
                index += 1;
            }
            Ops::WriteClearScreen => {
                println!("WRITE_CLEAR_SCREEN");
                index += 1;
            }
            Ops::WriteToCol => {
                println!("WRITE_TO_COL");
                index += 1;
            }
            Ops::LessThan => {
                println!("GREATER_THAN");
                index += 1;
            }
            Ops::GreaterThan => {
                println!("LESS_THAN");
                index += 1;
            }
            Ops::LessThanOrEqualTo => {
                println!("LESS_THAN_OR_EQUAL_TO");
                index += 1;
            }
            Ops::GreaterThanOrEqualTo => {
                println!("GREATER_THAN_OR_EQUAL_TO");
                index += 1;
            }
            Ops::Not => {
                println!("NOT");
                index += 1;
            }
            Ops::New => {
                println!("NEW");
                index += 1
            }
            Ops::Set => {
                println!("SET {}", program[index + 1]);
                index += 1 + std::mem::size_of::<usize>()
            }
            Ops::Get => {
                println!("GET {}", program[index + 1]);
                index += 1 + std::mem::size_of::<usize>()
            }
        }
    }
}
