/*
 * Copyright (c) 2021, Jonah Shafran.
 *
 * SPDX-License-Identifier: GPL-3.0-only
 */

use rust_decimal::Decimal;

pub const PUSH_DECIMAL: u8 = 1;
pub const ADD_DECIMAL: u8 = 2;
pub const SUB_DECIMAL: u8 = 3;
pub const MULT_DECIMAL: u8 = 4;
pub const DIV_DECIMAL: u8 = 5;

pub fn print_program(program: &Vec<u8>) {
    let mut index = 0;

    while index < program.len() {
        match program[index] {
            PUSH_DECIMAL => {
                let mut arr: [u8; 16] = [0; 16];
                for byte in 0..=15 {
                    arr[byte] = program[index + byte + 1];
                }
                println!("PUSH_DECIMAL: {:?}", Decimal::deserialize(arr));
                index += 17;
            }
            ADD_DECIMAL => {
                println!("ADD_DECIMAL");
                index += 1;
            }
            SUB_DECIMAL => {
                println!("SUB_DECIMAL");
                index += 1;
            }
            MULT_DECIMAL => {
                println!("MULT_DECIMAL");
                index += 1;
            }
            DIV_DECIMAL => {
                println!("DIV_DECIMAL");
                index += 1;
            }
            _ => panic!("Unknown operand"),
        }
    }
}
