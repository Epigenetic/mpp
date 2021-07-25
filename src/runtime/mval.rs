/*
 * Copyright (c) 2021, Jonah Shafran.
 *
 * SPDX-License-Identifier: GPL-3.0-only
 */

use crate::runtime::BTree;
use rust_decimal::prelude::{FromStr, Zero};
use rust_decimal::{Decimal, RoundingStrategy};
use std::convert::TryInto;
use std::ops::{Add, Div, Mul, Rem, Sub};
use std::{fmt, str};

#[derive(Debug)]
pub struct MVal {
    value: Option<String>,
    array: Option<BTree>,
}

impl MVal {
    pub fn new() -> MVal {
        MVal {
            value: None,
            array: None,
        }
    }

    pub fn from_string(value: String) -> MVal {
        MVal {
            value: Some(value),
            array: None,
        }
    }

    pub fn to_bytes(&self) -> Vec<u8> {
        let value = &self.value;
        let mut byte_vec = Vec::new();

        match value {
            None => byte_vec.push(0 as u8),
            Some(val) => {
                let string_bytes = val.as_bytes();
                let string_len = string_bytes.len().to_le_bytes();
                for byte in string_len {
                    byte_vec.push(byte);
                }
                for byte in val.as_bytes() {
                    byte_vec.push(*byte);
                }
            }
        }
        byte_vec
    }

    /// Convert a MVal to byte representation
    /// They are stored in the following format:
    /// Value is stored as the string's length followed by the string content in UFT-8
    /// Array format TBD
    pub fn from_bytes(bytes: &[u8]) -> (MVal, usize) {
        let (size_bytes, rest) = bytes.split_at(std::mem::size_of::<usize>());
        let size = usize::from_le_bytes(size_bytes.try_into().unwrap());
        let str_bytes = &rest[..size];
        let string = str::from_utf8(str_bytes).unwrap().to_string();
        (
            MVal {
                value: Some(string),
                array: None,
            },
            size,
        )
    }

    pub fn numeric_interpretation(&self) -> Decimal {
        match &self.value {
            None => Decimal::zero(),
            Some(value) => {
                let leading_number = self.extract_leading_number();
                match Decimal::from_str(&*leading_number) {
                    Ok(decimal) => decimal,
                    Err(_) => Decimal::zero(),
                }
            }
        }
    }

    pub fn boolean_interpretation(&self) -> bool {
        match &self.value {
            None => false,
            Some(_) => {
                if self.numeric_interpretation() != Decimal::zero() {
                    true
                } else {
                    false
                }
            }
        }
    }

    pub fn string_interpretation(&self) -> &str {
        match &self.value {
            None => "",
            Some(value) => &value,
        }
    }

    pub fn modulo(&self, rhs: Self) -> Self {
        let lhs_decimal = self.numeric_interpretation();
        let rhs_decimal = rhs.numeric_interpretation();
        return MVal::from_string(
            (((lhs_decimal % rhs_decimal) + rhs_decimal) % rhs_decimal).to_string(),
        );
    }

    /// Integer division for MVals. Essentially arithmetic division, but any fractional part of the
    /// result is discarded
    pub fn integer_divide(&self, rhs: Self) -> Self {
        let lhs_decimal = self.numeric_interpretation();
        let rhs_decimal = rhs.numeric_interpretation();
        return MVal::from_string(
            (lhs_decimal / rhs_decimal)
                .round_dp_with_strategy(0, RoundingStrategy::ToZero)
                .to_string(),
        );
    }

    fn extract_leading_number(&self) -> &str {
        if self.value == None {
            return "";
        }

        let mut index: usize = 0;
        let str_array: Vec<char> = self.value.as_ref().unwrap().chars().collect();
        let mut found_dot = false;

        while index < self.value.as_ref().unwrap().len() {
            if index == 0 && str_array[index] == '-' {
                index += 1;
                continue;
            }

            if str_array[index] == '.' {
                if found_dot {
                    break;
                }
                index += 1;
                continue;
            }

            if str_array[index].is_digit(10) {
                index += 1;
                continue;
            }
            break;
        }

        // If we just found a dot without any accompanying digits
        if index == 0 && str_array[0] == '.' {
            return "";
        }

        return &self.value.as_ref().unwrap()[0..index];
    }
}

impl fmt::Display for MVal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "value: {:?}, array: unimplemented", self.value)
    }
}

impl Add for MVal {
    type Output = MVal;

    fn add(self, rhs: Self) -> Self::Output {
        return MVal::from_string(
            (self.numeric_interpretation() + rhs.numeric_interpretation()).to_string(),
        );
    }
}

impl Sub for MVal {
    type Output = MVal;

    fn sub(self, rhs: Self) -> Self::Output {
        return MVal::from_string(
            (self.numeric_interpretation() - rhs.numeric_interpretation()).to_string(),
        );
    }
}

impl Mul for MVal {
    type Output = MVal;

    fn mul(self, rhs: Self) -> Self::Output {
        return MVal::from_string(
            (self.numeric_interpretation() * rhs.numeric_interpretation()).to_string(),
        );
    }
}

impl Div for MVal {
    type Output = MVal;

    fn div(self, rhs: Self) -> Self::Output {
        return MVal::from_string(
            (self.numeric_interpretation() / rhs.numeric_interpretation()).to_string(),
        );
    }
}

impl Rem for MVal {
    type Output = MVal;

    fn rem(self, rhs: Self) -> Self::Output {
        return MVal::from_string(
            (self.numeric_interpretation() % rhs.numeric_interpretation()).to_string(),
        );
    }
}
