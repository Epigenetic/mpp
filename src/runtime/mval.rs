/*
 * Copyright (c) 2021, Jonah Shafran.
 *
 * SPDX-License-Identifier: GPL-3.0-only
 */

use crate::runtime::BTree;
use rust_decimal::prelude::{FromStr, ToPrimitive, Zero};
use rust_decimal::{Decimal, RoundingStrategy};
use std::convert::TryInto;
use std::ops::{Add, Div, Mul, Rem, Sub};
use std::{fmt, str};

#[derive(Debug, PartialEq)]
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

    /// Creates an MVal from a string. Replaces occurrences of "" with ".
    pub fn from_string(value: String) -> MVal {
        MVal {
            value: Some(value.replace("\"\"", "\"")),
            array: None,
        }
    }

    /// Creates an MVal from a string. Does not perform the quotation mark sanitation
    /// from_string does.
    pub fn from_string_no_sanitize(value: String) -> MVal {
        MVal {
            value: Some(value),
            array: None,
        }
    }

    /// Convert a MVal to byte representation
    /// They are stored in the following format:
    /// Value is stored as the string's length followed by the string content in UFT-8
    /// Array format TBD
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

    /// Get an MVal from bytes, see to_bytes for the expected format.
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

    /// Get the numeric interpretation of an MVal
    /// If the value is numeric or the leading characters are numeric, that is the numeric
    /// interpretation (i.e. "123foo"=123). Otherwise, the interpretation is 0.
    pub fn numeric_interpretation(&self) -> Decimal {
        match &self.value {
            None => Decimal::zero(),
            Some(_) => {
                let leading_number = self.extract_leading_number();
                match Decimal::from_str(&*leading_number) {
                    Ok(decimal) => decimal,
                    Err(_) => Decimal::zero(),
                }
            }
        }
    }

    /// Get the boolean interpretation of an MVal
    /// If it has a value with a nonzero numeric interpretation, it is true, otherwise, it is false.
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

    /// Get the string interpretation of an MVal
    /// If it has a value, it returns that, otherwise, it returns the empty string.
    /// Array is always ignored.
    pub fn string_interpretation(&self) -> &str {
        match &self.value {
            None => "",
            Some(value) => &value,
        }
    }

    /// Modulo for MVals
    /// This is a proper modulo, not remainder like in rust
    pub fn modulo(&self, rhs: Self) -> Self {
        let lhs_decimal = self.numeric_interpretation();
        let rhs_decimal = rhs.numeric_interpretation();
        return MVal::from_string_no_sanitize(
            (((lhs_decimal % rhs_decimal) + rhs_decimal) % rhs_decimal).to_string(),
        );
    }

    /// Integer division for MVals. Essentially arithmetic division, but any fractional part of the
    /// result is discarded
    pub fn integer_divide(&self, rhs: Self) -> Self {
        let lhs_decimal = self.numeric_interpretation();
        let rhs_decimal = rhs.numeric_interpretation();
        return MVal::from_string_no_sanitize(
            (lhs_decimal / rhs_decimal)
                .round_dp_with_strategy(0, RoundingStrategy::ToZero)
                .to_string(),
        );
    }

    /// Raise an MVal to the rhs power. Uses floating point to do calculations for now, so not as
    /// precise as other operations.
    pub fn exponent(&self, rhs: Self) -> Self {
        let lhs_double = self.numeric_interpretation().to_f64().unwrap();
        let rhs_double = rhs.numeric_interpretation().to_f64().unwrap();
        return MVal::from_string_no_sanitize(f64::powf(lhs_double, rhs_double).to_string());
    }

    /// Extract the leading number from an MVal's value.
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
                found_dot = true;
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
        return MVal::from_string_no_sanitize(
            (self.numeric_interpretation() + rhs.numeric_interpretation()).to_string(),
        );
    }
}

impl Sub for MVal {
    type Output = MVal;

    fn sub(self, rhs: Self) -> Self::Output {
        return MVal::from_string_no_sanitize(
            (self.numeric_interpretation() - rhs.numeric_interpretation()).to_string(),
        );
    }
}

impl Mul for MVal {
    type Output = MVal;

    fn mul(self, rhs: Self) -> Self::Output {
        return MVal::from_string_no_sanitize(
            (self.numeric_interpretation() * rhs.numeric_interpretation()).to_string(),
        );
    }
}

impl Div for MVal {
    type Output = MVal;

    fn div(self, rhs: Self) -> Self::Output {
        return MVal::from_string_no_sanitize(
            (self.numeric_interpretation() / rhs.numeric_interpretation()).to_string(),
        );
    }
}

impl Rem for MVal {
    type Output = MVal;

    fn rem(self, rhs: Self) -> Self::Output {
        return MVal::from_string_no_sanitize(
            (self.numeric_interpretation() % rhs.numeric_interpretation()).to_string(),
        );
    }
}
