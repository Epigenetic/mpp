/*
 * Copyright (c) 2021, Jonah Shafran.
 *
 * SPDX-License-Identifier: GPL-3.0-only
 */

use crate::runtime::BTree;
use rust_decimal::Decimal;
use std::convert::TryInto;
use std::{fmt, str};

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
}

impl fmt::Display for MVal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "value: {:?}, array: unimplemented", self.value)
    }
}
