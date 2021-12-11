/*
 * Copyright (c) 2021, Jonah Shafran.
 *
 * SPDX-License-Identifier: GPL-3.0-only
 */

use crate::runtime::BTree;
use std::convert::TryInto;
use std::fmt::{Debug, Formatter};
use std::ops::{Add, Div, Mul, Neg, Sub};
use std::{fmt, str};

pub struct MVal {
    value: Option<MValValue>,
    pub value_type: MValType,
}

impl Debug for MVal {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        let mut debug_struct = f.debug_struct("MVal");
        debug_struct.field("value_type", &self.value_type);

        match self.value_type {
            MValType::String => debug_struct.field("value", &self.get_string_val()).finish(),
            MValType::Int => debug_struct.field("value", &self.get_int_val()).finish(),
            MValType::Double => debug_struct.field("value", &self.get_double_val()).finish(),
            MValType::Boolean => debug_struct
                .field("value", &self.get_boolean_val())
                .finish(),
            MValType::Null => debug_struct.field("value", &"Null").finish(),
        }
    }
}

impl PartialEq for MVal {
    fn eq(&self, other: &Self) -> bool {
        self.equals_internal(other)
    }
}

impl Clone for MVal {
    fn clone(&self) -> Self {
        match self.value_type {
            MValType::String => MVal {
                value: Some(MValValue {
                    string: self.get_string_val().clone(),
                }),
                value_type: MValType::String,
            },
            MValType::Int => MVal {
                value: Some(MValValue {
                    int: self.get_int_val(),
                }),
                value_type: MValType::Int,
            },
            MValType::Double => MVal {
                value: Some(MValValue {
                    double: self.get_double_val(),
                }),
                value_type: MValType::Double,
            },
            MValType::Boolean => MVal {
                value: Some(MValValue {
                    boolean: self.get_boolean_val(),
                }),
                value_type: MValType::Boolean,
            },
            MValType::Null => MVal {
                value: None,
                value_type: MValType::Null,
            },
        }
    }
}

#[repr(u8)]
#[derive(PartialEq, Debug, Copy, Clone)]
pub enum MValType {
    Null = 0,
    String = 1,
    Double = 2,
    Int = 3,
    Boolean = 4,
}

impl MValType {
    pub fn from_u8(value: u8) -> MValType {
        return match value {
            0 => MValType::Null,
            1 => MValType::String,
            2 => MValType::Double,
            3 => MValType::Int,
            4 => MValType::Boolean,
            val => panic!("Unrecognized value type: {}.", val),
        };
    }

    pub fn is_numeric_type(&self) -> bool {
        return match self {
            MValType::Double | MValType::Int => true,
            _ => false,
        };
    }
}

union MValValue {
    string: std::mem::ManuallyDrop<String>,
    double: f64,
    int: i32,
    boolean: bool,
}

impl MVal {
    pub fn new() -> MVal {
        MVal {
            value: None,
            value_type: MValType::Null,
        }
    }

    /// Creates a string type MVal from a string. Replaces occurrences of "" with ".
    pub fn from_string(value: String) -> MVal {
        MVal {
            value: Some(MValValue {
                string: std::mem::ManuallyDrop::new(value.replace("\"\"", "\"")),
            }),
            value_type: MValType::String,
        }
    }

    /// Creates a string type MVal from a string. Does not perform the quotation mark sanitation
    /// from_string does.
    pub fn from_string_no_sanitize(value: String) -> MVal {
        MVal {
            value: Some(MValValue {
                string: std::mem::ManuallyDrop::new(value),
            }),
            value_type: MValType::String,
        }
    }

    /// Convert an MVal to byte representation
    /// They are stored in the following format:
    /// Value is stored as the string's length followed by the string content in UTF-8
    pub fn to_bytes(&self) -> Vec<u8> {
        let value = &self.value;
        let mut byte_vec = vec![self.value_type as u8];

        match value {
            None => byte_vec.push(0 as u8),
            Some(val) => match &self.value_type {
                MValType::Null => {}
                MValType::String => unsafe {
                    let string_val = &val.string;
                    let string_bytes = string_val.as_bytes();
                    let string_len = string_bytes.len().to_le_bytes();
                    for byte in string_len {
                        byte_vec.push(byte);
                    }
                    for byte in string_val.as_bytes() {
                        byte_vec.push(*byte)
                    }
                },
                MValType::Double => unsafe {
                    let double_val = &val.double;
                    let double_bytes = double_val.to_le_bytes();
                    for byte in double_bytes {
                        byte_vec.push(byte)
                    }
                },
                MValType::Int => unsafe {
                    let int_val = &val.int;
                    let int_bytes = int_val.to_le_bytes();
                    for byte in int_bytes {
                        byte_vec.push(byte)
                    }
                },
                MValType::Boolean => unsafe {
                    let bool_val = &val.boolean;
                    let bool_byte = if *bool_val { 1u8 } else { 0u8 };
                    byte_vec.push(bool_byte);
                },
            },
        }
        byte_vec
    }

    /// Get an MVal from bytes, see to_bytes for the expected format.
    pub fn from_bytes(bytes: &[u8]) -> (MVal, usize) {
        return match MValType::from_u8(bytes[0]) {
            MValType::Null => (
                MVal {
                    value: None,
                    value_type: MValType::Null,
                },
                1,
            ),
            MValType::String => {
                let (size_bytes, rest) = bytes[1..].split_at(std::mem::size_of::<usize>());
                let size = usize::from_le_bytes(size_bytes.try_into().unwrap());
                let str_bytes = &rest[..size];
                let string = str::from_utf8(str_bytes).unwrap().to_string();
                (
                    MVal {
                        value: Some(MValValue {
                            string: std::mem::ManuallyDrop::new(string),
                        }),
                        value_type: MValType::String,
                    },
                    size + 1,
                )
            }
            MValType::Double => {
                let (double_bytes, _) = bytes[1..].split_at(std::mem::size_of::<f64>());
                let double = f64::from_le_bytes(double_bytes.try_into().unwrap());
                let size = std::mem::size_of::<f64>();
                (
                    MVal {
                        value: Some(MValValue { double }),
                        value_type: MValType::Double,
                    },
                    size + 1,
                )
            }
            MValType::Int => {
                let (int_bytes, _) = bytes[1..].split_at(std::mem::size_of::<i32>());
                let int = i32::from_le_bytes(int_bytes.try_into().unwrap());
                let size = std::mem::size_of::<i32>();
                (
                    MVal {
                        value: Some(MValValue { int }),
                        value_type: MValType::Int,
                    },
                    size + 1,
                )
            }
            MValType::Boolean => {
                let boolean = if bytes[1] == 1 {
                    true
                } else if bytes[1] == 0 {
                    false
                } else {
                    panic!("Not a legal value for a bool")
                };

                (
                    MVal {
                        value: Some(MValValue { boolean }),
                        value_type: MValType::Boolean,
                    },
                    2,
                )
            }
        };
    }

    pub fn get_string_val(&self) -> std::mem::ManuallyDrop<String> {
        if self.value_type != MValType::String {
            panic!("MVal is not a string.")
        }

        unsafe {
            return self
                .value
                .as_ref()
                .expect("Strings should not be null")
                .string
                .clone();
        }
    }

    pub fn get_double_val(&self) -> f64 {
        if self.value_type != MValType::Double {
            panic!("MVal is not a double.")
        }

        unsafe {
            return self
                .value
                .as_ref()
                .expect("Doubles should not be null")
                .double;
        }
    }

    pub fn get_int_val(&self) -> i32 {
        if self.value_type != MValType::Int {
            panic!("MVal is not an int.");
        }
        unsafe { return self.value.as_ref().expect("Ints should not be null").int }
    }

    pub fn get_boolean_val(&self) -> bool {
        if self.value_type != MValType::Boolean {
            panic!("MVal is not a boolean.")
        }

        unsafe {
            return self
                .value
                .as_ref()
                .expect("Booleans should not be null")
                .boolean;
        }
    }

    #[inline]
    fn verify_operands_numeric(&self, rhs: &Self) {
        if !self.value_type.is_numeric_type() {
            panic!("LHS is not numeric");
        }
        if !rhs.value_type.is_numeric_type() {
            panic!("RHS is not numeric");
        }
    }

    /// Modulo for MVals, can only be performed on numeric types (see `MValType::is_numeric_type`)
    /// This is a proper modulo, not remainder like in rust
    pub fn modulo(&self, rhs: &Self) -> Self {
        self.verify_operands_numeric(rhs);

        return if self.value_type == MValType::Int && rhs.value_type == MValType::Int {
            MVal {
                value: Some(MValValue {
                    int: self.get_int_val().rem_euclid(rhs.get_int_val()),
                }),
                value_type: MValType::Int,
            }
        } else {
            let lhs_double = if self.value_type == MValType::Int {
                self.get_int_val() as f64
            } else {
                self.get_double_val()
            };

            let rhs_double = if rhs.value_type == MValType::Int {
                rhs.get_int_val() as f64
            } else {
                rhs.get_double_val()
            };

            let result = lhs_double.rem_euclid(rhs_double);
            if result.fract() == 0.0 {
                MVal {
                    value: Some(MValValue { int: result as i32 }),
                    value_type: MValType::Int,
                }
            } else {
                MVal {
                    value: Some(MValValue { double: result }),
                    value_type: MValType::Double,
                }
            }
        };
    }

    /// Integer division for MValscan only be performed on numeric types (see `MValType::is_numeric_type`).
    /// Essentially arithmetic division, but any fractional part of the result is discarded
    pub fn integer_divide(&self, rhs: &Self) -> Self {
        self.verify_operands_numeric(rhs);

        let lhs_int = if self.value_type == MValType::Int {
            self.get_int_val()
        } else {
            self.get_double_val() as i32
        };

        let rhs_int = if rhs.value_type == MValType::Int {
            rhs.get_int_val()
        } else {
            rhs.get_double_val() as i32
        };

        MVal {
            value: Some(MValValue {
                int: lhs_int / rhs_int,
            }),
            value_type: MValType::Int,
        }
    }

    /// Raise an MVal to the rhs power, can only be performed on numeric types (see `MValType::is_numeric_type`).
    /// Precision varies depending on operands' types
    pub fn exponent(&self, rhs: &Self) -> Self {
        self.verify_operands_numeric(rhs);

        let lhs_double = if self.value_type == MValType::Int {
            self.get_int_val() as f64
        } else {
            self.get_double_val()
        };

        return if rhs.value_type == MValType::Int {
            let rhs_int = rhs.get_int_val();
            let result = lhs_double.powi(rhs_int);

            if result.fract() == 0.0 {
                MVal {
                    value: Some(MValValue { int: result as i32 }),
                    value_type: MValType::Int,
                }
            } else {
                MVal {
                    value: Some(MValValue { double: result }),
                    value_type: MValType::Double,
                }
            }
        } else {
            let rhs_double = rhs.get_double_val();
            let result = lhs_double.powf(rhs_double);

            if result.fract() == 0.0 {
                MVal {
                    value: Some(MValValue { int: result as i32 }),
                    value_type: MValType::Int,
                }
            } else {
                MVal {
                    value: Some(MValValue { double: result }),
                    value_type: MValType::Double,
                }
            }
        };
    }

    /// Less than for MVals, can only be performed on numeric types (see `MValType::is_numeric_type`).
    /// Compares the numeric values
    pub fn less_than(&self, rhs: &Self) -> Self {
        self.verify_operands_numeric(rhs);

        let lhs_double = if self.value_type == MValType::Int {
            self.get_int_val() as f64
        } else {
            self.get_double_val()
        };

        let rhs_double = if rhs.value_type == MValType::Int {
            rhs.get_int_val() as f64
        } else {
            rhs.get_double_val()
        };

        return MVal {
            value: Some(MValValue {
                boolean: lhs_double < rhs_double,
            }),
            value_type: MValType::Boolean,
        };
    }

    /// Greater than for MVals,can only be performed on numeric types (see `MValType::is_numeric_type`).
    /// Compares the numeric values
    pub fn greater_than(&self, rhs: &Self) -> Self {
        self.verify_operands_numeric(rhs);

        let lhs_double = if self.value_type == MValType::Int {
            self.get_int_val() as f64
        } else {
            self.get_double_val()
        };

        let rhs_double = if rhs.value_type == MValType::Int {
            rhs.get_int_val() as f64
        } else {
            rhs.get_double_val()
        };

        return MVal {
            value: Some(MValValue {
                boolean: lhs_double > rhs_double,
            }),
            value_type: MValType::Boolean,
        };
    }

    /// Less than or equal to for MVals, can only be performed on numeric types (see `MValType::is_numeric_type`).
    /// Compares the numeric values
    pub fn less_than_or_equal_to(&self, rhs: &Self) -> Self {
        self.verify_operands_numeric(rhs);

        let lhs_double = if self.value_type == MValType::Int {
            self.get_int_val() as f64
        } else {
            self.get_double_val()
        };

        let rhs_double = if rhs.value_type == MValType::Int {
            rhs.get_int_val() as f64
        } else {
            rhs.get_double_val()
        };

        return MVal {
            value: Some(MValValue {
                boolean: lhs_double <= rhs_double,
            }),
            value_type: MValType::Boolean,
        };
    }

    /// Greater than or equal to for MVals,can only be performed on numeric types (see `MValType::is_numeric_type`).
    /// Compares the numeric values
    pub fn greater_than_or_equal_to(&self, rhs: &Self) -> Self {
        self.verify_operands_numeric(rhs);

        let lhs_double = if self.value_type == MValType::Int {
            self.get_int_val() as f64
        } else {
            self.get_double_val()
        };

        let rhs_double = if rhs.value_type == MValType::Int {
            rhs.get_int_val() as f64
        } else {
            rhs.get_double_val()
        };

        return MVal {
            value: Some(MValValue {
                boolean: lhs_double >= rhs_double,
            }),
            value_type: MValType::Boolean,
        };
    }

    /// Not for MVals. If the numeric interpretation is greater than 0, it returns 0, otherwise 1
    pub fn not(&self) -> Self {
        if self.value_type != MValType::Boolean {
            panic!("Operand is not boolean")
        }

        return MVal {
            value: Some(MValValue {
                boolean: !self.get_boolean_val(),
            }),
            value_type: MValType::Boolean,
        };
    }

    /// Equals for MVals. Must be of the same type to be equal or a numeric type (ints can be compared
    /// to floats).
    pub fn equals(&self, rhs: &Self) -> Self {
        return if self.equals_internal(rhs) {
            MVal {
                value: Some(MValValue { boolean: true }),
                value_type: MValType::Boolean,
            }
        } else {
            return MVal {
                value: Some(MValValue { boolean: false }),
                value_type: MValType::Boolean,
            };
        };
    }

    /// Not equals for MVals. Must be of the same type to be equal or a numeric type (ints can be compared
    //   to floats).
    pub fn not_equals(&self, rhs: &Self) -> Self {
        return if self.equals_internal(rhs) {
            MVal {
                value: Some(MValValue { boolean: false }),
                value_type: MValType::Boolean,
            }
        } else {
            return MVal {
                value: Some(MValValue { boolean: true }),
                value_type: MValType::Boolean,
            };
        };
    }

    fn equals_internal(&self, rhs: &Self) -> bool {
        return if self.value_type.is_numeric_type() && rhs.value_type.is_numeric_type() {
            let lhs_double = if self.value_type == MValType::Int {
                self.get_int_val() as f64
            } else {
                self.get_double_val()
            };

            let rhs_double = if rhs.value_type == MValType::Int {
                rhs.get_int_val() as f64
            } else {
                rhs.get_double_val()
            };

            lhs_double == rhs_double
        } else if self.value_type == MValType::String && rhs.value_type == MValType::String {
            self.get_string_val() == rhs.get_string_val()
        } else if self.value_type == MValType::Boolean && rhs.value_type == MValType::Boolean {
            self.get_boolean_val() == rhs.get_boolean_val()
        } else if self.value_type == MValType::Null && rhs.value_type == MValType::Null {
            true
        } else {
            false
        };
    }
}

impl fmt::Display for MVal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self.value_type {
            MValType::String => write!(f, "{}", *self.get_string_val()),
            MValType::Int => write!(f, "{}", self.get_int_val()),
            MValType::Double => write!(f, "{}", self.get_double_val()),
            MValType::Boolean => write!(f, "{}", self.get_boolean_val()),
            MValType::Null => write!(f, ""),
        }
    }
}

impl Neg for &MVal {
    type Output = MVal;

    fn neg(self) -> Self::Output {
        if !self.value_type.is_numeric_type() {
            panic!("Operand not numeric")
        }

        return if self.value_type == MValType::Int {
            MVal {
                value: Some(MValValue {
                    int: -self.get_int_val(),
                }),
                value_type: MValType::Int,
            }
        } else if self.value_type == MValType::Double {
            MVal {
                value: Some(MValValue {
                    double: -self.get_double_val(),
                }),
                value_type: MValType::Double,
            }
        } else {
            unreachable!(
                "Missing implementation for numeric type {:?}",
                self.value_type
            )
        };
    }
}

impl Add for &MVal {
    type Output = MVal;

    fn add(self, rhs: Self) -> Self::Output {
        self.verify_operands_numeric(rhs);

        return if self.value_type == MValType::Int && rhs.value_type == MValType::Double {
            MVal {
                value: Some(MValValue {
                    int: self.get_int_val() + rhs.get_int_val(),
                }),
                value_type: MValType::Int,
            }
        } else {
            let lhs_double = if self.value_type == MValType::Int {
                self.get_int_val() as f64
            } else {
                self.get_double_val()
            };

            let rhs_double = if rhs.value_type == MValType::Int {
                rhs.get_int_val() as f64
            } else {
                rhs.get_double_val()
            };

            let result = lhs_double + rhs_double;
            if result.fract() == 0.0 {
                MVal {
                    value: Some(MValValue { int: result as i32 }),
                    value_type: MValType::Int,
                }
            } else {
                MVal {
                    value: Some(MValValue { double: result }),
                    value_type: MValType::Double,
                }
            }
        };
    }
}

impl Sub for &MVal {
    type Output = MVal;

    fn sub(self, rhs: Self) -> Self::Output {
        self.verify_operands_numeric(rhs);

        return if self.value_type == MValType::Int && rhs.value_type == MValType::Double {
            MVal {
                value: Some(MValValue {
                    int: self.get_int_val() - rhs.get_int_val(),
                }),
                value_type: MValType::Int,
            }
        } else {
            let lhs_double = if self.value_type == MValType::Int {
                self.get_int_val() as f64
            } else {
                self.get_double_val()
            };

            let rhs_double = if rhs.value_type == MValType::Int {
                rhs.get_int_val() as f64
            } else {
                rhs.get_double_val()
            };

            let result = lhs_double - rhs_double;
            if result.fract() == 0.0 {
                MVal {
                    value: Some(MValValue { int: result as i32 }),
                    value_type: MValType::Int,
                }
            } else {
                MVal {
                    value: Some(MValValue { double: result }),
                    value_type: MValType::Double,
                }
            }
        };
    }
}

impl Mul for &MVal {
    type Output = MVal;

    fn mul(self, rhs: Self) -> Self::Output {
        self.verify_operands_numeric(rhs);

        return if self.value_type == MValType::Int && rhs.value_type == MValType::Double {
            MVal {
                value: Some(MValValue {
                    int: self.get_int_val() * rhs.get_int_val(),
                }),
                value_type: MValType::Int,
            }
        } else {
            let lhs_double = if self.value_type == MValType::Int {
                self.get_int_val() as f64
            } else {
                self.get_double_val()
            };

            let rhs_double = if rhs.value_type == MValType::Int {
                rhs.get_int_val() as f64
            } else {
                rhs.get_double_val()
            };

            let result = lhs_double * rhs_double;
            if result.fract() == 0.0 {
                MVal {
                    value: Some(MValValue { int: result as i32 }),
                    value_type: MValType::Int,
                }
            } else {
                MVal {
                    value: Some(MValValue { double: result }),
                    value_type: MValType::Double,
                }
            }
        };
    }
}

impl Div for &MVal {
    type Output = MVal;

    // TODO: Handle division by zero in the runtime
    fn div(self, rhs: Self) -> Self::Output {
        self.verify_operands_numeric(rhs);

        return if self.value_type == MValType::Int && rhs.value_type == MValType::Double {
            MVal {
                value: Some(MValValue {
                    int: self.get_int_val() / rhs.get_int_val(),
                }),
                value_type: MValType::Int,
            }
        } else {
            let lhs_double = if self.value_type == MValType::Int {
                self.get_int_val() as f64
            } else {
                self.get_double_val()
            };

            let rhs_double = if rhs.value_type == MValType::Int {
                rhs.get_int_val() as f64
            } else {
                rhs.get_double_val()
            };

            let result = lhs_double / rhs_double;
            if result.fract() == 0.0 {
                MVal {
                    value: Some(MValValue { int: result as i32 }),
                    value_type: MValType::Int,
                }
            } else {
                MVal {
                    value: Some(MValValue { double: result }),
                    value_type: MValType::Double,
                }
            }
        };
    }
}

impl Drop for MVal {
    fn drop(&mut self) {
        if self.value_type == MValType::String {
            drop(self.get_string_val())
        }
    }
}

#[cfg(test)]
mod test {
    use crate::runtime::mval::{MValType, MValValue};
    use crate::runtime::MVal;

    //region Valid Cases
    #[test]
    fn test_add() {
        let lhs = MVal {
            value: Some(MValValue { int: 2 }),
            value_type: MValType::Int,
        };
        let rhs = MVal {
            value: Some(MValValue { int: 3 }),
            value_type: MValType::Int,
        };

        assert_eq!(
            &lhs + &rhs,
            MVal {
                value: Some(MValValue { int: 5 }),
                value_type: MValType::Int
            }
        )
    }

    #[test]
    fn test_sub() {
        let lhs = MVal {
            value: Some(MValValue { int: 2 }),
            value_type: MValType::Int,
        };
        let rhs = MVal {
            value: Some(MValValue { int: 3 }),
            value_type: MValType::Int,
        };

        assert_eq!(
            &lhs - &rhs,
            MVal {
                value: Some(MValValue { int: -1 }),
                value_type: MValType::Int
            }
        )
    }

    #[test]
    fn test_mult() {
        let lhs = MVal {
            value: Some(MValValue { int: 2 }),
            value_type: MValType::Int,
        };
        let rhs = MVal {
            value: Some(MValValue { int: 3 }),
            value_type: MValType::Int,
        };

        assert_eq!(
            &lhs * &rhs,
            MVal {
                value: Some(MValValue { int: 6 }),
                value_type: MValType::Int
            }
        )
    }

    #[test]
    fn test_div() {
        let lhs = MVal {
            value: Some(MValValue { int: 2 }),
            value_type: MValType::Int,
        };
        let rhs = MVal {
            value: Some(MValValue { int: 3 }),
            value_type: MValType::Int,
        };

        assert_eq!(
            (&lhs / &rhs),
            MVal {
                value: Some(MValValue { double: 2.0 / 3.0 }),
                value_type: MValType::Double
            }
        )
    }

    #[test]
    fn test_int_div() {
        let lhs = MVal {
            value: Some(MValValue { int: 2 }),
            value_type: MValType::Int,
        };
        let rhs = MVal {
            value: Some(MValValue { int: 3 }),
            value_type: MValType::Int,
        };

        assert_eq!(
            lhs.integer_divide(&rhs),
            MVal {
                value: Some(MValValue { int: 0 }),
                value_type: MValType::Int
            }
        )
    }

    #[test]
    fn test_mod() {
        let lhs = MVal {
            value: Some(MValValue { int: 5 }),
            value_type: MValType::Int,
        };
        let rhs = MVal {
            value: Some(MValValue { int: 3 }),
            value_type: MValType::Int,
        };

        assert_eq!(
            lhs.modulo(&rhs),
            MVal {
                value: Some(MValValue { int: 2 }),
                value_type: MValType::Int
            }
        )
    }

    #[test]
    fn test_power() {
        let lhs = MVal {
            value: Some(MValValue { int: 2 }),
            value_type: MValType::Int,
        };
        let rhs = MVal {
            value: Some(MValValue { int: 3 }),
            value_type: MValType::Int,
        };

        assert_eq!(
            lhs.exponent(&rhs),
            MVal {
                value: Some(MValValue { int: 8 }),
                value_type: MValType::Int
            }
        )
    }

    #[test]
    fn test_not() {
        let operand1 = MVal {
            value: Some(MValValue { boolean: true }),
            value_type: MValType::Boolean,
        };
        let operand2 = MVal {
            value: Some(MValValue { boolean: false }),
            value_type: MValType::Boolean,
        };

        assert_eq!(
            operand1.not(),
            MVal {
                value: Some(MValValue { boolean: false }),
                value_type: MValType::Boolean,
            }
        );
        assert_eq!(
            operand2.not(),
            MVal {
                value: Some(MValValue { boolean: true }),
                value_type: MValType::Boolean,
            }
        );
    }
    //endregion

    //region Description
    #[test]
    #[should_panic]
    fn test_not_wrong_type() {
        let operand = MVal::from_string_no_sanitize("-1".to_string());
        operand.not();
    }
    //endregion
}
