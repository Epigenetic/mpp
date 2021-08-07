/*
 * Copyright (c) 2021, Jonah Shafran.
 *
 * SPDX-License-Identifier: GPL-3.0-only
 */

mod test_runner;

use test_runner::run_error_test;
use test_runner::run_tests;

#[test]
fn test_arithmetic() {
    run_tests("tests/expression_tests/arithmetic.mtest")
}

#[test]
fn test_numeric_interpretation() {
    run_tests("tests/expression_tests/numeric_interpretation.mtest")
}

#[test]
fn test_order_of_operations() {
    run_tests("tests/expression_tests/order_of_operations.mtest")
}

#[test]
fn test_relational_expression() {
    run_tests("tests/expression_tests/relational_expression.mtest")
}

#[test]
fn test_boolean_expression() {
    run_tests("tests/expression_tests/boolean_expression.mtest")
}

#[test]
fn test_malformed_expression() {
    run_error_test("tests/expression_tests/malformed_expression.mtest")
}
