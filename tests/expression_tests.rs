/*
 * Copyright (c) 2021, Jonah Shafran.
 *
 * SPDX-License-Identifier: GPL-3.0-only
 */

mod test_runner;

use test_runner::run_tests;

#[test]
fn test_expressions() {
    run_tests("tests/expression.mtest")
}
