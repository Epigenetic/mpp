/*
 * Copyright (c) 2021, Jonah Shafran.
 *
 * SPDX-License-Identifier: GPL-3.0-only
 */

mod test_runner;

use test_runner::run_error_test;

#[test]
fn text_bad_token() {
    run_error_test("tests/malformed_token.mtest")
}
