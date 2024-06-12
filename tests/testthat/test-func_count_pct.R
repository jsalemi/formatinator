
# tests/testthat/test-func_count_pct.R
library(testthat)
library(formatinator)

test_that("func_count_pct works correctly", {
  expect_equal(func_count_pct(1500, 75.1234, 2), "1,500 (75.12%)")
  expect_equal(func_count_pct(2500, 85.5678, 1), "2,500 (85.6%)")
  expect_equal(func_count_pct(1000, 50, 0), "1,000 (50%)")
})
