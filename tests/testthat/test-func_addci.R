
# tests/testthat/test-func_addci.R
library(testthat)
library(formatinator)

test_that("func_addci works correctly", {
  expect_equal(func_addci(10.123, 9.456, 10.789, 2), "10.12 (9.46, 10.79)")
  expect_equal(func_addci(20.5, 19.0, 21.0, 1), "20.5 (19.0, 21.0)")
  expect_equal(func_addci(5, 4.5, 5.5, 0), "5 (5, 6)")
})
