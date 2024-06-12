
# tests/testthat/test-func_myround.R
library(testthat)
library(formatinator)

test_that("func_myround works correctly", {
  expect_equal(func_myround(12345.6789, 2), "12,345.68")
  expect_equal(func_myround(98765.4321, 1), "98,765.4")
  expect_equal(func_myround(1000, 0), "1,000")
})
