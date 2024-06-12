
# tests/testthat/test-func_addci_to.R
library(testthat)
library(formatinator)

test_that("func_addci_to works correctly", {
  expect_equal(func_addci_to(10.123, 9.456, 10.789, 2), "10.12 (9.46 to 10.79)")
  expect_equal(func_addci_to(20.5, 19.0, 21.0, 1), "20.5 (19.0 to 21.0)")
  expect_equal(func_addci_to(5, 4.5, 5.5, 0), "5 (5 to 6)")
})
