
# tests/testthat/test-func_dx_clean_na.R
library(testthat)
library(formatinator)

test_that("func_dx_clean_na works correctly", {
  expect_equal(func_dx_clean_na(" q25.1  "), "Q251")
  expect_equal(func_dx_clean_na("q25.1a  "), "Q251A")
})
