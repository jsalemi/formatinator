
# tests/testthat/test-func_dx_scan_yn.R
library(testthat)
library(formatinator)

test_that("func_dx_scan_yn works correctly", {
  df <- tibble::tibble(
    dx1 = c("X34.5", "X35.6", "X36.7"),
    dx2 = c("G33.1", "G34.3", "G35.4"),
    dx3 = c("F25.4", "F26.7", "F27.8"),
    dx4 = c(NA, "H44.8", "H45.9"),
    dx5 = c(NA, NA, "I56.0")
  )

  expect_equal(func_dx_scan_yn(df, "^G"), c(1, 1, 1))
  expect_equal(func_dx_scan_yn(df, "^G33"), c(1, 0, 0))
  expect_equal(func_dx_scan_yn(df, "[02468]$"), c(1, 1, 1))
})
