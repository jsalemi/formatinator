
#' Round Numbers with Commas
#'
#' This function rounds a number to a specified number of decimal places and
#' formats it with commas as thousand separators.
#'
#' @param num The number to be rounded.
#' @param dec The number of decimal places.
#' @return A character string of the formatted number.
#' @export
#' @examples
#' # Basic usage
#' func_myround(12345.6789, 2)
#'
#' # Using with dplyr to create a new formatted variable
#' library(dplyr)
#' df <- tibble(value = c(12345.6789, 98765.4321))
#' df <- df |> mutate(formatted_value = func_myround(value, 2))
func_myround <- function(num, dec) {
  stringr::str_trim(
    format(round(num, dec), nsmall=dec, big.mark = ",")
  )
}

#' Add Confidence Intervals
#'
#' This function formats an estimate with its confidence interval
#' (lower and upper bounds) and rounds them to a specified number of decimal places.
#'
#' @param est The estimate.
#' @param low The lower bound of the interval.
#' @param hi The upper bound of the interval.
#' @param dec The number of decimal places.
#' @return A character string with the estimate and confidence intervals.
#' @export
#' @examples
#' # Basic usage
#' func_addci(10.123, 9.456, 10.789, 2)
#'
#' # Using with dplyr to create a new formatted variable
#' library(dplyr)
#' df <- tibble(est = c(10.123, 20.456), low = c(9.456, 19.789), hi = c(10.789, 21.123))
#' df <- df |> mutate(ci = func_addci(est, low, hi, 2))
func_addci <- function(est, low, hi, dec) {
  stringr::str_trim(
    paste0(stringr::str_trim(format(round(est, dec), nsmall=dec)), " (",
           stringr::str_trim(format(round(low, dec), nsmall=dec)), ", ",
           stringr::str_trim(format(round(hi,  dec), nsmall=dec)), ")")
  )
}

#' Add Confidence Intervals with Dash
#'
#' This function formats an estimate with its confidence interval
#' (lower and upper bounds) using a dash and rounds them to a specified number of decimal places.
#'
#' @param est The estimate.
#' @param low The lower bound of the interval.
#' @param hi The upper bound of the interval.
#' @param dec The number of decimal places.
#' @return A character string with the estimate and confidence intervals.
#' @export
#' @examples
#' # Basic usage
#' func_addci_dash(10.123, 9.456, 10.789, 2)
#'
#' # Using with dplyr to create a new formatted variable
#' library(dplyr)
#' df <- tibble(est = c(10.123, 20.456), low = c(9.456, 19.789), hi = c(10.789, 21.123))
#' df <- df |> mutate(ci_dash = func_addci_dash(est, low, hi, 2))
func_addci_dash <- function(est, low, hi, dec) {
  stringr::str_trim(
    paste0(stringr::str_trim(format(round(est, dec), nsmall=dec)), " (",
           stringr::str_trim(format(round(low, dec), nsmall=dec)), " - ",
           stringr::str_trim(format(round(hi,  dec), nsmall=dec)), ")")
  )
}

#' Add Confidence Intervals with "to"
#'
#' This function formats an estimate with its confidence interval
#' (lower and upper bounds) using "to" and rounds them to a specified number of decimal places.
#'
#' @param est The estimate.
#' @param low The lower bound of the interval.
#' @param hi The upper bound of the interval.
#' @param dec The number of decimal places.
#' @return A character string with the estimate and confidence intervals.
#' @export
#' @examples
#' # Basic usage
#' func_addci_to(10.123, 9.456, 10.789, 2)
#'
#' # Using with dplyr to create a new formatted variable
#' library(dplyr)
#' df <- tibble(est = c(10.123, 20.456), low = c(9.456, 19.789), hi = c(10.789, 21.123))
#' df <- df |> mutate(ci_to = func_addci_to(est, low, hi, 2))
func_addci_to <- function(est, low, hi, dec) {
  stringr::str_trim(
    paste0(stringr::str_trim(format(round(est, dec), nsmall=dec)), " (",
           stringr::str_trim(format(round(low, dec), nsmall=dec)), " to ",
           stringr::str_trim(format(round(hi,  dec), nsmall=dec)), ")")
  )
}

#' Format Count and Percentage
#'
#' This function formats a count and percentage, rounding the percentage
#' to a specified number of decimal places.
#'
#' @param count The count.
#' @param pct The percentage.
#' @param dec The number of decimal places for the percentage.
#' @return A character string with the formatted count and percentage.
#' @export
#' @examples
#' # Basic usage
#' func_count_pct(1500, 75.1234, 2)
#'
#' # Using with dplyr to create a new formatted variable
#' library(dplyr)
#' df <- tibble(count = c(1500, 2500), pct = c(75.1234, 85.5678))
#' df <- df |> mutate(count_pct = func_count_pct(count, pct, 2))
func_count_pct <- function(count, pct, dec) {
  stringr::str_trim(
    paste0(format(round(count, 0), nsmall=0, big.mark = ","), " (",
           stringr::str_trim(format(round(pct, dec), nsmall=dec)), "%)")
  )
}
