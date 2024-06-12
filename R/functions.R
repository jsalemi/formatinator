
#' Round Numbers with Commas
#'
#' This function rounds a number to a specified number of decimal places and
#' formats it with commas as thousand separators.
#'
#' @param num The number to be rounded.
#' @param dec The number of decimal places.
#' @return A character string of the formatted number.
#' @import dplyr
#' @import stringr
#' @import tibble
#' @export
#' @examples
#' # Basic usage
#' func_myround(12345.6789, 2)
#'
#' # Using with dplyr to create a new formatted variable
#' library(dplyr)
#' df <- tibble::tibble(value = c(12345.6789, 98765.4321))
#' df <- df |> dplyr::mutate(formatted_value = func_myround(value, 2))
func_myround <- function(num, dec) {
  stringr::str_trim(
    format(janitor::round_half_up(num, dec), nsmall=dec, big.mark = ",")
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
#' @import dplyr
#' @import stringr
#' @import tibble
#' @export
#' @examples
#' # Basic usage
#' func_addci(10.123, 9.456, 10.789, 2)
#'
#' # Using with dplyr to create a new formatted variable
#' library(dplyr)
#' df <- tibble::tibble(est = c(10.123, 20.456), low = c(9.456, 19.789), hi = c(10.789, 21.123))
#' df <- df |> dplyr::mutate(ci = func_addci(est, low, hi, 2))
func_addci <- function(est, low, hi, dec) {
  stringr::str_trim(
    paste0(
      stringr::str_trim(format(janitor::round_half_up(est, dec), nsmall=dec)), " (",
      stringr::str_trim(format(janitor::round_half_up(low, dec), nsmall=dec)), ", ",
      stringr::str_trim(format(janitor::round_half_up(hi,  dec), nsmall=dec)), ")"
    )
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
#' @import dplyr
#' @import stringr
#' @import tibble
#' @export
#' @examples
#' # Basic usage
#' func_addci_dash(10.123, 9.456, 10.789, 2)
#'
#' # Using with dplyr to create a new formatted variable
#' library(dplyr)
#' df <- tibble::tibble(est = c(10.123, 20.456), low = c(9.456, 19.789), hi = c(10.789, 21.123))
#' df <- df |> dplyr::mutate(ci_dash = func_addci_dash(est, low, hi, 2))
func_addci_dash <- function(est, low, hi, dec) {
  stringr::str_trim(
    paste0(
      stringr::str_trim(format(janitor::round_half_up(est, dec), nsmall=dec)), " (",
      stringr::str_trim(format(janitor::round_half_up(low, dec), nsmall=dec)), " - ",
      stringr::str_trim(format(janitor::round_half_up(hi,  dec), nsmall=dec)), ")"
    )
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
#' @import dplyr
#' @import stringr
#' @import tibble
#' @export
#' @examples
#' # Basic usage
#' func_addci_to(10.123, 9.456, 10.789, 2)
#'
#' # Using with dplyr to create a new formatted variable
#' library(dplyr)
#' df <- tibble::tibble(est = c(10.123, 20.456), low = c(9.456, 19.789), hi = c(10.789, 21.123))
#' df <- df |> dplyr::mutate(ci_to = func_addci_to(est, low, hi, 2))
func_addci_to <- function(est, low, hi, dec) {
  stringr::str_trim(
    paste0(
      stringr::str_trim(format(janitor::round_half_up(est, dec), nsmall=dec)), " (",
      stringr::str_trim(format(janitor::round_half_up(low, dec), nsmall=dec)), " to ",
      stringr::str_trim(format(janitor::round_half_up(hi,  dec), nsmall=dec)), ")"
    )
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
#' @import dplyr
#' @import stringr
#' @import tibble
#' @export
#' @examples
#' # Basic usage
#' func_count_pct(1500, 75.1234, 2)
#'
#' # Using with dplyr to create a new formatted variable
#' library(dplyr)
#' df <- tibble::tibble(count = c(1500, 2500), pct = c(75.1234, 85.5678))
#' df <- df |> dplyr::mutate(count_pct = func_count_pct(count, pct, 2))
func_count_pct <- function(count, pct, dec) {
  stringr::str_trim(
    paste0(format(janitor::round_half_up(count, 0), nsmall=0, big.mark = ","), " (",
           stringr::str_trim(format(janitor::round_half_up(pct, dec), nsmall=dec)), "%)")
  )
}

#' Basic Diagnosis Code Field Cleaner
#'
#' This function capitalizes the a diagnosis code variable, removes any decimals,
#' removes leading and trailing blanks, and will convert any "" to NA.
#'
#' @param x The character string (e.g., variable) containing the diagnosis code values.
#' @return A character string with the cleaned values of the diagnosis code.
#' @import dplyr
#' @import stringr
#' @import tibble
#' @export
#' @examples
#' # Basic usage
#' func_dx_clean_na(" q25.1  ")
#' func_dx_clean_na("q25.1a  ")
#' func_dx_clean_na("")
#'
#' # Using across multiple diagnosis code variables in a dataset
#' # Original data
#' df <- tibble::tibble(dx1=" q25.1  ", dx2="q25.1a  ", dx3="", dx4=NA)
#' df
#' # Applying the cleaning function
#' df <- df |>
#'   dplyr::mutate(
#'     across(.cols = num_range("dx", 1:4),
#'            .fns  = ~ func_dx_clean_na(.x) )
#'   )
#' # Printing the cleaned dataset
#' df
func_dx_clean_na <- function(x) {
  dplyr::na_if( stringr::str_trim( stringr::str_replace_all( stringr::str_to_upper(x), pattern = "\\.", replacement = ""), "both"), "")
}

#' Create Binary Indicators of Diagnostic Conditions Based on Diagnosis Code Variables
#'
#' This function will scan diagnosis code variables in a data frame for specific codes
#' the user specifies and will create an indicator variable indicating presence (1)
#' or absence (0) of at least one of the desired codes reflected in a code pattern.
#'
#' @param df The data frame which contains the diagnosis code variables.
#' @param codes A regular expression (pattern) reflecting the codes of interest.
#' @return A numeric value of 0 or 1.
#' @import dplyr
#' @import stringr
#' @import purrr
#' @import tibble
#' @export
#' @examples
#' # Original data
#' df <- tibble::tibble(
#'   id  = c(1, 2, 3),
#'   dx1 = c("X34.5", "X35.6", "X36.7"),
#'   dx2 = c("G33.1", "G34.3", "G35.4"),
#'   dx3 = c("F25.4", "F26.7", "F27.8"),
#'   dx4 = c(NA,      "H44.8", "H45.9"),
#'   dx5 = c(NA,      NA,      "I56.0")
#' )
#' df
#' # Create three new variables for each observation
#' # flag_any_g_code should be 1 if any diagnoses begin with "G"
#' # flag_g33_code should be 1 if any diagnoses begin with "G33"
#' # flag_code_ends_even should be 1 if any diagnoses end in an even number
#' df <- df |>
#'   dplyr::mutate(
#'     flag_any_g_code     = func_dx_scan_yn(df |> dplyr::select(dx1:dx5), "^G"),
#'     flag_g33_code       = func_dx_scan_yn(df |> dplyr::select(dx1:dx5), "^G33"),
#'     flag_code_ends_even = func_dx_scan_yn(df |> dplyr::select(dx1:dx5), "[02468]$")
#'   )
#' df
func_dx_scan_yn <- function(df, codes) {
  as.integer(( rowSums(purrr::modify(df, grepl, pattern = codes)) > 0))
}

#' Create Counts of Diagnostic Conditions Based on Diagnosis Code Variables
#'
#' This function will scan diagnosis code variables in a data frame for specific codes
#' the user specifies and will create a variable reflecting the number of variables
#' which contained desired codes reflected in a code pattern (a count).
#'
#' @param df The data frame which contains the diagnosis code variables.
#' @param codes A regular expression (pattern) reflecting the codes of interest.
#' @return A numeric value of 0 or 1.
#' @import dplyr
#' @import stringr
#' @import purrr
#' @import tibble
#' @export
#' @examples
#' # Original data
#' df <- tibble::tibble(
#'   id  = c(1, 2, 3),
#'   dx1 = c("X34.5", "X35.6", "X36.7"),
#'   dx2 = c("G33.1", "G34.3", "G35.4"),
#'   dx3 = c("F25.4", "F26.7", "F27.8"),
#'   dx4 = c(NA,      "H44.8", "H45.9"),
#'   dx5 = c(NA,      NA,      "I56.0")
#' )
#' df
#' # Create three new variables for each observation
#' # cnt_any_g_code should count the number of diagnoses that begin with "G"
#' # cnt_g33_code should count the number of diagnoses that begin with "G33"
#' # cnt_code_ends_even should count the number of diagnoses that end in an even number
#' df <- df |>
#'   dplyr::mutate(
#'     cnt_any_g_code     = func_dx_scan_count(df |> dplyr::select(dx1:dx5), "^G"),
#'     cnt_g33_code       = func_dx_scan_count(df |> dplyr::select(dx1:dx5), "^G33"),
#'     cnt_code_ends_even = func_dx_scan_count(df |> dplyr::select(dx1:dx5), "[02468]$")
#'   )
#' df
func_dx_scan_count <- function(df, codes) {
  rowSums(purrr::modify(df, grepl, pattern = codes))
}
