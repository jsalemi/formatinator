
# File: formatinator/R/functions.R

#' Round Numbers with Commas
#'
#' @param num The number to be rounded.
#' @param dec The number of decimal places.
#' @return A character string of the formatted number.
#' @export
func_myround <- function(num, dec) {
  stringr::str_trim(
    format(round(num, dec), nsmall=dec, big.mark = ",")
  )
}

#' Add Confidence Intervals
#'
#' @param est The estimate.
#' @param low The lower bound of the interval.
#' @param hi The upper bound of the interval.
#' @param dec The number of decimal places.
#' @return A character string with the estimate and confidence intervals.
#' @export
func_addci <- function(est, low, hi, dec) {
  stringr::str_trim(
    paste0(stringr::str_trim(format(round(est, dec), nsmall=dec)), " (",
           stringr::str_trim(format(round(low, dec), nsmall=dec)), ", ",
           stringr::str_trim(format(round(hi,  dec), nsmall=dec)), ")")
  )
}

#' Add Confidence Intervals with Dash
#'
#' @param est The estimate.
#' @param low The lower bound of the interval.
#' @param hi The upper bound of the interval.
#' @param dec The number of decimal places.
#' @return A character string with the estimate and confidence intervals.
#' @export
func_addci_dash <- function(est, low, hi, dec) {
  stringr::str_trim(
    paste0(stringr::str_trim(format(round(est, dec), nsmall=dec)), " (",
           stringr::str_trim(format(round(low, dec), nsmall=dec)), " - ",
           stringr::str_trim(format(round(hi,  dec), nsmall=dec)), ")")
  )
}

#' Add Confidence Intervals with "to"
#'
#' @param est The estimate.
#' @param low The lower bound of the interval.
#' @param hi The upper bound of the interval.
#' @param dec The number of decimal places.
#' @return A character string with the estimate and confidence intervals.
#' @export
func_addci_to <- function(est, low, hi, dec) {
  stringr::str_trim(
    paste0(stringr::str_trim(format(round(est, dec), nsmall=dec)), " (",
           stringr::str_trim(format(round(low, dec), nsmall=dec)), " to ",
           stringr::str_trim(format(round(hi,  dec), nsmall=dec)), ")")
  )
}

#' Format Count and Percentage
#'
#' @param count The count.
#' @param pct The percentage.
#' @param dec The number of decimal places for the percentage.
#' @return A character string with the formatted count and percentage.
#' @export
func_count_pct <- function(count, pct, dec) {
  stringr::str_trim(
    paste0(format(round(count, 0), nsmall=0, big.mark = ","), " (",
           stringr::str_trim(format(round(pct, dec), nsmall=dec)), "%)")
  )
}











